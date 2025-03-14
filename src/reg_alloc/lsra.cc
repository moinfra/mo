#include "lsra.h"
#include "../lra.h"
#include "../machine.h"

void LinearScanRegisterAllocator::initialize()
{
    MO_DEBUG("Collecting initial intervals for all virtual registers.");

    // 收集所有虚拟寄存器的活跃区间
    for (auto &[reg, lr] : lra_.get_all_live_ranges())
    {
        if (MachineFunction::is_virtual_reg(reg))
        {
            for (auto &range : lr->atomized_ranges(mf_))
            {
                auto range_ptr = range.get();
                ranges_.push_back(std::move(range));
                unhandled_.push(range_ptr);
            }
        }
    }

    auto pregs = this->collect_allocatable_regs();
    MO_DEBUG("Initial available physical registers: %s", mo_join(pregs, ", ").c_str());
    for (unsigned preg : pregs)
    {
        phys_reg_states_[preg] = {.available = true, .assigned_range = nullptr, .last_use_pos = 0};
    }
}

RegAllocResult LinearScanRegisterAllocator::allocate_registers()
{
    MO_DEBUG("Starting register allocation.");
    initialize();
    RegAllocResult result;

    while (!unhandled_.empty())
    {
        auto current_range = unhandled_.top();
        LiveInterval current_interval = current_range->atomized_interval();
        MO_ASSERT(current_interval.parent() == current_range, "Expected atomized interval");
        MO_DEBUG("Processing interval: [%u, %u) for vreg %u", current_interval.start(), current_interval.end(), current_interval.parent()->vreg());
        unhandled_.pop();

        // 1. 释放过期区间
        expire_old_intervals(current_interval.start());

        // 2. 尝试分配物理寄存器
        if (current_range->is_allocated())
        {
            MO_DEBUG("Interval for physical register, skipping.");
            active_.push(current_range);
            continue;
        }
        else if (allocate_register_for(current_range))
        {
            MO_DEBUG("Successfully allocated register for vreg %u", current_range->vreg());
            active_.push(current_range);
            continue;
        }
        else
        {
            MO_DEBUG("Failed to allocate register for vreg %u, attempting to spill.", current_interval.parent()->vreg());
            // 3. 需要溢出处理
            LiveRange *spill_candidate = choose_register_to_spill(current_interval.start());
            if (spill_candidate)
            {
                MO_DEBUG("Spilling candidate found: vreg %u", spill_candidate->vreg());
                spill_live_range(spill_candidate);
                if (allocate_register_for(current_interval.parent()))
                {
                    MO_DEBUG("Successfully allocated register for vreg %u after spilling.", current_interval.parent()->vreg());
                    active_.push(current_interval.parent());
                }
                else
                {
                    MO_DEBUG("Failed to allocate register even after spilling.");
                    result.successful = false;
                    result.error_message = "Failed to allocate even after spilling";
                    return result;
                }
            }
            else
            {
                MO_DEBUG("No spill candidate found.");
                result.successful = false;
                result.error_message = "No spill candidate found";
                return result;
            }
        }
    }

    MO_DEBUG("Register allocation completed successfully.");
    result.successful = true;
    return result;
}

void LinearScanRegisterAllocator::expire_old_intervals(unsigned current_pos)
{
    MO_DEBUG("Expiring old intervals at position %u", current_pos);
    while (!active_.empty())
    {
        LiveRange *lr = active_.top();
        MO_DEBUG("Checking interval for register: %d", lr->vreg());
        bool ended = lr->atomized_interval().end() < current_pos;
        if (ended)
        {
            // 释放物理寄存器
            MO_DEBUG("Interval for register %d has ended, freeing it.", lr->vreg());
            MO_ASSERT(lr->is_allocated(), "Expected physical register");
            if (lr->is_allocated())
            {
                auto &state = phys_reg_states_[lr->preg()];
                state.available = true;
                state.assigned_range = nullptr;
                state.last_use_pos = 0;
            }
            active_.pop();
        }
        else
        {
            break;
        }
    }
}

LiveRange *LinearScanRegisterAllocator::choose_register_to_spill(unsigned current_pos)
{
    MO_DEBUG("Choosing register to spill at position %u", current_pos);

    MO_ASSERT(!active_.empty(), "No active live ranges");
    return active_.top();
}

bool LinearScanRegisterAllocator::allocate_register_for(LiveRange *lr)
{
    auto vreg = lr->vreg();
    if (auto preg_it = vreg_to_preg_map_.find(vreg); preg_it != vreg_to_preg_map_.end())
    {
        MO_DEBUG("Register for vreg %u already assigned to preg %u", vreg, preg_it->second);
        lr->assign(preg_it->second);
        return true;
    }

    MO_ASSERT(!lr->is_allocated(), "Expected virtual register");
    MO_DEBUG("Attempting to allocate register for vreg %u", vreg);
    std::vector<unsigned> candidates = get_allocatable_regs(vreg);

    // 寻找最佳物理寄存器
    for (unsigned preg : candidates)
    {
        MO_DEBUG("Checking physical register: %u", preg);
        PhysRegState &state = phys_reg_states_[preg];
        if (state.available && !has_conflict(lr, preg))
        {
            MO_DEBUG("  Allocating physical register: %u for vreg %u", preg, vreg);
            lr->assign(preg);
            assign_physical_reg(vreg, preg);
            state.available = false;
            state.assigned_range = lr;
            state.last_use_pos = lr->atomized_interval().end();

            return true;
        }
        if (!state.available)
        {
            MO_DEBUG("  Not available");
        }
        if (has_conflict(lr, preg))
        {
            MO_DEBUG("  Conflict with current live range");
        }
    }
    MO_DEBUG("Failed to allocate register for vreg %u", vreg);
    return false;
}

bool LinearScanRegisterAllocator::has_conflict(LiveRange *lr, unsigned preg) const
{
    MO_DEBUG("Checking for conflicts between vreg %u and preg %u", lr->vreg(), preg);
    // 检查与物理寄存器的活跃区间冲突
    if (LiveRange *phys_lr = lra_.get_physical_live_range(preg))
    {
        bool conflicts = phys_lr->interferes_with(*lr);
        MO_DEBUG("Conflict check result: %s", conflicts ? "true" : "false");
        return conflicts;
    }
    MO_DEBUG("No physical live range found for preg %u, assuming no conflict", preg);
    return false;
}

void LinearScanRegisterAllocator::spill_live_range(LiveRange *lr)
{
    MO_ASSERT(lr->is_allocated(), "Expected physical register");
    MO_DEBUG("Spilling range of preg %u", lr->vreg());
    auto slot = allocate_spill_slot(lr->vreg());
    auto vreg_rc = mf_.get_vreg_info(lr->vreg()).register_class_id_;
    auto tmp_regs = tri_.get_temp_regs(mf_.call_convention(), vreg_rc);
    MO_ASSERT(tmp_regs.size() > 0, "No temporary registers available for spilling");
    assign_temp_physical_reg(lr->vreg(), tmp_regs[0]);
    auto preg = lr->preg();

    MO_DEBUG("Freeing physical register: %u", preg);

    auto &state = phys_reg_states_[preg];
    MO_ASSERT(state.assigned_range == lr, "Expected assigned range");
    state.available = true;
    state.assigned_range = nullptr;
    state.last_use_pos = 0;

    lr->mark_spilled(slot);
}

std::vector<unsigned> LinearScanRegisterAllocator::get_allocatable_regs(unsigned vreg) const
{
    MO_DEBUG("Getting allocatable registers for vreg %u", vreg);
    // 获取虚拟寄存器的寄存器类别
    const auto &vreg_info = mf_.get_vreg_info(vreg);
    unsigned rc_id = vreg_info.register_class_id_;

    std::vector<unsigned> pregs = tri_.get_allocation_order(rc_id);
    // MO_DEBUG("Allocation order for vreg %u: [%s]", vreg, mo_join(pregs, ", ").c_str());
    // 按使用热度排序
    std::sort(pregs.begin(), pregs.end(), [&](unsigned a, unsigned b)
              { 
                MO_ASSERT(phys_reg_states_.count(a) != 0, "Invalid physical register: %u", a);
                MO_ASSERT(phys_reg_states_.count(b) != 0, "Invalid physical register: %u", b);
                return phys_reg_states_.at(a).last_use_pos > phys_reg_states_.at(b).last_use_pos; });
    MO_DEBUG("Allocatable registers for vreg %u: %zu registers found", vreg, pregs.size());
    return pregs;
}

std::vector<unsigned> LinearScanRegisterAllocator::collect_allocatable_regs() const
{
    MO_DEBUG("Getting initial allocatable registers");
    std::vector<unsigned> pregs;
    for (RegisterClass *rc : tri_.get_reg_classes())
    {
        for (unsigned preg : tri_.get_allocatable_regs(rc->id))
        {
            MO_ASSERT(!tri_.is_reserved_reg(preg), "Invalid physical register: %u, reserved", preg);
            pregs.push_back(preg);
        }
    }
    MO_DEBUG("Initial allocatable registers: %zu registers found", pregs.size());
    return pregs;
}
