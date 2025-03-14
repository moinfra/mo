#include "reg_alloc.h"
#include "machine.h"
#include "lra.h"
//===----------------------------------------------------------------------===//
// RegisterAllocator Implementation
//===----------------------------------------------------------------------===//

std::string RegAllocResult::to_string() const
{
    std::ostringstream oss;
    oss << "num_spills: " << num_spills << ", "
        << "num_copies: " << num_copies << ", "
        << "max_stack_size: " << max_stack_size << ", "
        << "successful: " << successful << ", "
        << "error_message: " << error_message;
    return oss.str();
}

RegisterAllocator::RegisterAllocator(MachineFunction &mf)
    : mf_(mf),
      lra_(*mf.live_range_analyzer()),
      tri_(*mf.parent()->target_reg_info()),
      tii_(*mf.parent()->target_inst_info()) {}

void RegisterAllocator::initialize_allocation()
{
    // 清除之前的分配状态
    vreg_to_preg_map_.clear();
    vreg_to_spill_slot_.clear();
    num_spills_ = 0;
    num_copies_ = 0;
}

void RegisterAllocator::apply()
{
    // test validity
    for (auto &mbb : mf_.basic_blocks())
    {
        for (auto inst_it = mbb->begin(); inst_it != mbb->end();)
        {
            auto inst = (*inst_it).get();
            MO_ASSERT(inst != nullptr, "nullptr machine instruction"); // 没有发生问题
            inst_it++;
        }
    }

    // handle replacements
    int bb_counter = -1;

    int load_counter = 0;
    int store_counter = 0;

    for (auto &mbb : mf_.basic_blocks())
    {
        bb_counter++;
        int i_counter = 0;
        for (auto it = mbb->begin(); it != mbb->end();)
        {
            MO_DEBUG("debug:bb %d i %d", bb_counter, i_counter++);
            auto inst = (*it).get();
            MO_ASSERT(inst != nullptr, "nullptr machine instruction");

            // 阶段1：收集需要插入的load/store操作 tuple: (verg, tmpreg, slot)
            std::vector<std::tuple<unsigned, unsigned, int>> loads, stores;
            for (auto &op : inst->operands())
            {
                if (!op.is_reg())
                {
                    continue;
                }

                auto vreg = op.reg();

                // 替换物理寄存器
                if (auto it = vreg_to_preg_map_.find(vreg); it != vreg_to_preg_map_.end())
                {
                    inst->replace_reg(vreg, it->second);
                }
                if (auto it = vreg_to_tmp_preg_map_.find(vreg); it != vreg_to_tmp_preg_map_.end())
                {
                    inst->replace_reg(vreg, it->second);
                }
                // 处理需要spill的情况
                auto spill_it = vreg_to_spill_slot_.find(vreg);
                if (spill_it == vreg_to_spill_slot_.end())
                {
                    continue;
                }

                unsigned tmp_reg = vreg_to_tmp_preg_map_[vreg];
                MO_DEBUG("spill %u to slot %d using tmp reg %u", vreg, spill_it->second, tmp_reg);
                int spill_slot = spill_it->second;

                if (op.is_def())
                {
                    stores.emplace_back(vreg, tmp_reg, spill_slot);
                }
                else
                {
                    loads.emplace_back(vreg, tmp_reg, spill_slot);
                }
            }

            // 阶段2：插入load指令（在原指令前）
            if (!loads.empty())
            {
                for (auto &[vreg, tmp_reg, slot] : loads)
                {
                    it = tii_.insert_load_from_stack(*mbb, it, tmp_reg, slot, 0);
                    ++it; // 关键点：移动到新插入的load之后
                    ++load_counter;
                }
                // 此时inst_it仍指向原指令，insert_pos指向最后一个load之后
            }

            // 阶段3：插入store指令（在原指令后）
            if (!stores.empty())
            {
                ++it; // 初始插入位置是原指令的下一条
                for (auto &[vreg, tmp_reg, slot] : stores)
                {
                    it = tii_.insert_store_to_stack(*mbb, it, tmp_reg, slot, 0);
                    ++it; // 关键点：移动到新插入的store之后
                    ++store_counter;
                }
            }

            if (loads.empty() && stores.empty())
            {
                ++it; // 无需插入指令，直接移动到下一条
            }
        }
    }

    MO_DEBUG("load_counter %d, store_counter %d", load_counter, store_counter);
}

std::optional<unsigned> RegisterAllocator::get_assigned_reg(unsigned vreg) const
{
    auto it = vreg_to_preg_map_.find(vreg);
    if (it != vreg_to_preg_map_.end())
    {
        return it->second;
    }
    return std::nullopt;
}

bool RegisterAllocator::assign_physical_reg(unsigned vreg, unsigned preg)
{
    // 检查物理寄存器是否可分配
    if (!tri_.can_allocate_reg(preg))
    {
        MO_ASSERT(false, "Not a allocatable physical register: %u", preg);
        return false;
    }

    // 不允许对vreg重新分配
    if (vreg_to_preg_map_.find(vreg) != vreg_to_preg_map_.end())
    {
        MO_ASSERT(false, "vreg %u already assigned to physical register %u", vreg, vreg_to_preg_map_[vreg]);
        return false;
    }

    vreg_to_preg_map_[vreg] = preg;
    return true;
}

bool RegisterAllocator::assign_temp_physical_reg(unsigned vreg, unsigned preg)
{
    // 检查物理寄存器是否可分配
    if (!tri_.is_temp_reg(mf_.call_convention(), preg))
    {
        MO_ASSERT(false, "Not a temporary register: %u", preg);
        return false;
    }

    // remove old allocation
    if (vreg_to_preg_map_.count(vreg))
    {
        vreg_to_preg_map_.erase(vreg);
    }

    // 不允许对vreg重新分配
    if (vreg_to_tmp_preg_map_.find(vreg) != vreg_to_tmp_preg_map_.end())
    {
        MO_ASSERT(false, "vreg %u already assigned to temporary register %u", vreg, vreg_to_tmp_preg_map_[vreg]);
        return false;
    }

    vreg_to_tmp_preg_map_[vreg] = preg;
    return true;
}

int RegisterAllocator::allocate_spill_slot(unsigned vreg)
{

    // 为同一个vreg不要创建多个溢出槽
    if (vreg_to_spill_slot_.find(vreg) != vreg_to_spill_slot_.end())
    {
        return vreg_to_spill_slot_[vreg];
    }

    if (vreg_to_preg_map_.find(vreg) != vreg_to_preg_map_.end()) // 如果 vreg 已经分配，则取消分配
    {
        vreg_to_preg_map_.erase(vreg);
    }

    if (vreg_to_tmp_preg_map_.find(vreg) != vreg_to_tmp_preg_map_.end()) // 如果 vreg 已经分配，则取消分配
    {
        vreg_to_tmp_preg_map_.erase(vreg);
    }

    const VRegInfo &vreg_info = mf_.get_vreg_info(vreg);

    // 为溢出创建栈对象
    FrameObjectMetadata spill_obj;
    spill_obj.size = vreg_info.size_;
    spill_obj.alignment = vreg_info.size_; // 按大小对齐
    spill_obj.flags = FrameObjectMetadata::IsSpillSlot;
    spill_obj.spill_rc_id = vreg_info.register_class_id_;

    int slot = mf_.frame()->create_frame_object(spill_obj);
    vreg_to_spill_slot_[vreg] = slot;
    num_spills_++;

    return slot;
}

float RegisterAllocator::calculate_spill_cost(unsigned vreg, const LiveRange &live_range)
{
    const VRegInfo &vreg_info = mf_.get_vreg_info(vreg);
    unsigned reg_class_weight = tri_.get_reg_class_weight(vreg_info.register_class_id_);

    // 计算使用计数
    unsigned use_count = 0;
    unsigned def_count = 0;
    unsigned loop_depth = 1; // 默认循环深度

    // 遍历指令以查找此vreg的使用
    for (auto &mbb : mf_.basic_blocks())
    {
        for (auto &inst : mbb->instructions())
        {
            unsigned pos = mf_.get_global_instr_pos(inst.get());

            // 如果寄存器在此位置不活跃，则跳过
            if (!live_range.live_at(pos))
            {
                continue;
            }

            // 计算使用和定义
            for (const auto &op : inst->operands())
            {
                if (op.is_reg() && op.reg() == vreg)
                {
                    if (op.is_def())
                    {
                        def_count++;
                    }
                    else
                    {
                        use_count++;
                    }
                }
            }
        }
    }

    // 计算溢出代价:
    // - 更多使用/定义意味着溢出代价更高
    // - 寄存器类权重因素
    float spill_cost = (use_count + def_count * 2.0f) * reg_class_weight * loop_depth;
    return spill_cost;
}
