#include "machine.h"

#include <iomanip>

//===----------------------------------------------------------------------===//
// Register Pressure Tracking Implementation
//===----------------------------------------------------------------------===//

void PressureTracker::add_interval(const LiveRange &lr)
{
    unsigned weight = tri_.get_reg_weight(lr.reg());
    for (auto interval : lr.intervals())
    {
        auto start = interval.start(), end = interval.end();

        for (unsigned pos = start; pos <= end; ++pos)
        {
            pressure_at_pos_[pos] += weight;
            // 任何添加操作都可能改变最大值，所以标记为 dirty
            max_pressure_dirty_ = true;
        }
    }
}

void PressureTracker::remove_interval(const LiveRange &lr)
{
    unsigned weight = tri_.get_reg_weight(lr.reg());
    for (auto interval : lr.intervals())
    {
        auto start = interval.start(), end = interval.end();

        for (unsigned pos = start; pos <= end; ++pos)
        {
            pressure_at_pos_[pos] -= weight;
            // 如果被减少的位置是原最大值，标记需要重新计算
            if (pressure_at_pos_[pos] + weight == cached_max_pressure_)
            {
                max_pressure_dirty_ = true;
            }
        }
    }
}

unsigned PressureTracker::get_max_pressure() const
{
    if (max_pressure_dirty_)
    {
        cached_max_pressure_ = 0;
        for (const auto &[pos, pressure] : pressure_at_pos_)
        {
            cached_max_pressure_ = std::max(cached_max_pressure_, pressure);
        }
        max_pressure_dirty_ = false;
    }
    return cached_max_pressure_;
}

void PressureTracker::dump_pressure_curve() const
{
    for (auto [pos, pressure] : pressure_at_pos_)
    {
        std::cout << "Position " << pos << ": " << pressure << "\n";
    }
}

//===----------------------------------------------------------------------===//
// MOperand Implementation
//===----------------------------------------------------------------------===//

MOperand::MEMrix MOperand::get_mem_rix() const
{
    assert(is_mem_rix());
    return std::get<MEMrix>(storage_);
}

MOperand MOperand::create_mem_rix(unsigned base_reg, unsigned index_reg,
                                  int scale, int offset)
{
    MOperand op;
    op.storage_ = MEMrix{base_reg, index_reg, scale, offset};
    return op;
}

MOperand MOperand::create_reg(unsigned reg, bool is_def)
{
    MOperand op;
    op.storage_ = reg;
    op.is_def_ = is_def;
    return op;
}

MOperand MOperand::create_imm(int64_t val)
{
    MOperand op;
    op.storage_ = val;
    return op;
}

MOperand MOperand::create_fp_imm(double val)
{
    MOperand op;
    op.storage_ = val;
    return op;
}

MOperand MOperand::create_frame_index(int idx)
{
    MOperand op;
    op.storage_ = idx;
    return op;
}

MOperand MOperand::create_global(GlobalVariable *gv)
{
    MOperand op;
    op.storage_ = gv;
    return op;
}

MOperand MOperand::create_external_sym(const char *symbol)
{
    MOperand op;
    op.storage_ = symbol;
    return op;
}

MOperand MOperand::create_mem_ri(unsigned base_reg, int offset)
{
    MOperand op;
    op.storage_ = MEMri{base_reg, offset};
    return op;
}

MOperand MOperand::create_mem_rr(unsigned base_reg, unsigned index_reg)
{
    MOperand op;
    op.storage_ = MEMrr{base_reg, index_reg};
    return op;
}

unsigned MOperand::get_reg() const
{
    assert(is_reg());
    return std::get<unsigned>(storage_);
}

int64_t MOperand::get_imm() const
{
    assert(is_imm());
    return std::get<int64_t>(storage_);
}

double MOperand::get_fp_imm() const
{
    assert(is_fp_imm());
    return std::get<double>(storage_);
}

int MOperand::get_frame_index() const
{
    assert(is_frame_index());
    return std::get<int>(storage_);
}

GlobalVariable *MOperand::get_global() const
{
    assert(is_global());
    return std::get<GlobalVariable *>(storage_);
}

const char *MOperand::get_external_sym() const
{
    assert(is_external_sym());
    return std::get<const char *>(storage_);
}

MOperand::MEMri MOperand::get_mem_ri() const
{
    assert(is_mem_ri());
    return std::get<MEMri>(storage_);
}

MOperand::MEMrr MOperand::get_mem_rr() const
{
    assert(is_mem_rr());
    return std::get<MEMrr>(storage_);
}

unsigned MOperand::get_base_reg() const
{
    if (is_mem_ri())
        return std::get<MEMri>(storage_).base_reg;
    if (is_mem_rr())
        return std::get<MEMrr>(storage_).base_reg;
    if (is_mem_rix())
        return std::get<MEMrix>(storage_).base_reg;
    assert(false && "Not a memory operand!");
    return 0;
}

bool MOperand::is_reg() const noexcept
{
    return std::holds_alternative<unsigned>(storage_);
}
bool MOperand::is_imm() const noexcept
{
    return std::holds_alternative<int64_t>(storage_);
}
bool MOperand::is_fp_imm() const noexcept
{
    return std::holds_alternative<double>(storage_);
}
bool MOperand::is_frame_index() const noexcept
{
    return std::holds_alternative<int>(storage_);
}
bool MOperand::is_global() const noexcept
{
    return std::holds_alternative<GlobalVariable *>(storage_);
}
bool MOperand::is_external_sym() const noexcept
{
    return std::holds_alternative<const char *>(storage_);
}
bool MOperand::is_mem_ri() const noexcept
{
    return std::holds_alternative<MEMri>(storage_);
}
bool MOperand::is_mem_rr() const noexcept
{
    return std::holds_alternative<MEMrr>(storage_);
}
bool MOperand::is_mem_rix() const noexcept
{
    return std::holds_alternative<MEMrix>(storage_);
}
bool MOperand::is_valid() const noexcept
{
    return !std::holds_alternative<std::monostate>(storage_);
}

std::string MOperand::to_string() const
{
    if (!is_valid())
        return "INVALID";

    std::ostringstream oss;
    if (is_reg())
    {
        oss << "R" << get_reg();
        if (is_def())
            oss << "<def>";
        if (is_kill())
            oss << "<kill>";
        if (is_dead())
            oss << "<dead>";
    }
    else if (is_imm())
    {
        oss << "#" << get_imm();
    }
    else if (is_fp_imm())
    {
        oss << "#" << get_fp_imm();
    }
    else if (is_mem_ri())
    {
        auto mem = get_mem_ri();
        oss << "[R" << mem.base_reg << " + " << mem.offset << "]";
    }
    else if (is_mem_rr())
    {
        auto mem = get_mem_rr();
        oss << "[R" << mem.base_reg << " + R" << mem.index_reg << "]";
    }
    else if (is_mem_rix())
    {
        auto mem = get_mem_rix();
        oss << "[R" << mem.base_reg << " + R" << mem.index_reg << "*" << mem.scale
            << " + " << mem.offset << "]";
    }
    else if (is_frame_index())
    {
        oss << "fi#" << get_frame_index();
    }
    else if (is_global())
    {
        oss << "global(" << get_global()->name() << ")";
    }
    else if (is_external_sym())
    {
        oss << "sym(" << get_external_sym() << ")";
    }
    return oss.str();
}

//===----------------------------------------------------------------------===//
// MachineInst Implementation
//===----------------------------------------------------------------------===//

std::string MachineInst::to_string(const TargetInstInfo *tii) const
{
    std::ostringstream oss;
    // TODO: Add position information
    // oss << "[" << std::setw(4) << mf.get_global_instr_pos(this) << "] ";

    if (tii)
    {
        // If target info is available, query opcode name
        oss << tii->get_opcode_name(opcode_);
    }
    else
    {
        // Otherwise, display raw value
        oss << "Inst#0x" << std::hex << opcode_;
    }

    for (const auto &op : operands())
    {
        oss << " " << op.to_string();
    }

    // Add flag information
    if (flags_.any())
    {
        oss << " [";
        if (has_flag(MIFlag::FrameSetup))
            oss << "FRAME_SETUP|";
        if (has_flag(MIFlag::Call))
            oss << "CALL|";
        if (has_flag(MIFlag::Terminator))
            oss << "TERMINATOR|";
        if (has_flag(MIFlag::MayLoad))
            oss << "MAY_LOAD|";
        if (has_flag(MIFlag::MayStore))
            oss << "MAY_STORE|";
        if (has_flag(MIFlag::HasDelaySlot))
            oss << "HAS_DELAY_SLOT|";
        if (has_flag(MIFlag::MayRaiseException))
            oss << "MAY_RAISE_EXCEPTION|";
        if (has_flag(MIFlag::HasSideEffects))
            oss << "HAS_SIDE_EFFECTS|";
        if (has_flag(MIFlag::IsVolatile))
            oss << "IS_VOLATILE|";
        if (has_flag(MIFlag::NotDuplicable))
            oss << "NOT_DUPLICABLE|";
        if (has_flag(MIFlag::IsCompare))
            oss << "IS_COMPARE|";
        oss.seekp(-1, oss.cur); // Remove trailing pipe

        // ... Other flags
        oss << "]";
    }
    return oss.str();
}

MachineInst::MachineInst(unsigned opcode, const std::vector<MOperand> &ops)
    : opcode_(opcode), ops_(ops) {}

bool MachineInst::verify(VerificationLevel level, const TargetInstInfo *tii,
                         std::string *err_msg) const
{
    // Level 1: Quick basic check
    for (const auto &op : operands())
    {
        if (!op.is_valid())
        {
            if (err_msg)
                *err_msg = "Invalid operand detected";
            return false;
        }
    }

    // Level 2: Cross-platform generic rules
    if (has_flag(MIFlag::MayLoad) && has_flag(MIFlag::MayStore))
    {
        if (err_msg)
            *err_msg = "Instruction cannot both load and store";
        return false;
    }

    // Level 3: Target-related validation
    if (level >= TARGET_SPECIFIC && tii)
    {
        std::string ti_err;
        if (!tii->verify_instruction(*this, ti_err))
        {
            if (err_msg)
                *err_msg = "[Target] " + ti_err;
            return false;
        }
    }
    return true;
}

// Flag operations
void MachineInst::set_flag(MIFlag flag, bool val)
{
    flags_.set(static_cast<size_t>(flag), val);
}
bool MachineInst::has_flag(MIFlag flag) const
{
    return flags_.test(static_cast<size_t>(flag));
}

// Operand management
void MachineInst::add_operand(const MOperand &op)
{
    ops_.push_back(op);
}
void MachineInst::insert_operand(unsigned idx, const MOperand &op)
{
    ops_.insert(ops_.begin() + idx, op);
}
void MachineInst::remove_operand(unsigned idx)
{
    ops_.erase(ops_.begin() + idx);
}

//===----------------------------------------------------------------------===//
// MachineBasicBlock Implementation
//===----------------------------------------------------------------------===//

MachineBasicBlock::MachineBasicBlock(MachineFunction &mf, unsigned number)
    : mf_(mf), number_(number) {}

MachineBasicBlock::iterator MachineBasicBlock::insert(iterator pos, std::unique_ptr<MachineInst> inst)
{
    return insts_.insert(pos, std::move(inst));
}

void MachineBasicBlock::add_instr(std::unique_ptr<MachineInst> mi)
{
    mf_.mark_global_positions_dirty();
    mf_.mark_live_ranges_dirty();
    insts_.push_back(std::move(mi));
}

void MachineBasicBlock::add_successor(MachineBasicBlock *succ)
{
    successors_.push_back(succ);
    succ->predecessors_.push_back(this);
}

void MachineBasicBlock::remove_successor(MachineBasicBlock *succ)
{
    auto it = std::find(successors_.begin(), successors_.end(), succ);
    if (it != successors_.end())
    {
        successors_.erase(it);
        succ->predecessors_.erase(
            std::find(succ->predecessors_.begin(), succ->predecessors_.end(), this));
    }
}

std::string MachineBasicBlock::get_label() const
{
    if (!label_.empty())
        return label_;
    return "BB" + std::to_string(number_);
}

std::string MachineBasicBlock::to_string() const
{
    std::ostringstream oss;
    oss << get_label() << ":\n";
    for (const auto &inst : insts_)
    {
        oss << "  " << inst->to_string() << "\n";
    }
    return oss.str();
}

// Cached live range access
const LiveRange &MachineBasicBlock::get_live_range(unsigned vreg) const
{
    return mf_.get_vreg_liverange(vreg);
}

unsigned MachineBasicBlock::get_instr_global_pos(iterator it) const
{
    return mf_.get_global_instr_pos(it->get());
}
//===----------------------------------------------------------------------===//
// FrameObjectInfo Implementation
//===----------------------------------------------------------------------===//

bool FrameObjectInfo::validate(std::string *err) const
{
    if (size < -1)
    {
        if (err)
            *err = "Invalid size value";
        return false;
    }
    if ((flags & IsVariableSize) && size != -1)
    {
        if (err)
            *err = "Variable-size object must have size=-1";
        return false;
    }
    return true;
}

//===----------------------------------------------------------------------===//
// MachineFunction Implementation
//===----------------------------------------------------------------------===//

MachineBasicBlock *MachineFunction::create_block()
{
    auto block =
        std::make_unique<MachineBasicBlock>(*this, next_bb_number_++);
    auto *ptr = block.get();
    blocks_.push_back(std::move(block));
    return ptr;
}

int MachineFunction::create_frame_object(FrameObjectInfo info)
{
    if (!info.validate())
        return -1;

    const int idx = next_frame_idx_++;
    frame_objects_.emplace(idx, std::move(info));
    is_frame_layout_dirty_ = true;
    return idx;
}

const FrameObjectInfo *MachineFunction::get_frame_object(int idx) const
{
    auto it = frame_objects_.find(idx);
    return it != frame_objects_.end() ? &it->second : nullptr;
}

const std::vector<int> &MachineFunction::get_frame_layout() const
{
    if (!is_frame_layout_dirty_)
        return layout_order_;

    // Reorder layout based on object attributes
    layout_order_.clear();
    for (const auto &[idx, obj] : frame_objects_)
    {
        layout_order_.push_back(idx);
    }

    // Sorting strategy: prioritize fixed-size and high-alignment objects
    std::sort(layout_order_.begin(), layout_order_.end(),
              [this](int a, int b)
              {
                  const auto &obj_a = frame_objects_.at(a);
                  const auto &obj_b = frame_objects_.at(b);
                  if (obj_a.flags != obj_b.flags)
                      return (obj_a.flags & FrameObjectInfo::IsFixedSize) >
                             (obj_b.flags & FrameObjectInfo::IsFixedSize);
                  return obj_a.alignment > obj_b.alignment;
              });

    is_frame_layout_dirty_ = false;
    return layout_order_;
}

int MachineFunction::calculate_frame_size() const
{
    int total = 0;
    int max_align = 1;

    for (int idx : get_frame_layout())
    {
        const auto &obj = frame_objects_.at(idx);
        if (obj.flags & FrameObjectInfo::IsVariableSize)
            continue; // Handle dynamic-size objects separately

        const int align = obj.alignment;
        max_align = std::max(max_align, align);

        // Add padding to satisfy alignment
        total = (total + align - 1) & -align;
        total += std::abs(obj.size);
    }

    // Final alignment of the entire stack frame
    total = (total + max_align - 1) & -max_align;
    return total;
}

unsigned MachineFunction::create_vreg(unsigned rc_id, unsigned size,
                                      bool is_fp, Value *orig)
{
    vreg_infos_[next_vreg_] = {rc_id, size, is_fp, orig};
    return next_vreg_++;
}

const VRegInfo &MachineFunction::get_vreg_info(unsigned reg) const
{
    auto it = vreg_infos_.find(reg);
    assert(it != vreg_infos_.end() && "Invalid virtual register!");
    return it->second;
}

// Get live range
const LiveRange &MachineFunction::get_vreg_liverange(unsigned vreg) const
{
    return lra_->get_live_range(vreg);
}

// Enhanced frame index management
bool MachineFunction::has_frame_index(Value *v) const
{
    for (const auto &[idx, obj] : frame_objects_)
    {
        if (obj.associated_value == v)
        {
            return true;
        }
    }
    return false;
}

// Frame index management
int MachineFunction::create_frame_object(Value *v)
{
    FrameObjectInfo info;
    info.size = 4;                             // Default size, adjust as needed
    info.alignment = 4;                        // Default alignment, adjust as needed
    info.flags = FrameObjectInfo::IsFixedSize; // Default flags
    info.associated_value = v;

    return create_frame_object(info);
}
int MachineFunction::get_frame_index(Value *v) const
{
    for (const auto &[idx, obj] : frame_objects_)
    {
        if (obj.associated_value == v)
        {
            return idx;
        }
    }
    assert(false && "Frame object not found!");
    return -1;
}

std::string MachineFunction::to_string() const
{
    std::ostringstream oss;
    oss << "Function: " << ir_func_->name() << "\n";
    oss << "Stack objects:\n";
    for (const auto &[idx, obj] : frame_objects_)
    {
        oss << "  fi#" << idx << " for " << obj.associated_value->name() << "\n";
    }

    oss << "Virtual registers:\n";
    for (const auto &[vreg, info] : vreg_infos_)
    {
        oss << "  vreg" << vreg
            // << " [" << info.lr.start << "-" << info.lr.end
            << ")\n";
    }

    for (const auto &bb : blocks_)
    {
        oss << bb->to_string() << "\n";
    }
    return oss.str();
}

//===----------------------------------------------------------------------===//
// MachineModule Implementation
//===----------------------------------------------------------------------===//

MachineFunction *MachineModule::create_machine_function(Function *f)
{
    functions_.push_back(std::make_unique<MachineFunction>(f));
    return functions_.back().get();
}

void MachineModule::set_target_info(const TargetRegisterInfo *tri,
                                    const TargetInstInfo *tii)
{
    tri_ = tri;
    tii_ = tii;
}

const std::vector<std::unique_ptr<MachineBasicBlock>> &MachineFunction::get_basic_blocks() const
{
    return blocks_;
}
