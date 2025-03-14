#include "machine.h"

#include <iomanip>

//===----------------------------------------------------------------------===//
// Register Pressure Tracking Implementation
//===----------------------------------------------------------------------===//

void PressureTracker::add_interval(const LiveRange &live_range)
{
    unsigned weight = tri_.get_reg_weight(live_range.vreg());
    for (const auto &interval : live_range.intervals())
    {
        auto start = interval.start();
        auto end = interval.end();

        for (unsigned pos = start; pos <= end; ++pos)
        {
            pressure_at_pos_[pos] += weight;
            // Any addition might change the maximum, so mark as dirty.
            max_pressure_dirty_ = true;
        }
    }
}

void PressureTracker::remove_interval(const LiveRange &live_range)
{
    unsigned weight = tri_.get_reg_weight(live_range.vreg());
    for (const auto &interval : live_range.intervals())
    {
        auto start = interval.start();
        auto end = interval.end();

        for (unsigned pos = start; pos <= end; ++pos)
        {
            pressure_at_pos_[pos] -= weight;
            // If the reduced position was the original maximum, mark for
            // recalculation.
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
    for (const auto &[pos, pressure] : pressure_at_pos_)
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
    op.type_ = MOperandType::MEMrix;
    return op;
}

MOperand MOperand::create_reg(unsigned reg, bool is_def)
{
    MOperand op;
    op.storage_ = reg;
    op.is_def_ = is_def;
    op.type_ = MOperandType::Register;
    return op;
}

MOperand MOperand::create_imm(int64_t val)
{
    MOperand op;
    op.storage_ = val;
    op.type_ = MOperandType::Immediate;
    return op;
}

MOperand MOperand::create_fp_imm(double val)
{
    MOperand op;
    op.storage_ = val;
    op.type_ = MOperandType::FPImmediate;
    return op;
}

MOperand MOperand::create_frame_index(int index)
{
    MOperand op;
    op.storage_ = index;
    op.type_ = MOperandType::FrameIndex;
    return op;
}

MOperand MOperand::create_global(GlobalVariable *global_variable)
{
    MOperand op;
    op.storage_ = global_variable;
    op.type_ = MOperandType::GlobalAddress;
    return op;
}

MOperand MOperand::create_external_sym(std::string symbol)
{
    MOperand op;
    op.storage_ = ExternalSymbol{std::move(symbol)};
    op.type_ = MOperandType::ExternalSymbol;
    return op;
}

MOperand MOperand::create_label(std::string symbol)
{
    MOperand op;
    op.storage_ = Label{std::move(symbol)};
    op.type_ = MOperandType::Label;
    return op;
}

MOperand MOperand::create_basic_block(MachineBasicBlock *bb)
{
    MOperand op;
    op.storage_ = bb;
    op.type_ = MOperandType::BasicBlock;
    return op;
}

MOperand MOperand::create_mem_ri(unsigned base_reg, int offset)
{
    MOperand op;
    op.storage_ = MEMri{base_reg, offset};
    op.type_ = MOperandType::MEMri;
    return op;
}

MOperand MOperand::create_mem_rr(unsigned base_reg, unsigned index_reg)
{
    MOperand op;
    op.storage_ = MEMrr{base_reg, index_reg};
    op.type_ = MOperandType::MEMrr;
    return op;
}
unsigned MOperand::reg() const
{
    assert(is_reg());
    return std::get<unsigned>(storage_);
}

int64_t MOperand::imm() const
{
    assert(is_imm());
    return std::get<int64_t>(storage_);
}

double MOperand::fp_imm() const
{
    assert(is_fp_imm());
    return std::get<double>(storage_);
}

int MOperand::frame_index() const
{
    assert(is_frame_index());
    return std::get<int>(storage_);
}

GlobalVariable *MOperand::global() const
{
    assert(is_global());
    return std::get<GlobalVariable *>(storage_);
}

const char *MOperand::external_sym() const
{
    assert(is_external_sym());
    return std::get<ExternalSymbol>(storage_).value.c_str();
}

const char *MOperand::label() const
{
    assert(is_label());
    return std::get<Label>(storage_).value.c_str();
}

MachineBasicBlock *MOperand::basic_block() const
{
    assert(is_basic_block());
    return std::get<MachineBasicBlock *>(storage_);
}

MOperand::MEMri MOperand::mem_ri() const
{
    assert(is_mem_ri());
    return std::get<MEMri>(storage_);
}

MOperand::MEMrr MOperand::mem_rr() const
{
    assert(is_mem_rr());
    return std::get<MEMrr>(storage_);
}

unsigned MOperand::base_reg() const
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

#include <cassert>

bool MOperand::is_reg() const noexcept
{
    bool expected = std::holds_alternative<unsigned>(storage_);
    bool actual = type_ == MOperandType::Register;
    assert(expected == actual);
    return actual;
}
bool MOperand::is_imm() const noexcept
{
    bool expected = std::holds_alternative<int64_t>(storage_);
    bool actual = type_ == MOperandType::Immediate;
    assert(expected == actual);
    return actual;
}
bool MOperand::is_fp_imm() const noexcept
{
    bool expected = std::holds_alternative<double>(storage_);
    bool actual = type_ == MOperandType::FPImmediate;
    assert(expected == actual);
    return actual;
}
bool MOperand::is_frame_index() const noexcept
{
    bool expected = std::holds_alternative<int>(storage_);
    bool actual = type_ == MOperandType::FrameIndex;
    assert(expected == actual);
    return actual;
}
bool MOperand::is_global() const noexcept
{
    bool expected = std::holds_alternative<GlobalVariable *>(storage_);
    bool actual = type_ == MOperandType::GlobalAddress;
    assert(expected == actual);
    return actual;
}
bool MOperand::is_external_sym() const noexcept
{
    bool expected = std::holds_alternative<ExternalSymbol>(storage_);
    bool actual = type_ == MOperandType::ExternalSymbol;
    assert(expected == actual);
    return actual;
}

bool MOperand::is_label() const noexcept
{
    bool expected = std::holds_alternative<Label>(storage_);
    bool actual = type_ == MOperandType::Label;
    assert(expected == actual);
    return actual;
}

bool MOperand::is_basic_block() const noexcept
{
    bool expected = std::holds_alternative<MachineBasicBlock *>(storage_);
    bool actual = type_ == MOperandType::BasicBlock;
    assert(expected == actual);
    return actual;
}

bool MOperand::is_mem_ri() const noexcept
{
    bool expected = std::holds_alternative<MEMri>(storage_);
    bool actual = type_ == MOperandType::MEMri;
    assert(expected == actual);
    return actual;
}

bool MOperand::is_mem_rr() const noexcept
{
    bool expected = std::holds_alternative<MEMrr>(storage_);
    bool actual = type_ == MOperandType::MEMrr;
    assert(expected == actual);
    return actual;
}

bool MOperand::is_mem_rix() const noexcept
{
    bool expected = std::holds_alternative<MEMrix>(storage_);
    bool actual = type_ == MOperandType::MEMrix;
    assert(expected == actual);
    return actual;
}

bool MOperand::is_valid() const noexcept
{
    bool expected = !std::holds_alternative<std::monostate>(storage_);
    bool actual = type_ != MOperandType::Invalid;
    assert(expected == actual);
    return actual;
}

std::string MOperand::to_string() const
{
    if (!is_valid())
        return "INVALID";

    std::ostringstream oss;
    if (is_reg())
    {
        oss << "R" << reg();
        if (is_def())
            oss << "<def>";
    }
    else if (is_imm())
    {
        oss << "#" << imm();
    }
    else if (is_fp_imm())
    {
        oss << "#" << fp_imm();
    }
    else if (is_mem_ri())
    {
        auto mem = mem_ri();
        oss << "[R" << mem.base_reg << " + " << mem.offset << "]";
    }
    else if (is_mem_rr())
    {
        auto mem = mem_rr();
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
        oss << "fi#" << frame_index();
    }
    else if (is_global())
    {
        oss << "global(" << global()->name() << ")";
    }
    else if (is_external_sym())
    {
        oss << "sym(" << external_sym() << ")";
    }
    else if (is_label())
    {
        oss << "label(" << label() << ")";
    }
    else if (is_basic_block())
    {
        oss << "bb(" << basic_block()->label() << ")";
    }
    return oss.str();
}

//===----------------------------------------------------------------------===//
// MachineInst Implementation
//===----------------------------------------------------------------------===//
size_t MachineInst::position() const
{
    MO_ASSERT(parent(), "Instruction has no parent function");
    auto mf = parent()->parent();
    return mf->get_global_instr_pos(this);
}

void MachineInst::erase_from_parent() const
{
    MO_ASSERT(parent(), "Instruction has no parent function");
    auto bb = parent();
    auto pos = bb->locate(this);
    bb->erase(pos);
}

std::string MachineInst::to_string(const TargetInstInfo *target_info) const
{
    std::ostringstream oss;
    // TODO: Add position information
    // oss << "[" << std::setw(4) << mf.get_global_instr_pos(this) << "] ";

    if (target_info)
    {
        // If target info is available, query opcode name

        oss << std::setw(8) << target_info->opcode_name(opcode_);
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
void MachineInst::add_operand(const MOperand &op) { ops_.push_back(op); }
void MachineInst::insert_operand(unsigned idx, const MOperand &op)
{
    ops_.insert(ops_.begin() + idx, op);
}
void MachineInst::remove_operand(unsigned idx) { ops_.erase(ops_.begin() + idx); }
void MachineInst::replace_reg(unsigned old_reg, unsigned new_reg)
{
    // Iterate through all operands
    for (auto &op : ops_)
    {
        // Replace direct register operands
        if (op.is_reg() && op.reg() == old_reg)
        {
            // Save original operand flags
            bool is_def = op.is_def();

            // Create a new register operand and preserve flags
            op = MOperand::create_reg(new_reg, is_def);
            continue;
        }

        // Replace base register in memory operands
        if (op.is_mem_ri() && op.base_reg() == old_reg)
        {
            MOperand::MEMri mem = op.mem_ri();
            op = MOperand::create_mem_ri(new_reg, mem.offset);
            continue;
        }

        // Replace register-register memory operands
        if (op.is_mem_rr())
        {
            MOperand::MEMrr mem = op.mem_rr();
            if (mem.base_reg == old_reg || mem.index_reg == old_reg)
            {
                unsigned new_base = (mem.base_reg == old_reg) ? new_reg : mem.base_reg;
                unsigned new_index = (mem.index_reg == old_reg) ? new_reg : mem.index_reg;
                op = MOperand::create_mem_rr(new_base, new_index);
            }
            continue;
        }

        // Replace scaled index memory operands
        if (op.is_mem_rix())
        {
            MOperand::MEMrix mem = op.get_mem_rix();
            if (mem.base_reg == old_reg || mem.index_reg == old_reg)
            {
                unsigned new_base = (mem.base_reg == old_reg) ? new_reg : mem.base_reg;
                unsigned new_index = (mem.index_reg == old_reg) ? new_reg : mem.index_reg;
                op = MOperand::create_mem_rix(new_base, new_index, mem.scale, mem.offset);
            }
            continue;
        }
    }
}
void MachineInst::remap_registers(const std::map<unsigned, unsigned> &vreg_map)
{
    // Iterate through all operands
    for (auto &op : ops_)
    {
        // Remap direct register operands
        if (op.is_reg())
        {
            unsigned old_reg = op.reg();
            auto it = vreg_map.find(old_reg);
            if (it != vreg_map.end())
            {
                // Found a mapping, replace with physical register
                bool is_def = op.is_def();

                op = MOperand::create_reg(it->second, is_def);
            }
        }

        // Remap base register in memory operands
        if (op.is_mem_ri())
        {
            MOperand::MEMri mem = op.mem_ri();
            auto it = vreg_map.find(mem.base_reg);
            if (it != vreg_map.end())
            {
                op = MOperand::create_mem_ri(it->second, mem.offset);
            }
        }

        // Remap register-register memory operands
        if (op.is_mem_rr())
        {
            MOperand::MEMrr mem = op.mem_rr();
            unsigned new_base = mem.base_reg;
            unsigned new_index = mem.index_reg;
            bool changed = false;

            auto base_it = vreg_map.find(mem.base_reg);
            if (base_it != vreg_map.end())
            {
                new_base = base_it->second;
                changed = true;
            }

            auto index_it = vreg_map.find(mem.index_reg);
            if (index_it != vreg_map.end())
            {
                new_index = index_it->second;
                changed = true;
            }

            if (changed)
            {
                op = MOperand::create_mem_rr(new_base, new_index);
            }
        }

        // Remap scaled index memory operands
        if (op.is_mem_rix())
        {
            MOperand::MEMrix mem = op.get_mem_rix();
            unsigned new_base = mem.base_reg;
            unsigned new_index = mem.index_reg;
            bool changed = false;

            auto base_it = vreg_map.find(mem.base_reg);
            if (base_it != vreg_map.end())
            {
                new_base = base_it->second;
                changed = true;
            }

            auto index_it = vreg_map.find(mem.index_reg);
            if (index_it != vreg_map.end())
            {
                new_index = index_it->second;
                changed = true;
            }

            if (changed)
            {
                op = MOperand::create_mem_rix(new_base, new_index, mem.scale, mem.offset);
            }
        }
    }
}
std::set<unsigned> MachineInst::uses() const
{
    std::set<unsigned> regs;
    // Iterate through all operands, collecting used (read) registers
    for (const auto &op : ops_)
    {
        // Direct register operands (not definitions)
        if (op.is_reg() && !op.is_def())
        {
            regs.insert(op.reg());
        }

        // Base register in memory operands
        if (op.is_mem_ri())
        {
            regs.insert(op.mem_ri().base_reg);
        }

        // Registers in register-register memory operands
        if (op.is_mem_rr())
        {
            regs.insert(op.mem_rr().base_reg);
            regs.insert(op.mem_rr().index_reg);
        }

        // Registers in scaled index memory operands
        if (op.is_mem_rix())
        {
            MOperand::MEMrix mem = op.get_mem_rix();
            regs.insert(mem.base_reg);
            regs.insert(mem.index_reg);
        }
    }

    // remove ZERO register from set
    return regs;
}

std::set<unsigned> MachineInst::defs() const
{
    std::set<unsigned> regs;
    // Iterate through all operands, collecting defined (written) registers
    for (const auto &op : ops_)
    {
        // Collect only register operands with the definition flag
        if (op.is_reg() && op.is_def())
        {
            regs.insert(op.reg());
        }
    }

    // remove ZERO register from set
    return regs;
}

//===----------------------------------------------------------------------===//
// MachineBasicBlock Implementation
//===----------------------------------------------------------------------===//

MachineBasicBlock::MachineBasicBlock(MachineFunction &machine_function,
                                     unsigned number, std::string label)
    : mf_(machine_function), number_(number), label_(std::move(label)) {}

MachineBasicBlock::iterator MachineBasicBlock::insert(
    iterator pos, std::unique_ptr<MachineInst> inst)
{
    mf_.mark_global_positions_dirty();
    mf_.live_range_analyzer()->mark_dirty();
    inst->parent_bb_ = this;
    return insts_.insert(pos, std::move(inst));
}

void MachineBasicBlock::erase(iterator pos)
{
    mf_.mark_global_positions_dirty();
    mf_.live_range_analyzer()->mark_dirty();
    insts_.erase(pos);
}

void MachineBasicBlock::append(std::unique_ptr<MachineInst> mi)
{
    mf_.mark_global_positions_dirty();
    mf_.live_range_analyzer()->mark_dirty();
    mi->parent_bb_ = this;
    insts_.push_back(std::move(mi));
}

MachineBasicBlock::iterator MachineBasicBlock::locate(const MachineInst *mi)
{
    auto it = std::find_if(insts_.begin(), insts_.end(), [mi](const std::unique_ptr<MachineInst> &ptr)
                           { return ptr.get() == mi; });
    return it;
}

MachineBasicBlock *MachineBasicBlock::next_physical_block() const
{
    auto it = mf_.locate(this);
    if (it == mf_.basic_blocks().end())
    {
        return nullptr;
    }
    it++;
    if (it == mf_.basic_blocks().end())
    {
        return nullptr;
    }
    return it->get();
}
MachineBasicBlock *MachineBasicBlock::prev_physical_block() const
{
    auto it = mf_.locate(this);
    if (it == mf_.basic_blocks().begin())
    {
        return nullptr;
    }
    it--;
    return it->get();
}

void MachineBasicBlock::add_successor(MachineBasicBlock *successor)
{
    mf_.mark_global_positions_dirty();
    mf_.live_range_analyzer()->mark_dirty();
    successors_.insert(successor);
    successor->predecessors_.insert(this);
}

void MachineBasicBlock::remove_successor(MachineBasicBlock *successor)
{
    mf_.mark_global_positions_dirty();
    mf_.live_range_analyzer()->mark_dirty();
    auto it = std::find(successors_.begin(), successors_.end(), successor);
    if (it != successors_.end())
    {
        successors_.erase(it);
        successor->predecessors_.erase(std::find(successor->predecessors_.begin(),
                                                 successor->predecessors_.end(),
                                                 this));
    }
}

void MachineBasicBlock::add_predecessor(MachineBasicBlock *predecessor)
{
    mf_.mark_global_positions_dirty();
    mf_.live_range_analyzer()->mark_dirty();
    predecessors_.insert(predecessor);
    predecessor->successors_.insert(this);
}

void MachineBasicBlock::remove_predecessor(MachineBasicBlock *predecessor)
{
    mf_.mark_global_positions_dirty();
    mf_.live_range_analyzer()->mark_dirty();
    auto it = std::find(predecessors_.begin(), predecessors_.end(), predecessor);
    if (it != predecessors_.end())
    {
        predecessors_.erase(it);
        predecessor->successors_.erase(std::find(predecessor->successors_.begin(),
                                                 predecessor->successors_.end(),
                                                 this));
    }
}

size_t MachineBasicBlock::global_start() const
{
    auto it = insts_.begin();
    if (it == insts_.end())
        return 0;

    return mf_.get_global_instr_pos((*it).get());
}
size_t MachineBasicBlock::global_end_inclusive() const
{
    auto it = insts_.end();
    if (it == insts_.begin())
        return 0;

    it--;
    return mf_.get_global_instr_pos((*it).get());
}

std::string MachineBasicBlock::to_string() const
{
    std::ostringstream oss;
    oss << label() << ":\n";
    for (const auto &inst : insts_)
    {
        oss << "  " << inst->to_string() << "\n";
    }
    return oss.str();
}

unsigned MachineBasicBlock::get_instr_global_pos(iterator it) const
{
    return mf_.get_global_instr_pos(it->get());
}

//===----------------------------------------------------------------------===//
// MachineFunction Implementation
//===----------------------------------------------------------------------===//

MachineBasicBlock *MachineFunction::create_block(std::string label)
{
    if (label.empty())
    {
        label = "BB" + std::to_string(next_bb_number_);
    }
    auto block =
        std::make_unique<MachineBasicBlock>(*this, next_bb_number_, std::move(label));
    auto *ptr = block.get();
    blocks_.push_back(std::move(block));
    next_bb_number_++;
    return ptr;
}

void MachineFunction::ensure_global_positions_computed() const
{
    if (!global_positions_dirty_)
        return;

    MO_DEBUG("Recomputing global instruction positions");

    // Reset global instruction positions
    global_instr_positions_.clear();
    next_global_pos_ = 0;

    // Traverse all instructions in basic block order
    for (const auto &bb : blocks_)
    {
        for (const auto &inst : bb->instructions())
        {
            global_instr_positions_[inst.get()] = next_global_pos_++;
            // printf("Global position for %p is %d\n", inst.get(), next_global_pos_);
        }
    }
    global_positions_dirty_ = false;

    MO_DEBUG("%zu global instruction positions computed", global_instr_positions_.size());
}

unsigned MachineFunction::create_vreg(unsigned register_class_id, unsigned size,
                                      bool is_fp, Value *original_value)
{
    vreg_infos_[next_vreg_] = {register_class_id, size, is_fp, original_value};
    return next_vreg_++;
}

unsigned MachineFunction::clone_vreg(unsigned vreg) {
    vreg_infos_[next_vreg_] = vreg_infos_[vreg];
    return next_vreg_++;
}

const VRegInfo &MachineFunction::get_vreg_info(unsigned reg) const
{
    auto it = vreg_infos_.find(reg);
    MO_ASSERT(it != vreg_infos_.end(), "Invalid virtual register: %u", reg);
    return it->second;
}

std::string MachineFunction::to_string() const
{
    std::ostringstream oss;
    oss << "Function: " << ir_func_->name() << "\n";
    oss << "Stack objects:\n";
    for (const auto &[idx, obj] : frame_->objects())
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
// TargetRegisterInfo Implementation
//===----------------------------------------------------------------------===//
const std::vector<unsigned> TargetRegisterInfo::get_classes_of_reg(unsigned reg) const
{
    static const std::vector<unsigned> empty_reg_list_;

    if (reg >= reg_descs_.size() || !reg_descs_[reg].rc_mask.any())
        return empty_reg_list_;

    // Build and return a list containing all register classes to which this register belongs
    std::vector<unsigned> result;
    for (unsigned i = 0; i < register_classes_.size(); ++i)
    {
        if (reg_descs_[reg].rc_mask.test(i))
            result.push_back(i);
    }
    return result;
}

const std::vector<RegisterClass *> TargetRegisterInfo::get_reg_classes() const
{
    std::vector<RegisterClass *> result;
    for (const auto &rc : register_classes_)
        result.push_back(rc.get());
    return result;
}

unsigned TargetRegisterInfo::get_reg_class_weight(unsigned register_class_id) const
{
    if (register_class_id >= register_classes_.size())
        return 0;
    return register_classes_[register_class_id]->weight;
}

unsigned TargetRegisterInfo::get_reg_weight(unsigned reg) const
{
    if (reg >= reg_descs_.size())
        return 0;

    // Get the primary register class of the register
    unsigned primary_rc = get_primary_reg_class(reg);
    return get_reg_class_weight(primary_rc);
}

void TargetRegisterInfo::add_register_class(const RegisterClass &register_class)
{
    std::unique_ptr<RegisterClass> rc_copy =
        std::make_unique<RegisterClass>(register_class);

    // Update the rc_mask of all registers contained in this register class
    for (unsigned reg : register_class.regs)
    {
        if (reg < reg_descs_.size())
        {
            reg_descs_[reg].rc_mask.set(register_classes_.size());

            // If this is the first register class containing this register, set it as the primary register class
            if (!reg_descs_[reg].rc_mask.any())
                reg_descs_[reg].primary_rc_id = register_classes_.size();
        }
    }

    register_classes_.push_back(std::move(rc_copy));
}

RegisterClass *TargetRegisterInfo::get_reg_class(unsigned register_class_id) const
{
    if (register_class_id >= register_classes_.size())
        return nullptr;
    return register_classes_[register_class_id].get();
}

void TargetRegisterInfo::add_alias(unsigned reg, unsigned alias, bool bidirectional)
{
    if (reg >= alias_map_.size() || alias >= alias_map_.size())
        return;

    alias_map_[reg].insert(alias);

    if (bidirectional)
        alias_map_[alias].insert(reg);
}

bool TargetRegisterInfo::are_aliases(unsigned reg1, unsigned reg2) const
{
    if (reg1 == reg2)
        return true;

    if (reg1 >= alias_map_.size() || reg2 >= alias_map_.size())
        return false;

    return alias_map_[reg1].count(reg2) > 0;
}

void TargetRegisterInfo::get_aliases(unsigned reg, std::vector<unsigned> &aliases) const
{
    aliases.clear();

    if (reg >= alias_map_.size())
        return;

    for (unsigned alias : alias_map_[reg])
        aliases.push_back(alias);
}
std::vector<unsigned> TargetRegisterInfo::get_allocatable_regs(unsigned reg_class_id) const
{
    std::vector<unsigned> result;

    if (reg_class_id >= register_classes_.size())
        return result;

    const RegisterClass *rc = register_classes_[reg_class_id].get();
    for (unsigned reg : rc->regs)
    {
        if (can_allocate_reg(reg))
            result.push_back(reg);
    }

    return result;
}

std::vector<unsigned> TargetRegisterInfo::get_temp_regs(CallingConv::ID cc, unsigned reg_class_id) const
{
    std::vector<unsigned> result;

    MO_ASSERT(call_conventions_.count(cc), "Unsupport calling convention: %u", cc);
    const CallingConventionRules &rules = call_conventions_.at(cc);

    MO_ASSERT(rules.temp_regs.size() > 0, "No temporary registers for calling convention: %u %p", cc, &rules);
    for (auto reg : rules.temp_regs)
    {
        result.push_back(reg);
    }

    return result;
}

std::vector<unsigned> TargetRegisterInfo::get_allocation_order(unsigned reg_class_id) const
{
    // The default implementation just returns the allocatable registers
    // Targets can override this method to provide a custom allocation order
    return get_allocatable_regs(reg_class_id);
}

bool TargetRegisterInfo::can_allocate_reg(unsigned reg, bool ignore_reserved) const
{
    if (reg >= reg_descs_.size())
    {
        return false;
    }

    // Check if the register is allocatable
    if (!reg_descs_[reg].is_allocatable)
    {
        return false;
    }

    // If reserved registers are not ignored, check if the register is reserved
    if (!ignore_reserved && reg_descs_[reg].is_reserved)
    {
        return false;
    }

    return true;
}

unsigned TargetRegisterInfo::get_reg_size_in_bytes(unsigned reg) const
{
    if (reg >= reg_descs_.size())
        return 0;

    // Get the primary register class of the register
    unsigned primary_rc = get_primary_reg_class(reg);
    if (primary_rc >= register_classes_.size())
        return 0;

    // Return the size of the register class in bytes
    return register_classes_[primary_rc]->copy_cost;
}

unsigned TargetRegisterInfo::get_suitable_reg_class(unsigned vreg) const
{
    MO_UNREACHABLE();
}

//===----------------------------------------------------------------------===//
// MachineFunction Implementation
//===----------------------------------------------------------------------===//

RegAllocResult MachineFunction::allocate_registers(RegisterAllocator &allocator)
{
    if (reg_alloc_result_ && (*reg_alloc_result_).successful)
        return *reg_alloc_result_; // Already allocated

    // Compute live ranges (if needed)
    lra_->compute();

    // Run register allocation
    RegAllocResult result = allocator.allocate_registers();

    reg_alloc_result_ = result;

    return result;
}

int MachineFunction::create_register_spill_slot(unsigned vreg, unsigned reg_class_id)
{
    FrameObjectMetadata spill_obj;
    const VRegInfo &vreg_info = get_vreg_info(vreg);

    spill_obj.size = vreg_info.size_;
    spill_obj.alignment = spill_obj.size; // Align by size
    spill_obj.flags = FrameObjectMetadata::IsSpillSlot;
    spill_obj.spill_rc_id = reg_class_id;

    return frame_->create_frame_object(spill_obj);
}

//===----------------------------------------------------------------------===//
// MachineModule Implementation
//===----------------------------------------------------------------------===//

MachineFunction *MachineModule::create_machine_function(Function *function)
{
    functions_.push_back(std::make_unique<MachineFunction>(function, this));
    return functions_.back().get();
}

void MachineModule::set_target_info(const TargetRegisterInfo *tri,
                                    const TargetInstInfo *tii)
{
    tri_ = tri;
    tii_ = tii;
}

const std::vector<std::unique_ptr<MachineBasicBlock>> &MachineFunction::basic_blocks() const
{
    return blocks_;
}

MachineFunction::iterator MachineFunction::locate(const MachineBasicBlock *mbb)
{
    auto ret = std::find_if(blocks_.begin(), blocks_.end(),
                            [mbb](const std::unique_ptr<MachineBasicBlock> &ptr)
                            {
                                return ptr.get() == mbb;
                            });
    return ret;
}
void MachineFunction::build_cfg()
{
    auto &mf = *this;
    auto tii = parent()->target_inst_info();

    // 清除所有基本块现有的CFG边
    for (auto &mbb : mf.basic_blocks())
    {
        mbb->clear_cfg();
    }

    // 遍历每个基本块，建立新的CFG边
    for (auto it = mf.basic_blocks().begin(); it != mf.basic_blocks().end(); ++it)
    {
        MachineBasicBlock *cur_mbb = it->get();

        // 查找终止指令（最后一条指令且标记为Terminator）
        MachineInst *terminator = nullptr;
        if (!cur_mbb->instructions().empty())
        {
            auto &last_inst = cur_mbb->instructions().back();
            if (last_inst->has_flag(MIFlag::Terminator))
            {
                terminator = last_inst.get();
            }
        }

        std::unordered_set<MachineBasicBlock *> out_branch_targets;
        MachineBasicBlock *out_fall_through = nullptr;

        if (terminator)
        {
            // 分析分支指令获取目标
            tii->analyze_branch(*cur_mbb, terminator, out_branch_targets, out_fall_through);

            // 添加显式分支目标
            for (auto target : out_branch_targets)
            {
                if (target)
                {
                    cur_mbb->add_successor(target);
                    target->add_predecessor(cur_mbb);
                }
            }

            // 添加fall-through目标
            if (out_fall_through)
            {
                cur_mbb->add_successor(out_fall_through);
                out_fall_through->add_predecessor(cur_mbb);
            }
        }
        else
        {
            // 无终止指令时自动添加顺序后继
            auto next_it = std::next(it);
            if (next_it != mf.basic_blocks().end())
            {
                MachineBasicBlock *next_mbb = next_it->get();
                cur_mbb->add_successor(next_mbb);
                next_mbb->add_predecessor(cur_mbb);
            }
        }
    }
}

void MachineFunction::export_text(std::ostream &out) const
{
    ensure_global_positions_computed();
    auto tii = parent()->target_inst_info();
    for (const auto &bb : basic_blocks())
    {
        out << bb->label() << ":\n";
        for (const auto &inst : *bb.get())
        {
            unsigned pos = get_global_instr_pos(inst.get());
            out << std::setw(4) << pos << ": " << inst->to_string(tii) << "\n";
        }
    }
}
void MachineFunction::dump_cfg(std::ostream &out) const
{
    auto tii = parent()->target_inst_info();

    lra_->compute(); // 确保信息是最新的

    out << "digraph CFG {\n";
    out << "  node [shape=rectangle];\n";

    // 输出基本块节点
    for (const auto &bb : basic_blocks())
    {
        out << "  " << bb->label() << " [fontname=\"consolas\";label=\""
            << bb->label() << " [" << bb->global_start() << ", " << bb->global_end_inclusive() << "]\\l";

        // 输出指令
        for (const auto &inst : bb->instructions())
        {
            unsigned pos = get_global_instr_pos(inst.get());
            out << std::setw(4) << pos << ": " << inst->to_string(tii) << "\\l";
        }

        // 输出 Use/Def
        out << "USE: {";
        for (unsigned reg : lra_->block_info(bb.get()).use)
        {
            out << reg << " ";
        }
        out << "}\\l";

        out << "DEF: {";
        for (unsigned reg : lra_->block_info(bb.get()).def)
        {
            out << reg << " ";
        }
        out << "}\\l";

        // 输出 In/Out
        out << " IN: {";
        for (unsigned reg : lra_->block_info(bb.get()).in)
        {
            out << reg << " ";
        }
        out << "}\\l";

        out << "OUT: {";
        for (unsigned reg : lra_->block_info(bb.get()).out)
        {
            out << reg << " ";
        }
        out << "}\\l";

        out << "\"];\n";
    }

    // 输出边
    for (const auto &bb : basic_blocks())
    {
        for (auto succ : bb->successors())
        {
            out << "  " << bb->label() << " -> " << succ->label() << ";\n";
        }
        if (bb->successors().size() == 0)
        {
            out << "  # " << bb->label() << " has no successors\n";
        }
    }

    out << "}\n";
}

void MachineFunction::export_cfg_to_json(std::ostream &os) const
{
    auto tii = parent()->target_inst_info();
    auto lra_ = live_range_analyzer();
    lra_->compute();

    std::set<unsigned> preg_set, vreg_set;

    // 收集所有寄存器
    for (const auto &[reg, lr] : lra_->get_all_live_ranges())
    {
        if (lr->is_allocated())
        {
            preg_set.insert(reg);
        }
        else
        {
            vreg_set.insert(reg);
        }
    }

    // Start of JSON object
    os << "{\n";

    // Basic Blocks
    os << "\"basic_blocks\": [\n";
    bool first_bb = true;
    for (const auto &bb : basic_blocks())
    {
        if (!first_bb)
        {
            os << ",\n";
        }
        first_bb = false;

        const auto &bi = lra_->block_info(bb.get());

        os << "{\n";
        os << "\"label\": \"" << bb->label() << "\",\n";
        os << "\"start\": " << bb->global_start() << ",\n";
        os << "\"end\": " << bb->global_end_inclusive() << ",\n";

        // Instructions
        os << "\"instructions\": [\n";
        bool first_inst = true;
        for (const auto &inst : bb->instructions())
        {
            if (!first_inst)
            {
                os << ",\n";
            }
            first_inst = false;

            unsigned pos = get_global_instr_pos(inst.get());
            os << "{\n";
            os << "\"pos\": " << pos << ",\n";
            std::string escaped_string = escape_json_string(inst->to_string(tii));
            os << "\"text\": \"" << escaped_string << "\",\n";

            // Active Registers
            os << "\"active_pregs\": [";
            bool first_preg = true;
            for (const auto &[reg, lr] : lra_->get_all_live_ranges())
            {
                for (const auto &interval : lr->intervals())
                {
                    if (lr->is_allocated() && interval.start() <= pos && pos < interval.end())
                    {
                        if (!first_preg)
                            os << ", ";
                        os << reg;
                        first_preg = false;
                    }
                }
            }
            os << "],\n";

            os << "\"active_vregs\": [";
            bool first_vreg = true;
            for (const auto &[reg, lr] : lra_->get_all_live_ranges())
            {
                for (const auto &interval : lr->intervals())
                {
                    if (!lr->is_allocated() && interval.start() <= pos && pos < interval.end())
                    {
                        if (!first_vreg)
                            os << ", ";
                        os << reg;
                        first_vreg = false;
                    }
                }
            }
            os << "]\n";

            os << "}";
        }
        os << "\n"
           << "],\n";

        os << "\"use\": [" << mo_join(bi.use, ",") << "],\n";
        os << "\"def\": [" << mo_join(bi.def, ",") << "],\n";
        os << "\"in\": [" << mo_join(bi.in, ",") << "],\n";
        os << "\"out\": [" << mo_join(bi.out, ",") << "]\n";

        os << "}";
    }
    os << "\n"
       << "],\n";

    // Edges
    os << "\"edges\": [\n";
    bool first_edge = true;
    for (const auto &bb : basic_blocks())
    {
        for (auto succ : bb->successors())
        {
            if (!first_edge)
            {
                os << ",\n";
            }
            first_edge = false;
            os << "{\n";
            os << "\"from\": \"" << bb->label() << "\",\n";
            os << "\"to\": \"" << succ->label() << "\"\n";
            os << "}";
        }
    }
    os << "\n"
       << "],\n";

    // Physical Registers
    os << "\"physical_regs\": [";
    bool first_preg_set = true;
    for (unsigned reg : preg_set)
    {
        if (!first_preg_set)
            os << ", ";
        os << reg;
        first_preg_set = false;
    }
    os << "],\n";

    // Virtual Registers
    os << "\"virtual_regs\": [";
    bool first_vreg_set = true;
    for (unsigned reg : vreg_set)
    {
        if (!first_vreg_set)
            os << ", ";
        os << reg;
        first_vreg_set = false;
    }
    os << "]\n";

    // End of JSON object
    os << "}\n";
}

MachineBasicBlock *MachineFunction::get_basic_block_by_label(const std::string &label) const
{
    auto it = std::find_if(blocks_.begin(), blocks_.end(), [label](const std::unique_ptr<MachineBasicBlock> &ptr)
                           { return ptr->label() == label; });
    if (it == blocks_.end())
    {
        return nullptr;
    }
    return it->get();
}
