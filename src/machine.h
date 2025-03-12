// machine.h - Target-Independent Machine IR

#pragma once

#include <algorithm>
#include <bitset>
#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "ir.h"
#include "lra.h"
#include "reg_alloc.h"
#include "machine_frame.h"

// Forward declarations
class CallingConv;
class Function;
class GlobalVariable;
class MachineBasicBlock;
class MachineFunction;
class MachineInst;
class Module;
class RegisterAllocator;
class TargetFrameLowering;
class TargetInstInfo;
class TargetRegisterInfo;
class Value;

#define MO_MAX_RC 16

//===----------------------------------------------------------------------===//
// Machine Operand Types
//===----------------------------------------------------------------------===//

class MOperand
{
public:
    // Memory operand type definitions
    struct MEMri
    {
        unsigned base_reg;
        int offset;
    };

    struct MEMrr
    {
        unsigned base_reg;
        unsigned index_reg;
    };

    struct MEMrix
    {
        unsigned base_reg;
        unsigned index_reg;
        int scale; // Scale factor, e.g., 1, 2, 4, 8
        int offset;
    };

    struct ExternalSymbol
    {
        std::string value;
    };

    struct Label
    {
        std::string value;
    };

    enum class MOperandType : unsigned
    {
        Invalid,
        Register,
        Immediate,
        FPImmediate,
        FrameIndex,
        GlobalAddress,
        ExternalSymbol,
        Label,
        MEMri,
        MEMrr,
        MEMrix
    };

private:
    using Storage = std::variant<
        std::monostate,   // Invalid
        unsigned,         // Register (physical/virtual register number)
        int64_t,          // Immediate (integer immediate)
        double,           // FPImmediate (floating-point immediate)
        int,              // FrameIndex (stack frame index)
        GlobalVariable *, // Global (global variable)
        ExternalSymbol,     // ExternalSym (external symbol)
        Label,             // Label (label to be resolved to a constant value)
        MEMri,            // MEMri memory operand
        MEMrr,            // MEMrr memory operand
        MEMrix            // MEMrix memory operand
        >;

    Storage storage_;
    MOperandType type_ = MOperandType::Invalid;
    bool is_def_ = false; // Is this a definition.

public:
    MOperand() = default;
    MOperand(const MOperand &) = default;
    MOperand(MOperand &&) = default;
    MOperand &operator=(const MOperand &) = default;
    MOperand &operator=(MOperand &&) = default;

    // Type checking methods
    bool is_reg() const noexcept;
    bool is_imm() const noexcept;
    bool is_fp_imm() const noexcept;
    bool is_frame_index() const noexcept;
    bool is_global() const noexcept;
    bool is_external_sym() const noexcept;
    bool is_label() const noexcept;
    bool is_mem_ri() const noexcept;
    bool is_mem_rr() const noexcept;
    bool is_mem_rix() const noexcept;
    bool is_valid() const noexcept;

    // Creation methods
    static MOperand create_reg(unsigned reg, bool is_def = false);
    static MOperand create_imm(int64_t val);
    static MOperand create_fp_imm(double val);
    static MOperand create_frame_index(int index);
    static MOperand create_global(GlobalVariable *global_variable);
    static MOperand create_external_sym(const char *symbol);
    static MOperand create_label(const char *symbol);
    static MOperand create_mem_ri(unsigned base_reg, int offset);
    static MOperand create_mem_rr(unsigned base_reg, unsigned index_reg);
    static MOperand create_mem_rix(unsigned base_reg, unsigned index_reg, int scale,
                                   int offset);

    // Access methods
    unsigned reg() const;
    int64_t imm() const;
    double fp_imm() const;
    int frame_index() const;
    GlobalVariable *global() const;
    const char *external_sym() const;
    const char *label() const;
    MEMri mem_ri() const;
    MEMrr mem_rr() const;
    MEMrix get_mem_rix() const;
    unsigned base_reg() const;
    bool is_def() const { return is_def_; }

    std::string to_string() const;
};

//===----------------------------------------------------------------------===//
// Machine Instructions
//===----------------------------------------------------------------------===//

/// Machine instruction flags
enum class MIFlag : unsigned
{
    FrameSetup,        // Stack frame setup instruction
    FrameDestroy,      // Stack frame destroy instruction
    Branch,            // Branch instruction
    Call,              // Function call instruction
    Terminator,        // Basic block terminator
    MayLoad,           // May load memory
    MayStore,          // May store memory
    HasDelaySlot,      // Has delay slot
    MayRaiseException, // May raise exception (e.g., division by zero)
    HasSideEffects,    // Has visible side effects (e.g., I/O)
    IsVolatile,        // Volatile memory access
    NotDuplicable,     // Non-duplicable instruction (e.g., specific call)
    IsCompare,         // Comparison operation instruction
    TOTAL_FLAGS = 13   // Total number of flags
};

class MachineInst
{
public:
    using FlagSet = std::bitset<static_cast<size_t>(MIFlag::TOTAL_FLAGS)>;

    enum VerificationLevel
    {
        QUICK_CHECK,     // Basic operand validity check
        TARGET_AGNOSTIC, // Cross-platform generic rule validation
        TARGET_SPECIFIC  // Target-related deep validation
    };

private:
    unsigned opcode_;
    std::vector<MOperand> ops_;
    FlagSet flags_;
    MachineBasicBlock *parent_bb_ = nullptr;

    friend class MachineBasicBlock;

public:
    MachineInst(unsigned opcode, const std::vector<MOperand> &operands = {});

    size_t position() const;
    void erase_from_parent() const;
    // Register allocation support
    void replace_reg(unsigned old_reg, unsigned new_reg);
    void remap_registers(const std::map<unsigned, unsigned> &vreg_map);

    // Flag operations
    void set_flag(MIFlag flag, bool val = true);
    bool has_flag(MIFlag flag) const;
    void clear_all_flags() { flags_.reset(); }

    // Operand management
    MOperand &operand(unsigned index) { return ops_.at(index); }
    void add_operand(const MOperand &operand);
    void insert_operand(unsigned index, const MOperand &operand);
    void remove_operand(unsigned index);
    bool is_operand_def(unsigned index) const noexcept;

    // Verification and string conversion
    bool verify(VerificationLevel level, const TargetInstInfo *target_info = nullptr,
                std::string *err_msg = nullptr) const;
    std::string to_string(const TargetInstInfo *target_info = nullptr) const;

public:
    // Accessors
    unsigned opcode() const { return opcode_; }
    const std::vector<MOperand> &operands() const { return ops_; }
    MachineBasicBlock *parent() const { return parent_bb_; }

    // Analysis methods
    std::set<unsigned> defs() const;
    std::set<unsigned> uses() const;
};

//===----------------------------------------------------------------------===//
// Machine Basic Blocks
//===----------------------------------------------------------------------===//

class MachineBasicBlock
{
public:
    using iterator = std::vector<std::unique_ptr<MachineInst>>::iterator;

private:
    MachineFunction &mf_;
    unsigned number_ = 0; // Unique identifier
    std::string label_;
    std::vector<std::unique_ptr<MachineInst>> insts_;
    std::vector<MachineBasicBlock *> predecessors_;
    std::vector<MachineBasicBlock *> successors_;
    mutable std::unique_ptr<PressureTracker> pressure_tracker_;

public:
    MachineBasicBlock(MachineFunction &mf, unsigned number, std::string label);

    const std::string &label() const { return label_; }
    MachineFunction *parent() const { return &mf_; }
    // Instruction access and manipulation
    const auto &instructions() const { return insts_; }
    iterator begin() { return insts_.begin(); }
    iterator end() { return insts_.end(); }
    iterator insert(iterator pos, std::unique_ptr<MachineInst> inst);
    void erase(iterator pos);
    void append(std::unique_ptr<MachineInst> mi);
    iterator locate(const MachineInst *mi);

    // CFG management
    const std::vector<MachineBasicBlock *> &predecessors() const { return predecessors_; }
    const std::vector<MachineBasicBlock *> &successors() const { return successors_; }
    void clear_cfg()
    {
        predecessors_.clear();
        successors_.clear();
    }
    size_t pred_size() const { return predecessors_.size(); }
    size_t succ_size() const { return successors_.size(); }
    void add_successor(MachineBasicBlock *successor);
    void remove_successor(MachineBasicBlock *successor);
    size_t global_start() const;
    size_t global_end_inclusive() const;
    // Label management
    void set_label(const std::string &label) { label_ = label; }
    std::string get_label() const;

    std::string to_string() const;

private:
    // Helper function to get max instruction position
    unsigned get_max_instr_position() const;

    unsigned get_instr_global_pos(iterator it) const;
};

//===----------------------------------------------------------------------===//
// Machine Function
//===----------------------------------------------------------------------===//

struct VRegInfo
{
    unsigned register_class_id_;      // Register class ID
    unsigned size_;                   // Register size (bytes)
    bool is_fp_ = false;              // Is floating-point register
    Value *original_value_ = nullptr; // Associated IR value
};

class MachineFunction
{

private:
    Function *ir_func_; // Source IR function
    MachineModule *mm_; // Target machine module
    static const unsigned FIRST_VIRT_REG = 1000;

    // Basic blocks
    std::vector<std::unique_ptr<MachineBasicBlock>> blocks_;

    // Frame management
    std::unique_ptr<MachineFrame> frame_;

    // Virtual register management
    unsigned next_vreg_ = FIRST_VIRT_REG; // Virtual register counter
    std::unordered_map<unsigned, VRegInfo> vreg_infos_;

    // Analysis results
    mutable std::unique_ptr<LiveRangeAnalyzer> lra_;

    // Global instruction position map: instruction pointer -> global position
    mutable std::unordered_map<const MachineInst *, size_t>
        global_instr_positions_;
    mutable size_t next_global_pos_ = 0;
    mutable bool global_positions_dirty_ = true;

    // Register allocation
    std::unique_ptr<RegisterAllocator> reg_allocator_;

    bool registers_allocated_ = false;

    // Disallow copy and assignment
    MachineFunction(const MachineFunction &) = delete;
    void operator=(const MachineFunction &) = delete;

    unsigned next_bb_number_ = 0; // Basic block number generator

public:
    static bool is_physical_reg(unsigned reg) { return reg < FIRST_VIRT_REG; }
    static bool is_virtual_reg(unsigned reg) { return reg >= FIRST_VIRT_REG; }

    explicit MachineFunction(Function *ir_function, MachineModule *mm)
        : ir_func_(ir_function), mm_(mm), frame_(new MachineFrame())
    {
        AliasCheckFn is_alias_fn = [this](unsigned reg1, unsigned reg2)
        {
            return false; // TODO: use info given by target
        };
        lra_ = std::unique_ptr<LiveRangeAnalyzer>(
            new LiveRangeAnalyzer(*this, is_alias_fn));
    }

    virtual ~MachineFunction() = default;

    // Basic block management
    MachineBasicBlock *create_block(std::string label = "");
    const std::vector<std::unique_ptr<MachineBasicBlock>> &basicblocks() const;

    // Virtual register management
    unsigned create_vreg(unsigned register_class_id, unsigned size = 4,
                         bool is_fp = false, Value *original_value = nullptr);
    const VRegInfo &get_vreg_info(unsigned reg) const;

    // Helper function to ensure global positions are computed
    void ensure_global_positions_computed() const;

    // Register allocation methods
    bool allocate_registers(RegisterAllocatorFactory::AllocatorType type =
                                RegisterAllocatorFactory::LINEAR_SCAN);
    void set_register_allocator(std::unique_ptr<RegisterAllocator> allocator);
    bool are_registers_allocated() const { return registers_allocated_; }
    int create_register_spill_slot(unsigned vreg, unsigned reg_class_id);

    // Instruction position management
    void mark_global_positions_dirty() { global_positions_dirty_ = true; }
    unsigned get_global_instr_pos(const MachineInst *mi) const
    {
        ensure_global_positions_computed();
        MO_ASSERT(global_instr_positions_.count(mi) != 0, "Instruction %p not found in global position map!", mi);
        return global_instr_positions_.at(mi);
    }

    std::unique_ptr<PressureTracker> compute_pressure() const;

    MachineModule *parent() const { return mm_; }
    LiveRangeAnalyzer *live_range_analyzer() const { return lra_.get(); }
    MachineFrame *frame() const { return frame_.get(); }
    std::string to_string() const;
};

//===----------------------------------------------------------------------===//
// Target Description Classes
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Calling Convention
//===----------------------------------------------------------------------===//

class CallingConv
{
public:
    struct ArgLocation
    {
        enum LocType
        {
            Register,
            Stack
        };
        LocType type_;
        unsigned reg_or_offset_;
    };

    enum ID
    {
        C,    // C calling convention
        Fast, // Fast call
        Vector,
        NUM_CALLING_CONV
    };

    struct RetLocation : ArgLocation
    {
        bool is_in_memory_; // Return value passed through memory
    };

    virtual ~CallingConv() = default;

    virtual void analyze_call(MachineFunction &mf,
                              const std::vector<Value *> &args,
                              std::vector<ArgLocation> &locations) const = 0;
    virtual void analyze_return(MachineFunction &mf, const Value *return_value,
                                RetLocation &location) const = 0;
};

//===----------------------------------------------------------------------===//
// Target Register Information
//===----------------------------------------------------------------------===//

struct RegisterDesc
{
    unsigned int spill_cost; // Register spill cost
    bool is_callee_saved;    // Callee-saved
    bool is_reserved;        // Reserved
    bool is_allocatable;     // Allocatable
    unsigned primary_rc_id;  // Primary register class ID
    std::bitset<16> rc_mask; // Register class mask
};

/// Target register class - represents a set of registers
struct RegisterClass
{
    unsigned id;
    std::string name;
    std::vector<unsigned> regs; // List of registers
    unsigned copy_cost;         // Register-to-register copy cost
    unsigned weight;            // Weight for pressure calculation
};

struct ArgPassingRule
{
    std::vector<unsigned> int_regs; // int param reg sequence
    std::vector<unsigned> fp_regs;  // float param reg sequence
    unsigned stack_align = 16;      // stack alignment
    bool shadow_space = false;      // shadow space for windows x64 calling convention
};

/// Target register information
class TargetRegisterInfo
{
protected:
    // Table of all register descriptors
    std::vector<RegisterDesc> reg_descs_;

    // Register classes
    std::vector<std::unique_ptr<RegisterClass>> register_classes_;

    // Calling convention related data
    std::unordered_map<CallingConv::ID, ArgPassingRule> arg_rules_;
    std::unordered_map<CallingConv::ID, std::vector<unsigned>> callee_saved_map_;
    std::unordered_map<CallingConv::ID, std::vector<unsigned>> caller_saved_map_;

    // Precomputed alias relationship (reg -> set of aliases)
    std::vector<std::unordered_set<unsigned>> alias_map_;

    const std::vector<unsigned> empty_reg_list_;

public:
    virtual ~TargetRegisterInfo() = default;
    explicit TargetRegisterInfo(unsigned num_regs)
        : reg_descs_(num_regs), alias_map_(num_regs) {}

    //===---------------- Register Attribute Queries -------------------===//
    bool is_callee_saved(unsigned reg) const { return reg_descs_[reg].is_callee_saved; }
    bool is_reserved_reg(unsigned reg) const { return reg_descs_[reg].is_reserved; }
    unsigned get_spill_cost(unsigned reg) const { return reg_descs_[reg].spill_cost; }
    unsigned get_primary_reg_class(unsigned reg) const
    {
        return reg_descs_[reg].primary_rc_id;
    }
    const std::vector<unsigned> get_reg_classes(unsigned reg) const;
    unsigned get_reg_class_weight(unsigned register_class_id) const;
    unsigned get_reg_weight(unsigned reg) const;

    //===---------------- Register Class Management ----------------------===//
    void add_register_class(const RegisterClass &register_class);
    RegisterClass *get_reg_class(unsigned register_class_id) const;

    //===---------------- Calling Convention Related ----------------------===//
    const std::vector<unsigned> &get_callee_saved_regs(CallingConv::ID cc) const;
    const std::vector<unsigned> &get_caller_saved_regs(CallingConv::ID cc) const;

    //===---------------- Alias Relationship Handling ----------------------===//
    void add_alias(unsigned reg, unsigned alias, bool bidirectional = true);
    bool are_aliases(unsigned reg1, unsigned reg2) const;
    void get_aliases(unsigned reg, std::vector<unsigned> &aliases) const;

    //===---------------- Register allocation support ----------------------===//
    virtual std::vector<unsigned> get_allocatable_regs(unsigned reg_class_id) const;
    virtual std::vector<unsigned> get_allocation_order(unsigned reg_class_id) const;
    virtual bool can_allocate_reg(unsigned reg, bool ignore_reserved = false) const;
    virtual unsigned get_reg_size_in_bytes(unsigned reg) const;
    virtual unsigned get_suitable_reg_class(unsigned vreg) const;

    //===---------------- Other Helper Methods ----------------------===//
    unsigned get_num_regs() const { return reg_descs_.size(); }
    const RegisterDesc &get_reg_desc(unsigned reg) const { return reg_descs_[reg]; }
};

//===----------------------------------------------------------------------===//
// Target Instruction Information
//===----------------------------------------------------------------------===//

/// Target instruction properties
class TargetInstInfo
{
public:
    virtual ~TargetInstInfo() = default;
    // Core interfaces
    virtual const char *opcode_name(unsigned opcode) const = 0;
    virtual unsigned get_inst_size(const MachineInst &mi) const = 0;

    // Instruction verification
    virtual bool verify_instruction(const MachineInst &mi,
                                    std::string &error_msg) const = 0;

    // Instruction encoding
    virtual uint32_t get_binary_encoding(const MachineInst &mi) const = 0;

    // Pseudo-instruction handling
    virtual void expand_pseudo(MachineBasicBlock &mbb,
                               MachineBasicBlock::iterator mi) const = 0;

    // Special instruction identification
    virtual bool is_return(const MachineInst &mi) const = 0;
    virtual bool is_call(const MachineInst &mi) const = 0;

    // Immediate legality check
    virtual bool is_legal_immediate(int64_t immediate,
                                    unsigned operand_size) const = 0;

    // Def-use info
    virtual bool is_operand_def(unsigned op, unsigned index) const = 0;
    virtual bool is_operand_use(unsigned op, unsigned index) const = 0;

    // Scheduling information
    virtual unsigned get_instruction_latency(unsigned opcode) const = 0;

    // Register operations
    virtual void copy_phys_reg(MachineBasicBlock &mbb,
                               MachineBasicBlock::iterator insert, unsigned dest_reg,
                               unsigned src_reg) const = 0;

    // Legalization handling
    virtual bool legalize_inst(MachineBasicBlock &mbb,
                               MachineBasicBlock::iterator mii,
                               MachineFunction &mf) const = 0;

    // Spill/reload generation
    virtual void insert_load_from_stack(MachineBasicBlock &mbb,
                                        MachineBasicBlock::iterator insert_point,
                                        unsigned dest_reg, int frame_index,
                                        int64_t offset = 0) const = 0;

    virtual void insert_store_to_stack(MachineBasicBlock &mbb,
                                       MachineBasicBlock::iterator insert_point,
                                       unsigned src_reg, int frame_index,
                                       int64_t offset = 0) const = 0;
};

//===----------------------------------------------------------------------===//
// Target Frame Lowering
//===----------------------------------------------------------------------===//

struct FrameLayout
{
    int stack_size;
    int offset;
};

class TargetFrameLowering
{
public:
    virtual ~TargetFrameLowering() = default;

    virtual void emit_prologue(MachineFunction &mf) const = 0;
    virtual void emit_epilogue(MachineFunction &mf) const = 0;
    virtual int get_frame_index_offset(const MachineFunction &mf,
                                       int frame_index) const = 0;
    virtual FrameLayout compute_frame_layout(const MachineFunction &mf) const = 0;

    virtual void emit_stack_protector(MachineFunction &mf,
                                      int guard_index) const = 0; // Insert stack protection code
};

//===----------------------------------------------------------------------===//
// Machine Module
//===----------------------------------------------------------------------===//

class MachineModule
{
private:
    Module *ir_module_;
    std::vector<std::unique_ptr<MachineFunction>> functions_;
    const TargetRegisterInfo *tri_ = nullptr;
    const TargetInstInfo *tii_ = nullptr;

public:
    explicit MachineModule(Module *ir_module) : ir_module_(ir_module) {}

    MachineFunction *create_machine_function(Function *function);

    void set_target_info(const TargetRegisterInfo *target_register_info,
                         const TargetInstInfo *target_inst_info);

    const TargetInstInfo *target_inst_info() const { return tii_; }
    const TargetRegisterInfo *target_reg_info() const { return tri_; }
};

//===----------------------------------------------------------------------===//
// Register Pressure Tracking
//===----------------------------------------------------------------------===//

class PressureTracker
{
private:
    const TargetRegisterInfo &tri_;
    std::map<unsigned, unsigned>
        pressure_at_pos_; // Use ordered map for traversal
    mutable bool max_pressure_dirty_ =
        false; // Flag indicating whether to recalculate max pressure
    mutable unsigned cached_max_pressure_ = 0;

    void mark_dirty() { max_pressure_dirty_ = true; }

public:
    explicit PressureTracker(const TargetRegisterInfo &target_register_info)
        : tri_(target_register_info) {}
    void add_interval(const LiveRange &live_range);
    void remove_interval(const LiveRange &live_range);
    unsigned get_max_pressure() const;
    void dump_pressure_curve() const;
};
