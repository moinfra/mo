#include "arm64_instruction_selector.h"
#include "../codegen/calling_convention.h"
#include "../ir.h"
#include "../mo_debug.h"
#include <algorithm>

namespace ARM64 {

ARM64InstructionSelector::ARM64InstructionSelector(const TargetRegisterInfo* reg_info,
                                                  const TargetInstInfo* inst_info)
    : reg_info_(reg_info), inst_info_(inst_info), next_vreg_id_(1000), 
      current_machine_func_(nullptr), frame_size_(16), current_stack_offset_(0) {
    // Initialize with default ARM64 AAPCS64 calling convention
    calling_convention_ = CallingConventionFactory::createForTarget("arm64");
}

bool ARM64InstructionSelector::selectInstructions(const Function& ir_func, 
                                                 MachineFunction& machine_func) {
    MO_DEBUG("ARM64 instruction selection for function: %s", ir_func.name().c_str());
    
    // Set current function context
    current_machine_func_ = &machine_func;
    ir_value_to_vreg_.clear();
    next_vreg_id_ = 1000;
    current_stack_offset_ = 0;  // Reset stack offset for new function
    
    // Calculate frame size based on local variables
    frame_size_ = calculateFrameSize(ir_func);
    
    // Create machine basic blocks for each IR basic block
    std::unordered_map<const BasicBlock*, MachineBasicBlock*> bb_map;
    
    for (const auto ir_bb : ir_func.basic_blocks()) {
        auto* machine_bb = machine_func.create_block(ir_bb->name().empty() ? "bb" : ir_bb->name());
        bb_map[ir_bb] = machine_bb;
    }
    
    // Handle function arguments using calling convention
    auto entry_bb = bb_map[ir_func.basic_blocks()[0]];
    const auto& args = ir_func.args();
    
    if (!args.empty() && calling_convention_) {
        // Get argument types
        std::vector<Type*> arg_types;
        for (const auto& arg : args) {
            arg_types.push_back(arg->type());
        }
        
        // Use calling convention to determine where arguments come from
        auto arg_locations = calling_convention_->getArgumentLocations(arg_types);
        
        for (size_t i = 0; i < args.size() && i < arg_locations.size(); ++i) {
            const auto& loc = arg_locations[i];
            auto arg = args[i];
            
            if (loc.type == ArgumentLocation::Type::Register) {
                auto dest_reg = getVirtualRegForValue(arg);
                auto dest_operand = createRegisterOperand(dest_reg, true);
                auto src_operand = createRegisterOperand(loc.reg_id, false);
                
                // Create move instruction: mov dest_reg, arg_reg
                auto mov_inst = std::make_unique<MachineInst>(ADD);
                mov_inst->add_operand(dest_operand);
                mov_inst->add_operand(src_operand);
                mov_inst->add_operand(createImmediateOperand(0)); // add dest_reg, arg_reg, #0 = mov dest_reg, arg_reg
                
                // Insert after prologue (we'll handle this in selectBasicBlock)
                // For now, we'll add a special marker to handle this
            }
            // TODO: Handle stack arguments
        }
    }
    
    // Select instructions for each basic block
    for (const auto ir_bb : ir_func.basic_blocks()) {
        auto* machine_bb = bb_map[ir_bb];
        if (!selectBasicBlock(*ir_bb, *machine_bb)) {
            return false;
        }
    }
    
    // Add argument handling after prologue for entry block using calling convention
    if (!args.empty() && calling_convention_) {
        auto* entry_bb = machine_func.basic_blocks()[0].get();
        auto insert_point = entry_bb->begin();
        // Skip prologue instructions (STP and ADD)
        if (insert_point != entry_bb->end()) ++insert_point;
        if (insert_point != entry_bb->end()) ++insert_point;
        
        // Get argument types
        std::vector<Type*> arg_types;
        for (const auto& arg : args) {
            arg_types.push_back(arg->type());
        }
        
        // Use calling convention to determine argument locations
        auto arg_locations = calling_convention_->getArgumentLocations(arg_types);
        
        for (size_t i = 0; i < args.size() && i < arg_locations.size(); ++i) {
            const auto& loc = arg_locations[i];
            auto arg = args[i];
            
            if (loc.type == ArgumentLocation::Type::Register) {
                auto dest_reg = getVirtualRegForValue(arg);
                auto dest_operand = createRegisterOperand(dest_reg, true);
                auto src_operand = createRegisterOperand(loc.reg_id, false);
                
                auto mov_inst = std::make_unique<MachineInst>(ADD);
                mov_inst->add_operand(dest_operand);
                mov_inst->add_operand(src_operand);
                mov_inst->add_operand(createImmediateOperand(0));
                
                entry_bb->insert(insert_point, std::move(mov_inst));
            }
            // TODO: Handle stack arguments
        }
    }
    
    return true;
}

const char* ARM64InstructionSelector::getTargetName() const {
    return "arm64-apple-darwin";
}

bool ARM64InstructionSelector::supportsFeature(const std::string& feature) const {
    if (feature == "arm64") return true;
    if (feature == "aarch64") return true;
    if (feature == "neon") return true;
    return false;
}

bool ARM64InstructionSelector::selectBasicBlock(const BasicBlock& ir_bb, 
                                               MachineBasicBlock& machine_bb) {
    MO_DEBUG("Selecting ARM64 instructions for basic block: %s", ir_bb.name().c_str());
    
    // Emit prologue for entry block
    if (ir_bb.name() == "entry") {
        emitPrologue(machine_bb);
    }
    
    // Select instructions for each IR instruction
    for (auto it = ir_bb.begin(); it != ir_bb.end(); ++it) {
        if (!selectInstruction(*it, machine_bb)) {
            return false;
        }
    }
    
    return true;
}

bool ARM64InstructionSelector::selectInstruction(const Instruction& ir_inst, 
                                                MachineBasicBlock& machine_bb) {
    // Use simple dispatch based on instruction kind
    if (auto* bin_inst = dynamic_cast<const BinaryInst*>(&ir_inst)) {
        return selectBinaryInst(*bin_inst, machine_bb);
    } else if (auto* ret_inst = dynamic_cast<const ReturnInst*>(&ir_inst)) {
        return selectReturnInst(*ret_inst, machine_bb);
    } else if (auto* load_inst = dynamic_cast<const LoadInst*>(&ir_inst)) {
        return selectLoadInst(*load_inst, machine_bb);
    } else if (auto* store_inst = dynamic_cast<const StoreInst*>(&ir_inst)) {
        return selectStoreInst(*store_inst, machine_bb);
    } else if (auto* alloca_inst = dynamic_cast<const AllocaInst*>(&ir_inst)) {
        return selectAllocaInst(*alloca_inst, machine_bb);
    } else if (auto* call_inst = dynamic_cast<const CallInst*>(&ir_inst)) {
        return selectCallInst(*call_inst, machine_bb);
    }
    
    MO_DEBUG("Unsupported instruction type in ARM64 selector");
    return true; // Don't fail for unsupported instructions, just ignore them
}

bool ARM64InstructionSelector::selectBinaryInst(const BinaryInst& op, 
                                               MachineBasicBlock& machine_bb) {
    auto lhs_operand = createOperandForValue(op.left());
    auto rhs_operand = createOperandForValue(op.right());
    auto dest_reg = getVirtualRegForValue(&op);
    auto dest_operand = createRegisterOperand(dest_reg, true);
    
    Opcode arm64_op = getARM64Opcode(op.opcode());
    
    // Create machine instruction
    auto machine_inst = std::make_unique<MachineInst>(arm64_op);
    machine_inst->add_operand(dest_operand);
    machine_inst->add_operand(lhs_operand);
    machine_inst->add_operand(rhs_operand);
    
    machine_bb.append(std::move(machine_inst));
    return true;
}

bool ARM64InstructionSelector::selectReturnInst(const ReturnInst& ret, 
                                               MachineBasicBlock& machine_bb) {
    // Handle return value
    if (ret.value()) {
        auto return_operand = createOperandForValue(ret.value());
        
        // Move return value to X0 (ARM64 return register)
        auto x0_operand = createRegisterOperand(X0, true);
        auto mov_inst = std::make_unique<MachineInst>(MOV_IMM);
        mov_inst->add_operand(x0_operand);
        mov_inst->add_operand(return_operand);
        machine_bb.append(std::move(mov_inst));
    }
    
    // Emit epilogue and return
    emitEpilogue(machine_bb);
    
    auto ret_inst = std::make_unique<MachineInst>(RET);
    machine_bb.append(std::move(ret_inst));
    return true;
}

bool ARM64InstructionSelector::selectLoadInst(const LoadInst& load, 
                                             MachineBasicBlock& machine_bb) {
    auto src_operand = createOperandForValue(load.pointer());
    auto dest_reg = getVirtualRegForValue(&load);
    auto dest_operand = createRegisterOperand(dest_reg, true);
    
    auto load_inst = std::make_unique<MachineInst>(LDR);
    load_inst->add_operand(dest_operand);
    load_inst->add_operand(src_operand);
    
    machine_bb.append(std::move(load_inst));
    return true;
}

bool ARM64InstructionSelector::selectStoreInst(const StoreInst& store, 
                                              MachineBasicBlock& machine_bb) {
    auto val_operand = createOperandForValue(store.value());
    auto ptr_operand = createOperandForValue(store.pointer());
    
    auto store_inst = std::make_unique<MachineInst>(STR);
    store_inst->add_operand(val_operand);
    store_inst->add_operand(ptr_operand);
    
    machine_bb.append(std::move(store_inst));
    return true;
}

bool ARM64InstructionSelector::selectAllocaInst(const AllocaInst& alloca, 
                                               MachineBasicBlock& machine_bb) {
    // Allocate unique stack slot for this alloca
    auto dest_reg = getVirtualRegForValue(&alloca);
    auto dest_operand = createRegisterOperand(dest_reg, true);
    
    // Allocate a unique stack offset for this allocation  
    int stack_offset = allocateStackSlot(8, 8);  // 8 bytes, 8-byte aligned
    
    // Generate: add dest_reg, sp, #offset
    auto mov_inst = std::make_unique<MachineInst>(ADD);
    mov_inst->add_operand(dest_operand);
    mov_inst->add_operand(createRegisterOperand(SP));
    mov_inst->add_operand(createImmediateOperand(stack_offset));
    
    machine_bb.append(std::move(mov_inst));
    return true;
}

bool ARM64InstructionSelector::selectCallInst(const CallInst& call, 
                                             MachineBasicBlock& machine_bb) {
    if (!calling_convention_) {
        return false; // No calling convention set
    }
    
    auto args = call.arguments();
    
    // Get argument types
    std::vector<Type*> arg_types;
    for (const auto& arg : args) {
        arg_types.push_back(arg->type());
    }
    
    // Use calling convention to determine argument locations
    auto arg_locations = calling_convention_->getArgumentLocations(arg_types);
    
    // Move arguments to their designated locations
    for (size_t i = 0; i < args.size() && i < arg_locations.size(); ++i) {
        const auto& loc = arg_locations[i];
        
        if (loc.type == ArgumentLocation::Type::Register) {
            auto arg_operand = createOperandForValue(args[i]);
            auto dest_operand = createRegisterOperand(loc.reg_id, true);
            
            // Create move instruction - use MOV_IMM for immediates, ADD for registers
            std::unique_ptr<MachineInst> mov_inst;
            if (arg_operand.is_imm()) {
                // mov dest, #imm
                mov_inst = std::make_unique<MachineInst>(MOV_IMM);
                mov_inst->add_operand(dest_operand);
                mov_inst->add_operand(arg_operand);
            } else {
                // add dest, src, #0 (equivalent to mov dest, src)
                mov_inst = std::make_unique<MachineInst>(ADD);
                mov_inst->add_operand(dest_operand);
                mov_inst->add_operand(arg_operand);
                mov_inst->add_operand(createImmediateOperand(0));
            }
            machine_bb.append(std::move(mov_inst));
        }
        // TODO: Handle stack arguments
    }
    
    // Create call instruction
    auto call_inst = std::make_unique<MachineInst>(BL);
    if (auto* func = call.called_function()) {
        call_inst->add_operand(MOperand::create_label(func->name()));
    }
    machine_bb.append(std::move(call_inst));
    
    // Handle return value using calling convention
    if (call.type() && !call.type()->is_void()) {
        auto return_loc = calling_convention_->getReturnLocation(call.type());
        
        if (return_loc.type == ArgumentLocation::Type::Register) {
            auto ret_reg = getVirtualRegForValue(&call);
            auto ret_operand = createRegisterOperand(ret_reg, true);
            auto return_reg_operand = createRegisterOperand(return_loc.reg_id, false);
            
            auto mov_inst = std::make_unique<MachineInst>(ADD);
            mov_inst->add_operand(ret_operand);
            mov_inst->add_operand(return_reg_operand);
            mov_inst->add_operand(createImmediateOperand(0)); // mov ret_reg, return_reg
            machine_bb.append(std::move(mov_inst));
        }
    }
    
    return true;
}

// Stub implementations for remaining methods
bool ARM64InstructionSelector::selectUnaryInst(const UnaryInst& op, MachineBasicBlock& machine_bb) {
    return true; // Stub
}

bool ARM64InstructionSelector::selectBranchInst(const BranchInst& branch, MachineBasicBlock& machine_bb) {
    return true; // Stub
}

bool ARM64InstructionSelector::selectPhiInst(const PhiInst& phi, MachineBasicBlock& machine_bb) {
    return true; // Stub
}

unsigned ARM64InstructionSelector::getVirtualRegForValue(const Value* ir_value) {
    auto it = ir_value_to_vreg_.find(ir_value);
    if (it != ir_value_to_vreg_.end()) {
        return it->second;
    }
    
    unsigned vreg_id = next_vreg_id_++;
    ir_value_to_vreg_[ir_value] = vreg_id;
    return vreg_id;
}

unsigned ARM64InstructionSelector::getRegisterClassForType(const Type* type) const {
    // Simple register class selection
    return ARM64RegisterInfo::GPR64; // Default to 64-bit GPR
}

MOperand ARM64InstructionSelector::createOperandForValue(const Value* value) {
    // Try to detect constants by dynamic cast
    if (auto* constant = dynamic_cast<const ConstantInt*>(value)) {
        return MOperand::create_imm(constant->value());
    }
    
    // Default: create virtual register operand
    unsigned vreg = getVirtualRegForValue(value);
    return MOperand::create_reg(vreg);
}

MOperand ARM64InstructionSelector::createImmediateOperand(int64_t imm) {
    return MOperand::create_imm(imm);
}

MOperand ARM64InstructionSelector::createRegisterOperand(unsigned reg, bool is_def) {
    return MOperand::create_reg(reg, is_def);
}

MOperand ARM64InstructionSelector::createMemoryOperand(unsigned base_reg, int64_t offset) {
    return MOperand::create_mem_ri(base_reg, offset);
}

void ARM64InstructionSelector::emitPrologue(MachineBasicBlock& entry_bb) {
    // ARM64 function prologue
    // stp x29, x30, [sp, #-16]!
    auto stp_inst = std::make_unique<MachineInst>(STP);
    stp_inst->add_operand(createRegisterOperand(X29));
    stp_inst->add_operand(createRegisterOperand(X30));
    stp_inst->add_operand(createMemoryOperand(SP, -16));
    entry_bb.append(std::move(stp_inst));
    
    // mov x29, sp (set frame pointer)
    auto mov_inst = std::make_unique<MachineInst>(ADD);
    mov_inst->add_operand(createRegisterOperand(X29, true));
    mov_inst->add_operand(createRegisterOperand(SP));
    mov_inst->add_operand(createImmediateOperand(0));
    entry_bb.append(std::move(mov_inst));
}

void ARM64InstructionSelector::emitEpilogue(MachineBasicBlock& exit_bb) {
    // ARM64 function epilogue
    // ldp x29, x30, [sp], #16
    auto ldp_inst = std::make_unique<MachineInst>(LDP);
    ldp_inst->add_operand(createRegisterOperand(X29, true));
    ldp_inst->add_operand(createRegisterOperand(X30, true));
    ldp_inst->add_operand(createMemoryOperand(SP, 16));
    exit_bb.append(std::move(ldp_inst));
}

Opcode ARM64InstructionSelector::getARM64Opcode(::Opcode op_kind) const {
    switch (op_kind) {
    case ::Opcode::Add:    return ADD;
    case ::Opcode::Sub:    return SUB;
    case ::Opcode::Mul:    return MUL;
    default:               return ADD; // Default fallback
    }
}

Opcode ARM64InstructionSelector::getARM64ConditionCode(::Opcode op_kind) const {
    switch (op_kind) {
    case ::Opcode::ICmp:   return BEQ;
    default:               return BEQ; // Default fallback
    }
}

bool ARM64InstructionSelector::canEncodeImmediate(int64_t imm, bool is_12bit) const {
    if (is_12bit) {
        return (imm >= 0 && imm <= 4095);
    }
    return (imm >= -2048 && imm <= 2047);
}

void ARM64InstructionSelector::loadLargeImmediate(unsigned dest_reg, int64_t imm, MachineBasicBlock& bb) {
    // Load large immediate using MOVZ/MOVK sequence
    auto movz_inst = std::make_unique<MachineInst>(MOVZ);
    movz_inst->add_operand(createRegisterOperand(dest_reg, true));
    movz_inst->add_operand(createImmediateOperand(imm & 0xFFFF));
    bb.append(std::move(movz_inst));
}

int ARM64InstructionSelector::calculateFrameSize(const Function& func) {
    // Simple frame size calculation
    return 16; // Minimum frame size for ARM64
}

int ARM64InstructionSelector::allocateStackSlot(unsigned size, unsigned align) {
    // Simple stack slot allocation
    frame_size_ += size;
    return -frame_size_;
}

} // namespace ARM64