#pragma once

#include "../codegen/instruction_selector.h"
#include "../codegen/calling_convention.h"
#include "arm64_target.h"

namespace ARM64 {

class ARM64InstructionSelector : public codegen::InstructionSelector {
private:
    const TargetRegisterInfo* reg_info_;
    const TargetInstInfo* inst_info_;
    
    // Calling convention
    std::unique_ptr<CallingConvention> calling_convention_;
    
    // Virtual register management
    std::unordered_map<const Value*, unsigned> ir_value_to_vreg_;
    unsigned next_vreg_id_;
    
    // Current function context
    MachineFunction* current_machine_func_;
    
    // Frame management
    int frame_size_;
    int current_stack_offset_;  // Track current stack offset for allocations

public:
    ARM64InstructionSelector(const TargetRegisterInfo* reg_info,
                            const TargetInstInfo* inst_info);
    
    /// Set calling convention (configurable)
    void setCallingConvention(std::unique_ptr<CallingConvention> cc) {
        calling_convention_ = std::move(cc);
    }
    
    /// Select instructions for the given function
    bool selectInstructions(const Function& ir_func, 
                          MachineFunction& machine_func) override;
    
    /// Get target name
    const char* getTargetName() const override;
    
    /// Check if selector supports a specific feature
    bool supportsFeature(const std::string& feature) const override;

private:
    //===------------------------------------------------------------------===//
    // Instruction Selection Methods
    //===------------------------------------------------------------------===//
    
    /// Select instructions for a basic block
    bool selectBasicBlock(const BasicBlock& ir_bb, MachineBasicBlock& machine_bb);
    
    /// Select instruction for a single IR instruction
    bool selectInstruction(const Instruction& ir_inst, MachineBasicBlock& machine_bb);
    
    /// Select specific instruction types
    bool selectBinaryInst(const BinaryInst& op, MachineBasicBlock& machine_bb);
    bool selectUnaryInst(const UnaryInst& op, MachineBasicBlock& machine_bb);
    bool selectLoadInst(const LoadInst& load, MachineBasicBlock& machine_bb);
    bool selectStoreInst(const StoreInst& store, MachineBasicBlock& machine_bb);
    bool selectAllocaInst(const AllocaInst& alloca, MachineBasicBlock& machine_bb);
    bool selectCallInst(const CallInst& call, MachineBasicBlock& machine_bb);
    bool selectReturnInst(const ReturnInst& ret, MachineBasicBlock& machine_bb);
    bool selectBranchInst(const BranchInst& branch, MachineBasicBlock& machine_bb);
    bool selectPhiInst(const PhiInst& phi, MachineBasicBlock& machine_bb);
    
    //===------------------------------------------------------------------===//
    // Virtual Register Management
    //===------------------------------------------------------------------===//
    
    /// Get or create virtual register for an IR value
    unsigned getVirtualRegForValue(const Value* ir_value);
    
    /// Get register class for a type
    unsigned getRegisterClassForType(const Type* type) const;
    
    //===------------------------------------------------------------------===//
    // ARM64-specific Helper Methods  
    //===------------------------------------------------------------------===//
    
    /// Create machine operand for IR value
    MOperand createOperandForValue(const Value* value);
    
    /// Create machine operand for immediate value
    MOperand createImmediateOperand(int64_t imm);
    
    /// Create machine operand for register
    MOperand createRegisterOperand(unsigned reg, bool is_def = false);
    
    /// Create machine operand for memory access
    MOperand createMemoryOperand(unsigned base_reg, int64_t offset);
    
    /// Emit function prologue
    void emitPrologue(MachineBasicBlock& entry_bb);
    
    /// Emit function epilogue
    void emitEpilogue(MachineBasicBlock& exit_bb);
    
    /// Get ARM64 opcode for binary operation
    Opcode getARM64Opcode(::Opcode op_kind) const;
    
    /// Get ARM64 condition code for comparison
    Opcode getARM64ConditionCode(::Opcode op_kind) const;
    
    /// Check if immediate can be encoded
    bool canEncodeImmediate(int64_t imm, bool is_12bit = true) const;
    
    /// Load large immediate into register
    void loadLargeImmediate(unsigned dest_reg, int64_t imm, MachineBasicBlock& bb);
    
    /// Calculate stack frame size
    int calculateFrameSize(const Function& func);
    
    /// Allocate stack slot for local variable
    int allocateStackSlot(unsigned size, unsigned align = 8);
};

} // namespace ARM64