// riscv_instruction_selector.h - RISC-V specific instruction selection
#pragma once

#include "../codegen/instruction_selector.h"
#include "riscv_target.h"
#include <map>

// Forward declarations
class TargetRegisterInfo;
class TargetInstInfo;
class Value;
class Instruction;
class BasicBlock;
class MachineBasicBlock;

namespace RISCV {

/// RISCVInstructionSelector - RISC-V implementation of instruction selection
/// 
/// This class implements the instruction selection interface for RISC-V targets,
/// converting IR-level operations into RISC-V machine instructions.
class RISCVInstructionSelector : public codegen::InstructionSelector {
private:
    const TargetRegisterInfo* register_info_;
    const TargetInstInfo* inst_info_;
    ABIVersion abi_version_;

public:
    /// Constructor
    /// @param reg_info RISC-V register information
    /// @param inst_info RISC-V instruction information
    RISCVInstructionSelector(const TargetRegisterInfo* reg_info,
                           const TargetInstInfo* inst_info);
    
    /// Destructor
    ~RISCVInstructionSelector() override = default;

    /// Select instructions for a function (IR → RISC-V machine code)
    /// @param ir_func The IR function to convert
    /// @param machine_func The target MachineFunction to populate
    /// @return True if selection succeeded, false on error
    bool selectInstructions(const Function& ir_func, 
                          MachineFunction& machine_func) override;
    
    /// Get target name
    const char* getTargetName() const override;
    
    /// Check if selector supports a specific feature
    bool supportsFeature(const std::string& feature) const override;
    
    /// Get estimated cost of instruction selection
    size_t estimateSelectionCost(const Function& ir_func) const override;

    /// Get the register info used by this selector
    const TargetRegisterInfo* getRegisterInfo() const { return register_info_; }
    
    /// Get the instruction info used by this selector
    const TargetInstInfo* getInstInfo() const { return inst_info_; }
    
    /// Get the ABI version
    ABIVersion getABIVersion() const { return abi_version_; }

private:
    /// Helper method to determine ABI version from register info
    ABIVersion determineABIVersion(const TargetRegisterInfo* reg_info);
    
    /// Select instructions for a basic block
    /// @param ir_bb IR basic block
    /// @param machine_bb Target machine basic block
    /// @return True if selection succeeded
    bool selectBasicBlock(const BasicBlock& ir_bb, MachineBasicBlock& machine_bb);
    
    /// Select instruction for a single IR instruction
    /// @param ir_inst IR instruction
    /// @param machine_bb Target machine basic block to append to
    /// @return True if selection succeeded
    bool selectInstruction(const Instruction& ir_inst, MachineBasicBlock& machine_bb);
    
    /// Get RISC-V register for IR value
    /// @param ir_value IR value
    /// @return RISC-V register number, or INVALID_REG if not found
    unsigned getRegisterForValue(const Value* ir_value);
    
    /// Create a new virtual register
    /// @return New virtual register number
    unsigned createVirtualRegister();

    /// Select instruction for a single IR instruction (Phase 1.5 addition)
    /// @param ir_inst IR instruction
    /// @param machine_bb Target machine basic block to append to
    /// @return True if selection succeeded
    bool selectSingleInstruction(const Instruction& ir_inst, MachineBasicBlock& machine_bb);
    
    /// Get virtual register for IR value (Phase 1.5 addition)
    /// @param ir_value IR value
    /// @return Virtual register number
    unsigned getVirtualRegForValue(const Value* ir_value);
    
    /// Generate function prologue (Commit 7 addition)
    /// @param machine_bb Machine basic block to add prologue to
    void generateFunctionPrologue(MachineBasicBlock& machine_bb);
    
    /// Generate function epilogue (Commit 7 addition)  
    /// @param machine_bb Machine basic block to add epilogue to
    void generateFunctionEpilogue(MachineBasicBlock& machine_bb);
    
    /// Generate conditional branch from ICmp instruction (Commit 8 addition)
    /// @param icmp_inst ICmp instruction providing the condition
    /// @param true_bb Target basic block for true condition
    /// @param false_bb Target basic block for false condition  
    /// @param machine_bb Machine basic block to add branch instructions to
    /// @return True if generation succeeded
    bool generateConditionalBranch(const ICmpInst& icmp_inst, 
                                 const BasicBlock* true_bb,
                                 const BasicBlock* false_bb,
                                 MachineBasicBlock& machine_bb);

private:
    // Next available virtual register number
    unsigned next_virtual_reg_ = 1000;  // Start virtual regs at 1000
    
    // IR value to virtual register mapping (Phase 1.5 addition)
    std::map<const Value*, unsigned> ir_value_to_vreg_;
};

} // namespace RISCV