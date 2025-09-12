// arm64_assembly_emitter.h - ARM64 specific assembly emission
#pragma once

#include "../codegen/assembly_emitter.h"
#include "arm64_target.h"
#include <string>
#include <memory>

// Forward declarations
class MachineModule;
class MachineFunction;
class MachineBasicBlock;
class MachineInst;
class TargetRegisterInfo;
class TargetInstInfo;

namespace ARM64 {

/// ARM64AssemblyEmitter - ARM64 specific assembly emitter
/// 
/// This class converts machine-level code into ARM64 assembly text format.
/// It handles ARM64 specific instruction formatting, register naming,
/// and assembly directives for both AAPCS64 and Darwin ABIs.
class ARM64AssemblyEmitter : public codegen::AssemblyEmitter {
public:
    /// Constructor
    /// @param reg_info Target register information
    /// @param inst_info Target instruction information
    ARM64AssemblyEmitter(const TargetRegisterInfo* reg_info,
                        const TargetInstInfo* inst_info);
    
    /// Emit assembly code for a machine module
    /// @param module The machine module to emit assembly for
    /// @return String containing the complete ARM64 assembly output
    std::string emit(const MachineModule& module) override;
    
    /// Get the target name for this emitter
    /// @return "arm64-aapcs" or "arm64-apple-darwin" based on ABI
    const char* getTargetName() const override;
    
    /// Check if emitter supports a specific feature
    /// @param feature Feature name to check
    /// @return True if feature is supported
    bool supportsFeature(const std::string& feature) const override;
    
    /// Get estimated size of assembly output
    /// @param module The machine module to estimate
    /// @return Estimated size in bytes
    size_t estimateOutputSize(const MachineModule& module) const override;

    /// Access to component information
    const TargetRegisterInfo* getRegisterInfo() const { return register_info_; }
    const TargetInstInfo* getInstInfo() const { return inst_info_; }
    ABIVersion getABIVersion() const { return abi_version_; }

private:
    /// Target component references
    const TargetRegisterInfo* register_info_;
    const TargetInstInfo* inst_info_;
    ABIVersion abi_version_;
    
    /// Current function being emitted (for register allocation context)
    mutable const MachineFunction* current_function_;
    
    /// Helper methods for assembly emission
    std::string emitFunction(const MachineFunction& function);
    std::string emitBasicBlock(const MachineBasicBlock& bb);
    std::string emitInstruction(const MachineInst& inst);
    std::string emitDirectives();
    std::string emitGlobalLabels(const MachineModule& module);
    
    /// Register and operand formatting
    std::string formatRegister(unsigned reg) const;
    std::string formatOperand(const MOperand& operand) const;
    std::string formatImmediate(int64_t imm) const;
    std::string formatMemoryOperand(const MOperand& base, const MOperand& offset) const;
    
    /// ARM64-specific formatting
    std::string formatBranchTarget(const std::string& label) const;
    std::string formatConditionCode(Opcode cond_op) const;
    std::string formatShiftOperand(const MOperand& operand, unsigned shift_type, unsigned shift_amount) const;
    
    /// Utility methods
    ABIVersion determineABIVersion(const TargetRegisterInfo* reg_info);
    bool isLargeImmediate(int64_t imm, Opcode opcode) const;
    std::string getABIDirectives() const;
};

} // namespace ARM64