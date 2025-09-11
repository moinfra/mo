// riscv_assembly_emitter.h - RISC-V specific assembly emission
#pragma once

#include "../codegen/assembly_emitter.h"
#include "riscv_target.h"
#include <string>
#include <memory>

// Forward declarations
class MachineModule;
class MachineFunction;
class MachineBasicBlock;
class MachineInst;
class TargetRegisterInfo;
class TargetInstInfo;

namespace RISCV {

/// RISCVAssemblyEmitter - RISC-V specific assembly emitter
/// 
/// This class converts machine-level code into RISC-V assembly text format.
/// It handles RISC-V specific instruction formatting, register naming,
/// and assembly directives.
class RISCVAssemblyEmitter : public codegen::AssemblyEmitter {
public:
    /// Constructor
    /// @param reg_info Target register information
    /// @param inst_info Target instruction information
    RISCVAssemblyEmitter(const TargetRegisterInfo* reg_info,
                        const TargetInstInfo* inst_info);
    
    /// Emit assembly code for a machine module
    /// @param module The machine module to emit assembly for
    /// @return String containing the complete RISC-V assembly output
    std::string emit(const MachineModule& module) override;
    
    /// Get the target name for this emitter
    /// @return "riscv32" or "riscv64" based on ABI
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
    
    /// Utility methods
    ABIVersion determineABIVersion(const TargetRegisterInfo* reg_info);
};

} // namespace RISCV