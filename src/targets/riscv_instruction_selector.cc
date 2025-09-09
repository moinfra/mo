// riscv_instruction_selector.cc - Implementation of RISC-V instruction selection
#include "riscv_instruction_selector.h"
#include "riscv_target.h"

#include <cassert>

namespace RISCV {

//===----------------------------------------------------------------------===//
// RISCVInstructionSelector Implementation
//===----------------------------------------------------------------------===//

RISCVInstructionSelector::RISCVInstructionSelector(const TargetRegisterInfo* reg_info,
                                                 const TargetInstInfo* inst_info)
    : register_info_(reg_info)
    , inst_info_(inst_info)
    , abi_version_(determineABIVersion(reg_info))
{
    assert(reg_info && "Register info cannot be null");
    assert(inst_info && "Instruction info cannot be null");
}

bool RISCVInstructionSelector::selectInstructions(const Function& ir_func, 
                                                 MachineFunction& machine_func) {
    // TODO: Implement instruction selection
    // For now, return success but do minimal work for testing
    
    // Create a basic block if the machine function is empty
    if (machine_func.basic_blocks().empty()) {
        auto* entry_bb = machine_func.create_block("entry");
        (void)entry_bb;  // Suppress unused variable warning
    }
    
    return true;  // Success (placeholder)
}

const char* RISCVInstructionSelector::getTargetName() const {
    switch (abi_version_) {
        case ABIVersion::ILP32:
        case ABIVersion::ILP32F:
            return "riscv32";
        case ABIVersion::LP64:
        case ABIVersion::LP64F:
        case ABIVersion::LP64D:
            return "riscv64";
        default:
            return "riscv-unknown";
    }
}

bool RISCVInstructionSelector::supportsFeature(const std::string& feature) const {
    // Basic RISC-V features
    if (feature == "riscv") return true;
    if (feature == "i") return true;      // Base integer instruction set
    if (feature == "m") return true;      // Multiplication and division
    if (feature == "instruction-selection") return true;
    
    // ABI-specific features
    if (feature == "32bit") {
        return (abi_version_ == ABIVersion::ILP32 ||
               abi_version_ == ABIVersion::ILP32F);
    }
    if (feature == "64bit") {
        return (abi_version_ == ABIVersion::LP64 ||
               abi_version_ == ABIVersion::LP64F ||
               abi_version_ == ABIVersion::LP64D);
    }
    
    // Floating-point features  
    if (feature == "f") {
        return (abi_version_ == ABIVersion::ILP32F ||
               abi_version_ == ABIVersion::LP64F ||
               abi_version_ == ABIVersion::LP64D);
    }
    if (feature == "d") {
        return (abi_version_ == ABIVersion::LP64D);
    }
    
    return false;
}

size_t RISCVInstructionSelector::estimateSelectionCost(const Function& ir_func) const {
    // RISC-V-specific cost estimation
    size_t base_cost = codegen::InstructionSelector::estimateSelectionCost(ir_func);
    
    // RISC-V tends to have simple instruction selection, so reduce cost slightly
    return base_cost * 8 / 10;  // 80% of base cost
}

//===----------------------------------------------------------------------===//
// Private Helper Methods
//===----------------------------------------------------------------------===//

ABIVersion RISCVInstructionSelector::determineABIVersion(const TargetRegisterInfo* reg_info) {
    // Cast to RISC-V specific register info and get ABI version
    const auto* riscv_reg_info = static_cast<const RISCVRegisterInfo*>(reg_info);
    return riscv_reg_info->getABIVersion();
}

bool RISCVInstructionSelector::selectBasicBlock(const BasicBlock& ir_bb, 
                                               MachineBasicBlock& machine_bb) {
    // TODO: Implement basic block instruction selection
    (void)ir_bb;        // Suppress unused parameter warning
    (void)machine_bb;   // Suppress unused parameter warning
    return true;
}

bool RISCVInstructionSelector::selectInstruction(const Instruction& ir_inst,
                                                MachineBasicBlock& machine_bb) {
    // TODO: Implement single instruction selection
    (void)ir_inst;      // Suppress unused parameter warning
    (void)machine_bb;   // Suppress unused parameter warning
    return true;
}

unsigned RISCVInstructionSelector::getRegisterForValue(const Value* ir_value) {
    // TODO: Implement register allocation for IR values
    (void)ir_value;     // Suppress unused parameter warning
    return createVirtualRegister();  // For now, create a new virtual register
}

unsigned RISCVInstructionSelector::createVirtualRegister() {
    return next_virtual_reg_++;
}

} // namespace RISCV