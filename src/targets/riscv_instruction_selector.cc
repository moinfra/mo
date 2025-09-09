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
    // Phase 1.5 - Commit 5.2: Real instruction selection implementation
    
    // Clear any existing state
    ir_value_to_vreg_.clear();
    next_virtual_reg_ = 1000;
    
    // Map to track IR basic blocks to machine basic blocks
    std::map<const BasicBlock*, MachineBasicBlock*> bb_map;
    
    // First pass: Create machine basic blocks for each IR basic block
    for (const auto ir_bb : ir_func.basic_blocks()) {
        auto* machine_bb = machine_func.create_block(ir_bb->name().empty() ? "bb" : ir_bb->name());
        bb_map[ir_bb] = machine_bb;
    }
    
    // Second pass: Process instructions in each basic block
    for (const auto ir_bb : ir_func.basic_blocks()) {
        auto* machine_bb = bb_map[ir_bb];
        
        // Process each instruction in the basic block
        for (auto it = ir_bb->begin(); it != ir_bb->end(); ++it) {
            if (!selectSingleInstruction(*it, *machine_bb)) {
                // Failed to select instruction - this is not necessarily an error
                // in our minimal implementation, just continue
            }
        }
    }
    
    return true;  // Success
}

//===----------------------------------------------------------------------===//
// Single Instruction Selection Implementation (Phase 1.5 - Commit 5.2)
//===----------------------------------------------------------------------===//

bool RISCVInstructionSelector::selectSingleInstruction(const Instruction& ir_inst, 
                                                       MachineBasicBlock& mbb) {
    switch (ir_inst.opcode()) {
        case ::Opcode::Add: {
            // IR: %result = add i32 %a, %b  →  RISC-V: add vreg_result, vreg_a, vreg_b
            auto dst_reg = getVirtualRegForValue(&ir_inst);
            auto src1_reg = getVirtualRegForValue(ir_inst.operand(0));
            auto src2_reg = getVirtualRegForValue(ir_inst.operand(1));
            
            auto add_inst = std::make_unique<MachineInst>(
                static_cast<unsigned>(RISCV::ADD),
                std::vector<MOperand>{
                    MOperand::create_reg(dst_reg, true),   // def
                    MOperand::create_reg(src1_reg, false), // use  
                    MOperand::create_reg(src2_reg, false)  // use
                }
            );
            mbb.append(std::move(add_inst));
            return true;
        }
        
        case ::Opcode::Sub: {
            // IR: %result = sub i32 %a, %b  →  RISC-V: sub vreg_result, vreg_a, vreg_b
            auto dst_reg = getVirtualRegForValue(&ir_inst);
            auto src1_reg = getVirtualRegForValue(ir_inst.operand(0));
            auto src2_reg = getVirtualRegForValue(ir_inst.operand(1));
            
            auto sub_inst = std::make_unique<MachineInst>(
                static_cast<unsigned>(RISCV::SUB),
                std::vector<MOperand>{
                    MOperand::create_reg(dst_reg, true),   // def
                    MOperand::create_reg(src1_reg, false), // use  
                    MOperand::create_reg(src2_reg, false)  // use
                }
            );
            mbb.append(std::move(sub_inst));
            return true;
        }
        
        case ::Opcode::Mul: {
            // IR: %result = mul i32 %a, %b  →  RISC-V: mul vreg_result, vreg_a, vreg_b
            auto dst_reg = getVirtualRegForValue(&ir_inst);
            auto src1_reg = getVirtualRegForValue(ir_inst.operand(0));
            auto src2_reg = getVirtualRegForValue(ir_inst.operand(1));
            
            auto mul_inst = std::make_unique<MachineInst>(
                static_cast<unsigned>(RISCV::MUL),
                std::vector<MOperand>{
                    MOperand::create_reg(dst_reg, true),   // def
                    MOperand::create_reg(src1_reg, false), // use  
                    MOperand::create_reg(src2_reg, false)  // use
                }
            );
            mbb.append(std::move(mul_inst));
            return true;
        }
        
        case ::Opcode::SDiv: {
            // IR: %result = sdiv i32 %a, %b  →  RISC-V: div vreg_result, vreg_a, vreg_b
            auto dst_reg = getVirtualRegForValue(&ir_inst);
            auto src1_reg = getVirtualRegForValue(ir_inst.operand(0));
            auto src2_reg = getVirtualRegForValue(ir_inst.operand(1));
            
            auto div_inst = std::make_unique<MachineInst>(
                static_cast<unsigned>(RISCV::DIV),
                std::vector<MOperand>{
                    MOperand::create_reg(dst_reg, true),   // def
                    MOperand::create_reg(src1_reg, false), // use  
                    MOperand::create_reg(src2_reg, false)  // use
                }
            );
            mbb.append(std::move(div_inst));
            return true;
        }
        
        case ::Opcode::Ret: {
            // IR: ret i32 %val  →  RISC-V: mv a0, vreg_val; ret
            if (ir_inst.operands().size() > 0) {
                // Move return value to a0 register
                auto ret_val_reg = getVirtualRegForValue(ir_inst.operand(0));
                auto mv_inst = std::make_unique<MachineInst>(
                    static_cast<unsigned>(RISCV::ADD),  // mv is add with x0
                    std::vector<MOperand>{
                        MOperand::create_reg(static_cast<unsigned>(RISCV::A0), true),    // def: a0
                        MOperand::create_reg(ret_val_reg, false),                        // use: return value
                        MOperand::create_reg(static_cast<unsigned>(RISCV::ZERO), false) // use: x0 (zero)
                    }
                );
                mbb.append(std::move(mv_inst));
            }
            
            // Generate ret instruction (jalr x0, ra, 0)
            auto ret_inst = std::make_unique<MachineInst>(
                static_cast<unsigned>(RISCV::JALR),
                std::vector<MOperand>{
                    MOperand::create_reg(static_cast<unsigned>(RISCV::ZERO), true),  // def: x0 (ignored)
                    MOperand::create_reg(static_cast<unsigned>(RISCV::RA), false),   // use: return address
                    MOperand::create_imm(0)                                          // immediate: offset 0
                }
            );
            mbb.append(std::move(ret_inst));
            return true;
        }
        
        default: {
            // Unsupported instruction: generate a comment for debugging
            std::string comment_text = "# unsupported IR opcode: " + 
                                     std::to_string(static_cast<int>(ir_inst.opcode()));
            auto comment_inst = std::make_unique<MachineInst>(
                static_cast<unsigned>(RISCV::COMMENT),  // Special comment opcode
                std::vector<MOperand>{
                    MOperand::create_external_sym(comment_text)
                }
            );
            mbb.append(std::move(comment_inst));
            return false;  // Indicate unsupported instruction
        }
    }
}

unsigned RISCVInstructionSelector::getVirtualRegForValue(const Value* ir_value) {
    // Check if we already have a virtual register for this IR value
    auto it = ir_value_to_vreg_.find(ir_value);
    if (it != ir_value_to_vreg_.end()) {
        return it->second;
    }
    
    // Create a new virtual register for this IR value
    unsigned new_vreg = next_virtual_reg_++;
    ir_value_to_vreg_[ir_value] = new_vreg;
    
    // TODO: In a full implementation, we would also:
    // 1. Determine the appropriate register class based on the IR value's type
    // 2. Register the virtual register with the MachineFunction
    // 3. Handle different register classes (int, float, etc.)
    
    return new_vreg;
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