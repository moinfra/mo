// riscv_assembly_emitter.cc - Implementation of RISC-V assembly emission
#include "riscv_assembly_emitter.h"
#include "riscv_target.h"

#include <cassert>
#include <sstream>

namespace RISCV {

//===----------------------------------------------------------------------===//
// RISCVAssemblyEmitter Implementation
//===----------------------------------------------------------------------===//

RISCVAssemblyEmitter::RISCVAssemblyEmitter(const TargetRegisterInfo* reg_info,
                                           const TargetInstInfo* inst_info)
    : register_info_(reg_info)
    , inst_info_(inst_info)
    , abi_version_(determineABIVersion(reg_info))
{
    assert(reg_info && "Register info cannot be null");
    assert(inst_info && "Instruction info cannot be null");
}

std::string RISCVAssemblyEmitter::emit(const MachineModule& module) {
    std::stringstream asm_output;
    
    // Emit assembly directives and metadata
    asm_output << emitDirectives();
    asm_output << emitGlobalLabels(module);
    
    // Emit each function
    for (const auto& function : module.functions()) {
        asm_output << emitFunction(*function);
        asm_output << "\n";  // Extra newline between functions
    }
    
    return asm_output.str();
}

const char* RISCVAssemblyEmitter::getTargetName() const {
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

bool RISCVAssemblyEmitter::supportsFeature(const std::string& feature) const {
    // Basic RISC-V features
    if (feature == "riscv") return true;
    if (feature == "assembly-emission") return true;
    if (feature == "gas-syntax") return true;  // GNU assembler syntax
    
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

size_t RISCVAssemblyEmitter::estimateOutputSize(const MachineModule& module) const {
    // RISC-V specific size estimation
    size_t base_size = codegen::AssemblyEmitter::estimateOutputSize(module);
    
    // Add overhead for directives, labels, and RISC-V instruction formatting
    size_t directive_overhead = 200;  // Rough estimate for .text, .globl, etc.
    size_t per_function_overhead = 50;  // Function labels and basic formatting
    
    return base_size + directive_overhead + 
           (module.functions().size() * per_function_overhead);
}

//===----------------------------------------------------------------------===//
// Private Helper Methods
//===----------------------------------------------------------------------===//

std::string RISCVAssemblyEmitter::emitDirectives() {
    std::stringstream directives;
    
    // Standard RISC-V assembly directives
    directives << ".text\n";
    directives << ".arch rv64gc\n";  // RISC-V 64-bit with standard extensions
    directives << ".file \"generated.s\"\n";
    directives << "\n";
    
    return directives.str();
}

std::string RISCVAssemblyEmitter::emitGlobalLabels(const MachineModule& module) {
    std::stringstream labels;
    
    // Emit global labels for all functions
    for (const auto& function : module.functions()) {
        // TODO: Get function name from IR function
        // For now, emit a placeholder
        labels << ".globl function_" << function.get() << "\n";
    }
    
    if (!module.functions().empty()) {
        labels << "\n";
    }
    
    return labels.str();
}

std::string RISCVAssemblyEmitter::emitFunction(const MachineFunction& function) {
    std::stringstream func_asm;
    
    // Function label
    func_asm << "function_" << &function << ":\n";
    
    // Emit each basic block
    for (const auto& bb : function.basic_blocks()) {
        func_asm << emitBasicBlock(*bb);
    }
    
    return func_asm.str();
}

std::string RISCVAssemblyEmitter::emitBasicBlock(const MachineBasicBlock& bb) {
    std::stringstream bb_asm;
    
    // Basic block label (if not the first block)
    if (!bb.label().empty() && bb.label() != "entry") {
        bb_asm << bb.label() << ":\n";
    }
    
    // Emit each instruction in the basic block
    for (const auto& inst : bb.instructions()) {
        bb_asm << "    " << emitInstruction(*inst) << "\n";
    }
    
    return bb_asm.str();
}

std::string RISCVAssemblyEmitter::emitInstruction(const MachineInst& inst) {
    // Phase 1.5 - Commit 5.3: Real RISC-V assembly emission
    auto opcode = static_cast<RISCV::Opcode>(inst.opcode());
    
    switch (opcode) {
        case RISCV::ADD: {
            // add rd, rs1, rs2
            if (inst.operands().size() >= 3) {
                return "add " + formatOperand(inst.operands()[0]) + ", " +
                              formatOperand(inst.operands()[1]) + ", " +
                              formatOperand(inst.operands()[2]);
            }
            return "# malformed add instruction";
        }
        
        case RISCV::SUB: {
            // sub rd, rs1, rs2
            if (inst.operands().size() >= 3) {
                return "sub " + formatOperand(inst.operands()[0]) + ", " +
                              formatOperand(inst.operands()[1]) + ", " +
                              formatOperand(inst.operands()[2]);
            }
            return "# malformed sub instruction";
        }
        
        case RISCV::MUL: {
            // mul rd, rs1, rs2
            if (inst.operands().size() >= 3) {
                return "mul " + formatOperand(inst.operands()[0]) + ", " +
                              formatOperand(inst.operands()[1]) + ", " +
                              formatOperand(inst.operands()[2]);
            }
            return "# malformed mul instruction";
        }
        
        case RISCV::DIV: {
            // div rd, rs1, rs2
            if (inst.operands().size() >= 3) {
                return "div " + formatOperand(inst.operands()[0]) + ", " +
                              formatOperand(inst.operands()[1]) + ", " +
                              formatOperand(inst.operands()[2]);
            }
            return "# malformed div instruction";
        }
        
        case RISCV::ADDI: {
            // addi rd, rs1, imm
            if (inst.operands().size() >= 3) {
                return "addi " + formatOperand(inst.operands()[0]) + ", " +
                               formatOperand(inst.operands()[1]) + ", " +
                               formatOperand(inst.operands()[2]);
            }
            return "# malformed addi instruction";
        }
        
        case RISCV::LW: {
            // lw rd, offset(rs1)
            if (inst.operands().size() >= 2) {
                return "lw " + formatOperand(inst.operands()[0]) + ", " +
                             formatOperand(inst.operands()[1]);
            }
            return "# malformed lw instruction";
        }
        
        case RISCV::SW: {
            // sw rs2, offset(rs1) 
            if (inst.operands().size() >= 2) {
                return "sw " + formatOperand(inst.operands()[0]) + ", " +
                             formatOperand(inst.operands()[1]);
            }
            return "# malformed sw instruction";
        }
        
        case RISCV::JALR: {
            // jalr rd, rs1, offset
            if (inst.operands().size() >= 3) {
                auto& rd = inst.operands()[0];
                auto& rs1 = inst.operands()[1]; 
                auto& offset = inst.operands()[2];
                
                // Special case: ret is jalr x0, ra, 0
                if (rd.is_reg() && rd.reg() == static_cast<unsigned>(RISCV::ZERO) &&
                    rs1.is_reg() && rs1.reg() == static_cast<unsigned>(RISCV::RA) &&
                    offset.is_imm() && offset.imm() == 0) {
                    return "ret";
                }
                
                return "jalr " + formatOperand(rd) + ", " +
                               formatOperand(rs1) + ", " + 
                               formatOperand(offset);
            }
            return "# malformed jalr instruction";
        }
        
        case RISCV::COMMENT: {
            // Comment pseudo-instruction
            if (inst.operands().size() >= 1) {
                return formatOperand(inst.operands()[0]);
            }
            return "# empty comment";
        }
        
        default:
            return "# unknown opcode: " + std::to_string(inst.opcode());
    }
}

std::string RISCVAssemblyEmitter::formatRegister(unsigned reg) const {
    // Phase 1.5 - Commit 5.3: Real RISC-V register formatting
    
    // Handle virtual registers (>= 1000)
    if (reg >= 1000) {
        return "vreg" + std::to_string(reg);
    }
    
    // Handle RISC-V physical registers
    switch (reg) {
        // Zero register
        case static_cast<unsigned>(RISCV::ZERO): return "zero";
        
        // Return address
        case static_cast<unsigned>(RISCV::RA):   return "ra";
        
        // Stack pointer
        case static_cast<unsigned>(RISCV::SP):   return "sp";
        
        // Global pointer  
        case static_cast<unsigned>(RISCV::GP):   return "gp";
        
        // Thread pointer
        case static_cast<unsigned>(RISCV::TP):   return "tp";
        
        // Argument/return registers
        case static_cast<unsigned>(RISCV::A0):   return "a0";
        case static_cast<unsigned>(RISCV::A1):   return "a1";
        case static_cast<unsigned>(RISCV::A2):   return "a2";
        case static_cast<unsigned>(RISCV::A3):   return "a3";
        case static_cast<unsigned>(RISCV::A4):   return "a4";
        case static_cast<unsigned>(RISCV::A5):   return "a5";
        case static_cast<unsigned>(RISCV::A6):   return "a6";
        case static_cast<unsigned>(RISCV::A7):   return "a7";
        
        // Saved registers
        case static_cast<unsigned>(RISCV::S0):   return "s0";
        case static_cast<unsigned>(RISCV::S1):   return "s1";
        case static_cast<unsigned>(RISCV::S2):   return "s2";
        case static_cast<unsigned>(RISCV::S3):   return "s3";
        case static_cast<unsigned>(RISCV::S4):   return "s4";
        case static_cast<unsigned>(RISCV::S5):   return "s5";
        case static_cast<unsigned>(RISCV::S6):   return "s6";
        case static_cast<unsigned>(RISCV::S7):   return "s7";
        case static_cast<unsigned>(RISCV::S8):   return "s8";
        case static_cast<unsigned>(RISCV::S9):   return "s9";
        case static_cast<unsigned>(RISCV::S10):  return "s10";
        case static_cast<unsigned>(RISCV::S11):  return "s11";
        
        // Temporary registers
        case static_cast<unsigned>(RISCV::T0):   return "t0";
        case static_cast<unsigned>(RISCV::T1):   return "t1";
        case static_cast<unsigned>(RISCV::T2):   return "t2";
        case static_cast<unsigned>(RISCV::T3):   return "t3";
        case static_cast<unsigned>(RISCV::T4):   return "t4";
        case static_cast<unsigned>(RISCV::T5):   return "t5";
        case static_cast<unsigned>(RISCV::T6):   return "t6";
        
        // Default: use x notation for any other register
        default:
            return "x" + std::to_string(reg);
    }
}

std::string RISCVAssemblyEmitter::formatOperand(const MOperand& operand) const {
    // Phase 1.5 - Commit 5.3: Real operand formatting
    if (operand.is_reg()) {
        return formatRegister(operand.reg());
    } else if (operand.is_imm()) {
        return std::to_string(operand.imm());
    } else if (operand.is_external_sym()) {
        return operand.external_sym();
    } else if (operand.is_mem_ri()) {
        auto mem_operand = operand.mem_ri();
        return std::to_string(mem_operand.offset) + "(" + 
               formatRegister(mem_operand.base_reg) + ")";
    } else {
        return "# unknown operand type";
    }
}

std::string RISCVAssemblyEmitter::formatImmediate(int64_t imm) const {
    return std::to_string(imm);
}

ABIVersion RISCVAssemblyEmitter::determineABIVersion(const TargetRegisterInfo* reg_info) {
    // Cast to RISC-V specific register info and get ABI version
    const auto* riscv_reg_info = static_cast<const RISCVRegisterInfo*>(reg_info);
    return riscv_reg_info->getABIVersion();
}

} // namespace RISCV