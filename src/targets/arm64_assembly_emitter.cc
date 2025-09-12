// arm64_assembly_emitter.cc - Implementation of ARM64 assembly emission
#include "arm64_assembly_emitter.h"
#include "arm64_target.h"

#include <cassert>
#include <sstream>
#include <iomanip>

namespace ARM64 {

//===----------------------------------------------------------------------===//
// ARM64AssemblyEmitter Implementation
//===----------------------------------------------------------------------===//

ARM64AssemblyEmitter::ARM64AssemblyEmitter(const TargetRegisterInfo* reg_info,
                                           const TargetInstInfo* inst_info)
    : register_info_(reg_info)
    , inst_info_(inst_info)
    , abi_version_(determineABIVersion(reg_info))
    , current_function_(nullptr)
{
    assert(reg_info && "Register info cannot be null");
    assert(inst_info && "Instruction info cannot be null");
}

std::string ARM64AssemblyEmitter::emit(const MachineModule& module) {
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

const char* ARM64AssemblyEmitter::getTargetName() const {
    switch (abi_version_) {
        case ABIVersion::AAPCS64:
            return "arm64-aapcs";
        case ABIVersion::DARWIN:
            return "arm64-apple-darwin";
        default:
            return "arm64-unknown";
    }
}

bool ARM64AssemblyEmitter::supportsFeature(const std::string& feature) const {
    // Basic ARM64 features
    if (feature == "arm64") return true;
    if (feature == "aarch64") return true;
    if (feature == "assembly-emission") return true;
    if (feature == "gas-syntax") return true;  // GNU assembler syntax
    
    // ABI-specific features
    if (feature == "aapcs64") {
        return abi_version_ == ABIVersion::AAPCS64;
    }
    if (feature == "darwin") {
        return abi_version_ == ABIVersion::DARWIN;
    }
    
    // ARM64 ISA features
    if (feature == "neon") return true;      // SIMD/FP support
    if (feature == "crypto") return false;   // Crypto extensions (not implemented)
    if (feature == "crc") return false;      // CRC extensions (not implemented)
    
    return false;
}

size_t ARM64AssemblyEmitter::estimateOutputSize(const MachineModule& module) const {
    // ARM64 specific size estimation
    size_t base_size = codegen::AssemblyEmitter::estimateOutputSize(module);
    
    // Add overhead for directives, labels, and ARM64 instruction formatting
    size_t directive_overhead = 300;  // Rough estimate for .text, .globl, etc.
    size_t per_function_overhead = 80; // Function labels and prologue/epilogue
    
    return base_size + directive_overhead + 
           (module.functions().size() * per_function_overhead);
}

//===----------------------------------------------------------------------===//
// Private Helper Methods
//===----------------------------------------------------------------------===//

std::string ARM64AssemblyEmitter::emitFunction(const MachineFunction& function) {
    std::stringstream func_output;
    current_function_ = &function;
    
    // Get function name from IR
    std::string func_name = "unknown_function";
    if (function.ir_func()) {
        func_name = function.ir_func()->name();
        // Add underscore prefix for Darwin ABI
        if (abi_version_ == ABIVersion::DARWIN) {
            func_name = "_" + func_name;
        }
    }
    
    // Function label and directives
    func_output << "\n.text\n";
    func_output << ".globl " << func_name << "\n";
    
    // Platform-specific function attributes
    if (abi_version_ == ABIVersion::DARWIN) {
        func_output << ".p2align 2\n";
    } else {
        func_output << ".align 4\n";
    }
    
    func_output << func_name << ":\n";
    
    // Emit basic blocks
    for (const auto& bb : function.basic_blocks()) {
        func_output << emitBasicBlock(*bb);
    }
    
    current_function_ = nullptr;
    return func_output.str();
}

std::string ARM64AssemblyEmitter::emitBasicBlock(const MachineBasicBlock& bb) {
    std::stringstream bb_output;
    
    // Basic block label (if needed) - skip "entry" to avoid conflicts
    if (!bb.label().empty() && bb.label() != "entry") {
        bb_output << bb.label() << ":\n";
    }
    
    // Emit instructions
    for (const auto& inst : bb.instructions()) {
        bb_output << emitInstruction(*inst);
    }
    
    return bb_output.str();
}

std::string ARM64AssemblyEmitter::emitInstruction(const MachineInst& inst) {
    std::stringstream inst_output;
    const char* mnemonic = inst_info_->opcode_name(inst.opcode());
    
    // Handle special cases
    Opcode opcode = static_cast<Opcode>(inst.opcode());
    
    if (opcode == COMMENT) {
        // Comment pseudo-instruction
        if (!inst.operands().empty() && inst.operands()[0].is_imm()) {
            inst_output << "    // Comment\n";
        }
        return inst_output.str();
    }
    
    if (opcode == NOP) {
        inst_output << "    nop\n";
        return inst_output.str();
    }
    
    // Special handling for memory operations
    const auto& operands = inst.operands();
    
    if (opcode == STP && operands.size() >= 3) {
        // ARM64 store pair - special handling for stack operations
        if (operands[2].is_mem_ri()) {
            auto mem_op = operands[2].mem_ri();
            if (mem_op.base_reg == SP && mem_op.offset == -16) {
                // Prologue: stp x29, x30, [sp, #-16]!
                inst_output << "    stp " << formatOperand(operands[0]) << ", " 
                           << formatOperand(operands[1]) << ", [sp, #-16]!\n";
                return inst_output.str();
            }
        }
        // Default STP format
        inst_output << "    stp " << formatOperand(operands[0]) << ", " 
                   << formatOperand(operands[1]) << ", " << formatOperand(operands[2]) << "\n";
        return inst_output.str();
    }
    
    if (opcode == LDP && operands.size() >= 3) {
        // ARM64 load pair - special handling for stack operations  
        if (operands[2].is_mem_ri()) {
            auto mem_op = operands[2].mem_ri();
            if (mem_op.base_reg == SP && mem_op.offset == 16) {
                // Epilogue: ldp x29, x30, [sp], #16
                inst_output << "    ldp " << formatOperand(operands[0]) << ", " 
                           << formatOperand(operands[1]) << ", [sp], #16\n";
                return inst_output.str();
            }
        }
        // Default LDP format
        inst_output << "    ldp " << formatOperand(operands[0]) << ", " 
                   << formatOperand(operands[1]) << ", " << formatOperand(operands[2]) << "\n";
        return inst_output.str();
    }

    if (opcode == STR && operands.size() >= 2) {
        // ARM64 store: str src, [dst]
        inst_output << "    str " << formatOperand(operands[0]) << ", [" << formatOperand(operands[1]) << "]\n";
        return inst_output.str();
    }
    
    if (opcode == LDR && operands.size() >= 2) {
        // ARM64 load: ldr dst, [src]
        inst_output << "    ldr " << formatOperand(operands[0]) << ", [" << formatOperand(operands[1]) << "]\n";
        return inst_output.str();
    }
    
    if (opcode == MOV_IMM && operands.size() >= 2) {
        // ARM64 move: mov dst, src (use "mov" instead of "add dst, src, #0")
        inst_output << "    mov " << formatOperand(operands[0]) << ", " << formatOperand(operands[1]) << "\n";
        return inst_output.str();
    }
    
    // Regular instruction formatting
    inst_output << "    " << mnemonic;
    
    // Add operands
    for (size_t i = 0; i < operands.size(); ++i) {
        if (i == 0) {
            inst_output << " ";
        } else {
            inst_output << ", ";
        }
        inst_output << formatOperand(operands[i]);
    }
    
    inst_output << "\n";
    return inst_output.str();
}

std::string ARM64AssemblyEmitter::emitDirectives() {
    std::stringstream directives;
    
    // Platform-specific directives
    if (abi_version_ == ABIVersion::DARWIN) {
        directives << ".section __TEXT,__text,regular,pure_instructions\n";
        directives << ".build_version macos, 11, 0\n";
    } else {
        directives << ".text\n";
        directives << ".arch armv8-a\n";
    }
    
    return directives.str();
}

std::string ARM64AssemblyEmitter::emitGlobalLabels(const MachineModule& module) {
    std::stringstream globals;
    
    // Export all function symbols
    for (const auto& function : module.functions()) {
        if (function->ir_func()) {
            std::string func_name = function->ir_func()->name();
            // Add underscore prefix for Darwin ABI
            if (abi_version_ == ABIVersion::DARWIN) {
                func_name = "_" + func_name;
            }
            globals << ".globl " << func_name << "\n";
        }
    }
    
    return globals.str();
}

std::string ARM64AssemblyEmitter::formatRegister(unsigned reg) const {
    // Handle virtual registers (>= 1000) - look up allocated physical register
    if (reg >= 1000) {
        if (current_function_ && current_function_->are_registers_allocated()) {
            // Get the register allocator that contains the mappings
            if (auto* reg_alloc = current_function_->get_register_allocator()) {
                // Look up the assigned physical register
                if (auto assigned_reg = reg_alloc->get_assigned_reg(reg)) {
                    // Use the allocated physical register
                    unsigned phys_reg = *assigned_reg;
                    return formatRegister(phys_reg); // Recursive call with physical register
                }
            }
        }
        return "vreg" + std::to_string(reg);
    }
    
    // Cast to ARM64RegisterInfo to access getRegisterName
    if (const auto* arm64_reg_info = dynamic_cast<const ARM64RegisterInfo*>(register_info_)) {
        return arm64_reg_info->getRegisterName(reg);
    }
    
    return "unknown_reg" + std::to_string(reg);
}

std::string ARM64AssemblyEmitter::formatOperand(const MOperand& operand) const {
    if (operand.is_reg()) {
        return formatRegister(operand.reg());
    } else if (operand.is_imm()) {
        return formatImmediate(operand.imm());
    } else if (operand.is_external_sym()) {
        return operand.external_sym();
    } else if (operand.is_label()) {
        std::string label = operand.label();
        // Add underscore prefix for Darwin ABI function calls
        if (abi_version_ == ABIVersion::DARWIN) {
            label = "_" + label;
        }
        return label;
    } else if (operand.is_mem_ri()) {
        // ARM64 memory operand: [base, #offset]
        auto mem_operand = operand.mem_ri();
        std::string base = formatRegister(mem_operand.base_reg);
        if (mem_operand.offset == 0) {
            return "[" + base + "]";
        } else {
            return "[" + base + ", #" + std::to_string(mem_operand.offset) + "]";
        }
    } else {
        return "UNKNOWN_OPERAND";
    }
}

std::string ARM64AssemblyEmitter::formatImmediate(int64_t imm) const {
    return "#" + std::to_string(imm);
}

std::string ARM64AssemblyEmitter::formatMemoryOperand(const MOperand& base, const MOperand& offset) const {
    std::string result = "[";
    
    if (base.is_reg()) {
        result += formatRegister(base.reg());
    }
    
    if (offset.is_imm() && offset.imm() != 0) {
        result += ", #" + std::to_string(offset.imm());
    } else if (offset.is_reg()) {
        result += ", " + formatRegister(offset.reg());
    }
    
    result += "]";
    return result;
}

std::string ARM64AssemblyEmitter::formatBranchTarget(const std::string& label) const {
    // ARM64 branch targets are just labels
    return label;
}

std::string ARM64AssemblyEmitter::formatConditionCode(Opcode cond_op) const {
    switch (cond_op) {
    case BEQ: return "eq";
    case BNE: return "ne";
    case BLT: return "lt";
    case BGE: return "ge";
    case BGT: return "gt";
    case BLE: return "le";
    default:  return "unknown";
    }
}

std::string ARM64AssemblyEmitter::formatShiftOperand(const MOperand& operand, 
                                                    unsigned shift_type, 
                                                    unsigned shift_amount) const {
    std::string result = formatOperand(operand);
    
    if (shift_amount > 0) {
        const char* shift_name = "lsl";  // Default to logical shift left
        switch (shift_type) {
        case 0: shift_name = "lsl"; break;  // Logical shift left
        case 1: shift_name = "lsr"; break;  // Logical shift right  
        case 2: shift_name = "asr"; break;  // Arithmetic shift right
        case 3: shift_name = "ror"; break;  // Rotate right
        }
        result += ", " + std::string(shift_name) + " #" + std::to_string(shift_amount);
    }
    
    return result;
}

ABIVersion ARM64AssemblyEmitter::determineABIVersion(const TargetRegisterInfo* reg_info) {
    // Try to cast to ARM64RegisterInfo to get ABI version
    if (const auto* arm64_reg_info = dynamic_cast<const ARM64RegisterInfo*>(reg_info)) {
        return arm64_reg_info->getABIVersion();
    }
    
    // Default to Darwin ABI for Apple Silicon
    return ABIVersion::DARWIN;
}

bool ARM64AssemblyEmitter::isLargeImmediate(int64_t imm, Opcode opcode) const {
    // ARM64 immediate encoding limits
    switch (opcode) {
    case ADD_IMM:
    case SUB_IMM:
    case CMP_IMM:
        // 12-bit immediate (0-4095) with optional shift
        return (imm < 0 || imm > 4095);
        
    case MOV_IMM:
        // 16-bit immediate with shift (MOVZ/MOVK pattern)
        return (imm < -65536 || imm > 65535);
        
    default:
        // Conservative: treat as large if outside 12-bit range
        return (imm < 0 || imm > 4095);
    }
}

std::string ARM64AssemblyEmitter::getABIDirectives() const {
    switch (abi_version_) {
    case ABIVersion::DARWIN:
        return ".section __TEXT,__text,regular,pure_instructions\n"
               ".build_version macos, 11, 0\n";
    case ABIVersion::AAPCS64:
        return ".text\n"
               ".arch armv8-a\n";
    default:
        return ".text\n";
    }
}

} // namespace ARM64