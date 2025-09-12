#include "arm64_target.h"
#include <cassert>
#include <sstream>

namespace ARM64 {

//===----------------------------------------------------------------------===//
// ARM64RegisterInfo Implementation
//===----------------------------------------------------------------------===//

ARM64RegisterInfo::ARM64RegisterInfo(ABIVersion abi) : TargetRegisterInfo(128), abi_version_(abi) {
    // Initialize register descriptors
    reg_descs_.resize(128); // X0-X31, W0-W31, SP, V0-V31
    
    // General Purpose Registers (X0-X30)
    for (unsigned i = X0; i <= X30; ++i) {
        reg_descs_[i] = {
            .spill_cost = 1,
            .is_reserved = (i == X18), // X18 is platform register on some systems
            .is_allocatable = (i != X18),
            .primary_rc_id = GPR64,
            .rc_mask = {0}
        };
        reg_descs_[i].rc_mask.set(GPR64);
    }
    
    // XZR (zero register) - not allocatable
    reg_descs_[XZR] = {
        .spill_cost = 0,
        .is_reserved = true,
        .is_allocatable = false,
        .primary_rc_id = GPR64,
        .rc_mask = {0}
    };
    
    // 32-bit W registers (W0-W30)
    for (unsigned i = W0; i <= W30; ++i) {
        unsigned x_reg = i - W0; // Corresponding X register
        reg_descs_[i] = {
            .spill_cost = 1,
            .is_reserved = (x_reg == X18),
            .is_allocatable = (x_reg != X18),
            .primary_rc_id = GPR32,
            .rc_mask = {0}
        };
        reg_descs_[i].rc_mask.set(GPR32);
    }
    
    // WZR (zero register) - not allocatable
    reg_descs_[WZR] = {
        .spill_cost = 0,
        .is_reserved = true,
        .is_allocatable = false,
        .primary_rc_id = GPR32,
        .rc_mask = {0}
    };
    
    // Stack Pointer - reserved
    reg_descs_[SP] = {
        .spill_cost = 0,
        .is_reserved = true,
        .is_allocatable = false,
        .primary_rc_id = GPR64,
        .rc_mask = {0}
    };
    
    // SIMD/FP registers (V0-V31)
    for (unsigned i = V0; i <= V31; ++i) {
        reg_descs_[i] = {
            .spill_cost = 2, // Higher cost for SIMD registers
            .is_reserved = false,
            .is_allocatable = true,
            .primary_rc_id = SIMD,
            .rc_mask = {0}
        };
        reg_descs_[i].rc_mask.set(SIMD);
    }
}

const char* ARM64RegisterInfo::getRegisterName(unsigned reg) const {
    if (reg >= reg_descs_.size()) return "INVALID";
    
    // X registers (64-bit)
    if (reg >= X0 && reg <= X30) {
        static const char* x_names[] = {
            "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
            "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
            "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
            "x24", "x25", "x26", "x27", "x28", "x29", "x30"
        };
        return x_names[reg - X0];
    }
    
    if (reg == XZR) return "xzr";
    
    // W registers (32-bit)
    if (reg >= W0 && reg <= W30) {
        static const char* w_names[] = {
            "w0", "w1", "w2", "w3", "w4", "w5", "w6", "w7",
            "w8", "w9", "w10", "w11", "w12", "w13", "w14", "w15",
            "w16", "w17", "w18", "w19", "w20", "w21", "w22", "w23",
            "w24", "w25", "w26", "w27", "w28", "w29", "w30"
        };
        return w_names[reg - W0];
    }
    
    if (reg == WZR) return "wzr";
    if (reg == SP) return "sp";
    
    // SIMD/FP registers
    if (reg >= V0 && reg <= V31) {
        static const char* v_names[] = {
            "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7",
            "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15",
            "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",
            "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31"
        };
        return v_names[reg - V0];
    }
    
    return "UNKNOWN";
}

std::vector<unsigned> ARM64RegisterInfo::get_allocatable_regs(unsigned reg_class_id) const {
    std::vector<unsigned> regs;
    
    switch (reg_class_id) {
    case GPR64:
        // X0-X17, X19-X30 (skip X18 which may be reserved)
        for (unsigned i = X0; i <= X30; ++i) {
            if (reg_descs_[i].is_allocatable) {
                regs.push_back(i);
            }
        }
        break;
        
    case GPR32:
        // W0-W17, W19-W30 (skip W18 which may be reserved)
        for (unsigned i = W0; i <= W30; ++i) {
            if (reg_descs_[i].is_allocatable) {
                regs.push_back(i);
            }
        }
        break;
        
    case SIMD:
        // V0-V31
        for (unsigned i = V0; i <= V31; ++i) {
            regs.push_back(i);
        }
        break;
    }
    
    return regs;
}

std::vector<unsigned> ARM64RegisterInfo::get_allocation_order(unsigned reg_class_id) const {
    auto regs = get_allocatable_regs(reg_class_id);
    
    // ARM64 calling convention priority:
    // - Caller-saved registers first (X0-X17, except argument registers for parameters)
    // - Callee-saved registers second (X19-X30)
    
    if (reg_class_id == GPR64) {
        std::vector<unsigned> ordered;
        
        // Temporary registers first (X9-X17)
        for (unsigned i = X9; i <= X17 && i != X18; ++i) {
            if (reg_descs_[i].is_allocatable) {
                ordered.push_back(i);
            }
        }
        
        // Argument registers that can be reused (X2-X7)
        for (unsigned i = X2; i <= X7; ++i) {
            if (reg_descs_[i].is_allocatable) {
                ordered.push_back(i);
            }
        }
        
        // Callee-saved registers (X19-X28)
        for (unsigned i = X19; i <= X28; ++i) {
            if (reg_descs_[i].is_allocatable) {
                ordered.push_back(i);
            }
        }
        
        return ordered;
    }
    
    return regs;
}

std::vector<unsigned> ARM64RegisterInfo::get_temp_regs(CallingConv::ID cc, unsigned reg_class_id) const {
    std::vector<unsigned> temps;
    
    if (reg_class_id == GPR64) {
        // Caller-saved temporary registers
        temps = {X9, X10, X11, X12, X13, X14, X15, X16, X17};
    } else if (reg_class_id == GPR32) {
        temps = {W9, W10, W11, W12, W13, W14, W15, W16, W17};
    }
    
    return temps;
}

bool ARM64RegisterInfo::isGPR(unsigned reg) const {
    return (reg >= X0 && reg <= XZR) || (reg >= W0 && reg <= WZR) || reg == SP;
}

bool ARM64RegisterInfo::isX64Register(unsigned reg) const {
    return (reg >= X0 && reg <= XZR) || reg == SP;
}

bool ARM64RegisterInfo::isW32Register(unsigned reg) const {
    return (reg >= W0 && reg <= WZR);
}

bool ARM64RegisterInfo::isSIMDRegister(unsigned reg) const {
    return reg >= V0 && reg <= V31;
}

//===----------------------------------------------------------------------===//
// ARM64TargetInstInfo Implementation
//===----------------------------------------------------------------------===//

ARM64TargetInstInfo::ARM64TargetInstInfo(ABIVersion abi) : abi_version_(abi) {
}

const char* ARM64TargetInstInfo::opcode_name(unsigned opcode) const {
    switch (static_cast<Opcode>(opcode)) {
    case ADD: return "add";
    case ADDS: return "adds";
    case SUB: return "sub";
    case SUBS: return "subs";
    case MUL: return "mul";
    case UDIV: return "udiv";
    case SDIV: return "sdiv";
    case ADD_IMM: return "add";
    case SUB_IMM: return "sub";
    case MOV_IMM: return "mov";
    case MOVZ: return "movz";
    case MOVK: return "movk";
    case AND: return "and";
    case ORR: return "orr";
    case EOR: return "eor";
    case LSL: return "lsl";
    case LSR: return "lsr";
    case ASR: return "asr";
    case LDR: return "ldr";
    case LDRB: return "ldrb";
    case LDRH: return "ldrh";
    case STR: return "str";
    case STRB: return "strb";
    case STRH: return "strh";
    case LDP: return "ldp";
    case STP: return "stp";
    case B: return "b";
    case BL: return "bl";
    case BR: return "br";
    case BLR: return "blr";
    case RET: return "ret";
    case BEQ: return "b.eq";
    case BNE: return "b.ne";
    case BLT: return "b.lt";
    case BGE: return "b.ge";
    case BGT: return "b.gt";
    case BLE: return "b.le";
    case CMP: return "cmp";
    case CMP_IMM: return "cmp";
    case NOP: return "nop";
    case COMMENT: return "//";
    default: return "UNKNOWN";
    }
}

unsigned ARM64TargetInstInfo::get_inst_size(const MachineInst& mi) const {
    // All ARM64 instructions are 4 bytes (32 bits)
    return 4;
}

bool ARM64TargetInstInfo::verify_instruction(const MachineInst& mi, std::string& error_msg) const {
    // Basic verification - could be expanded
    if (mi.opcode() >= 0x2000) {
        error_msg = "Invalid ARM64 opcode";
        return false;
    }
    return true;
}

// Stub implementations for pure virtual methods
uint32_t ARM64TargetInstInfo::get_binary_encoding(const MachineInst &mi) const {
    return 0; // Stub
}

void ARM64TargetInstInfo::expand_pseudo(MachineBasicBlock &mbb, MachineBasicBlock::iterator mi) const {
    // Stub - no pseudo instructions to expand
}

bool ARM64TargetInstInfo::is_return(const MachineInst &mi) const {
    return mi.opcode() == RET;
}

bool ARM64TargetInstInfo::is_call(const MachineInst &mi) const {
    return mi.opcode() == BL || mi.opcode() == BLR;
}

bool ARM64TargetInstInfo::is_legal_immediate(int64_t immediate, unsigned operand_size) const {
    // Basic ARM64 immediate legality check
    return (immediate >= -2048 && immediate <= 2047);
}

bool ARM64TargetInstInfo::is_operand_def(unsigned op, unsigned index) const {
    return index == 0; // First operand is typically the destination
}

bool ARM64TargetInstInfo::is_operand_use(unsigned op, unsigned index) const {
    return index > 0; // Other operands are typically source
}

unsigned ARM64TargetInstInfo::get_instruction_latency(unsigned opcode) const {
    // Simple latency model
    switch (opcode) {
    case MUL:
        return 3;
    case UDIV:
    case SDIV:
        return 12;
    default:
        return 1;
    }
}

void ARM64TargetInstInfo::copy_phys_reg(MachineBasicBlock &mbb, MachineBasicBlock::iterator insert,
                                       unsigned dest_reg, unsigned src_reg) const {
    // Create a MOV instruction to copy registers
    auto mov_inst = std::make_unique<MachineInst>(ADD); // Use ADD with 0 immediate as MOV
    mov_inst->add_operand(MOperand::create_reg(dest_reg, true));
    mov_inst->add_operand(MOperand::create_reg(src_reg, false));
    mov_inst->add_operand(MOperand::create_imm(0));
    mbb.insert(insert, std::move(mov_inst));
}

bool ARM64TargetInstInfo::legalize_inst(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii,
                                       MachineFunction &mf) const {
    return false; // No legalization needed
}

MachineBasicBlock::iterator ARM64TargetInstInfo::insert_load_from_stack(MachineBasicBlock &mbb,
                                                                       MachineBasicBlock::iterator insert_point,
                                                                       unsigned dest_reg, int frame_index,
                                                                       int64_t offset) const {
    auto load_inst = std::make_unique<MachineInst>(LDR);
    load_inst->add_operand(MOperand::create_reg(dest_reg, true));
    load_inst->add_operand(MOperand::create_frame_index(frame_index));
    return mbb.insert(insert_point, std::move(load_inst));
}

MachineBasicBlock::iterator ARM64TargetInstInfo::insert_store_to_stack(MachineBasicBlock &mbb,
                                                                      MachineBasicBlock::iterator insert_point,
                                                                      unsigned src_reg, int frame_index,
                                                                      int64_t offset) const {
    auto store_inst = std::make_unique<MachineInst>(STR);
    store_inst->add_operand(MOperand::create_reg(src_reg, false));
    store_inst->add_operand(MOperand::create_frame_index(frame_index));
    return mbb.insert(insert_point, std::move(store_inst));
}

bool ARM64TargetInstInfo::analyze_branch(MachineBasicBlock &mbb, MachineInst *terminator,
                                        std::unordered_set<MachineBasicBlock *> &branch_targets,
                                        MachineBasicBlock *&fall_through) const {
    return false; // No branch analysis implemented
}

//===----------------------------------------------------------------------===//
// ARM64Target Implementation
//===----------------------------------------------------------------------===//

ARM64Target::ARM64Target(
    std::unique_ptr<ARM64RegisterInfo> reg_info,
    std::unique_ptr<ARM64TargetInstInfo> inst_info,
    ABIVersion abi
) : reg_info_(std::move(reg_info)), 
    inst_info_(std::move(inst_info)), 
    abi_version_(abi) {
}

const char* ARM64Target::getTargetName() const {
    switch (abi_version_) {
    case ABIVersion::AAPCS64: return "arm64-aapcs";
    case ABIVersion::DARWIN: return "arm64-apple-darwin";
    default: return "arm64-unknown";
    }
}

} // namespace ARM64