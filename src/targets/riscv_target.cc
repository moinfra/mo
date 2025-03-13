
#include "riscv_target.h"

using namespace RISCV;

//===----------------------------------------------------------------------===//
// RegisterInfo Implementation
//===----------------------------------------------------------------------===//

RISCVRegisterInfo::RISCVRegisterInfo(ABIVersion abi)
    : TargetRegisterInfo(64)
{
    initializeRegisters(abi);
    initializeRegisterClasses(abi);
    initializeCallingConventions(abi);
}

// 初始化寄存器信息
void RISCVRegisterInfo::initializeRegisters(ABIVersion abi)
{
    // 初始化整型寄存器 (x0-x31)
    for (unsigned reg = Reg::ZERO; reg <= Reg::T6; ++reg)
    {
        reg_descs_[reg].spill_cost = 6;
        reg_descs_[reg].is_callee_saved = false;
        reg_descs_[reg].is_reserved = false;
        reg_descs_[reg].is_allocatable = true;
        reg_descs_[reg].primary_rc_id = 0; // 默认属于整型寄存器类
        reg_descs_[reg].rc_mask = 0x1;     // 属于整型寄存器类
    }

    // 标记特殊整型寄存器
    const std::vector<unsigned> reserved_int_regs = {
        Reg::ZERO, // 零寄存器
        Reg::SP,   // 栈指针
        Reg::GP,   // 全局指针
        Reg::TP    // 线程指针
    };

    for (auto reg : reserved_int_regs)
    {
        reg_descs_[reg].is_reserved = true;
        reg_descs_[reg].is_allocatable = false;
    }

    // 配置被调用者保存的整型寄存器 (s0-s11)
    const std::vector<unsigned> callee_saved_int = {
        Reg::S0, Reg::S1,
        Reg::S2, Reg::S3, Reg::S4, Reg::S5,
        Reg::S6, Reg::S7, Reg::S8, Reg::S9,
        Reg::S10, Reg::S11};

    for (auto reg : callee_saved_int)
    {
        reg_descs_[reg].is_callee_saved = true;
        reg_descs_[reg].spill_cost = 10; // 更高的溢出代价
    }

    // 初始化浮点寄存器 (f0-f31)，根据ABI是否支持浮点
    bool has_float = (abi == ABIVersion::ILP32F || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D);

    if (has_float)
    {
        for (unsigned reg = Reg::F0; reg <= Reg::F31; ++reg)
        {
            reg_descs_[reg].spill_cost = 8;
            reg_descs_[reg].is_callee_saved = false;
            reg_descs_[reg].is_reserved = false;
            reg_descs_[reg].is_allocatable = true;
            reg_descs_[reg].primary_rc_id = 1; // 默认属于浮点寄存器类
            reg_descs_[reg].rc_mask = 0x2;     // 属于浮点寄存器类
        }

        // 配置被调用者保存的浮点寄存器 (fs0-fs11)
        const std::vector<unsigned> callee_saved_fp = {
            Reg::F8, Reg::F9,
            Reg::F18, Reg::F19, Reg::F20, Reg::F21,
            Reg::F22, Reg::F23, Reg::F24, Reg::F25,
            Reg::F26, Reg::F27};

        for (auto reg : callee_saved_fp)
        {
            reg_descs_[reg].is_callee_saved = true;
            reg_descs_[reg].spill_cost = 12;
        }
    }
    else
    {
        // 如果不支持浮点，则标记所有浮点寄存器为不可分配
        for (unsigned reg = Reg::F0; reg <= Reg::F31; ++reg)
        {
            reg_descs_[reg].is_reserved = true;
            reg_descs_[reg].is_allocatable = false;
        }
    }
}

// 初始化寄存器类
void RISCVRegisterInfo::initializeRegisterClasses(ABIVersion abi)
{
    // 定义整型寄存器类 (GR32)
    RegisterClass gr32_class = {
        GR32,
        "GR32", // 类名称
        {
            // 所有可分配的32位整型寄存器
            Reg::RA, Reg::SP, Reg::GP, Reg::TP,
            Reg::T0, Reg::T1, Reg::T2,
            Reg::S0, Reg::S1,
            Reg::A0, Reg::A1, Reg::A2, Reg::A3,
            Reg::A4, Reg::A5, Reg::A6, Reg::A7,
            Reg::S2, Reg::S3, Reg::S4, Reg::S5,
            Reg::S6, Reg::S7, Reg::S8, Reg::S9,
            Reg::S10, Reg::S11,
            Reg::T3, Reg::T4, Reg::T5, Reg::T6},
        1, // 拷贝代价
        1  // 寄存器压力权重
    };

    // 定义64位整型寄存器类 (GR64)
    RegisterClass gr64_class = {
        GR64,
        "GR64", // 类名称
        {
            // 与GR32相同的寄存器集合，但在LP64模式下是64位宽
            Reg::RA, Reg::SP, Reg::GP, Reg::TP,
            Reg::T0, Reg::T1, Reg::T2,
            Reg::S0, Reg::S1,
            Reg::A0, Reg::A1, Reg::A2, Reg::A3,
            Reg::A4, Reg::A5, Reg::A6, Reg::A7,
            Reg::S2, Reg::S3, Reg::S4, Reg::S5,
            Reg::S6, Reg::S7, Reg::S8, Reg::S9,
            Reg::S10, Reg::S11,
            Reg::T3, Reg::T4, Reg::T5, Reg::T6},
        1, // 拷贝代价
        2  // 较高的权重(因为是64位)
    };

    // 定义单精度浮点寄存器类 (FP32)
    RegisterClass fp32_class = {
        FP32,
        "FP32", // 类名称
        {
            // 所有浮点寄存器F0-F31
            Reg::F0, Reg::F1, Reg::F2, Reg::F3,
            Reg::F4, Reg::F5, Reg::F6, Reg::F7,
            Reg::F8, Reg::F9, Reg::F10, Reg::F11,
            Reg::F12, Reg::F13, Reg::F14, Reg::F15,
            Reg::F16, Reg::F17, Reg::F18, Reg::F19,
            Reg::F20, Reg::F21, Reg::F22, Reg::F23,
            Reg::F24, Reg::F25, Reg::F26, Reg::F27,
            Reg::F28, Reg::F29, Reg::F30, Reg::F31},
        2, // 浮点拷贝代价较高
        1  // 权重
    };

    // 定义双精度浮点寄存器类 (FP64)
    RegisterClass fp64_class = {
        FP64,
        "FP64", // 类名称
        {
            // 与FP32相同的寄存器，但在D扩展下是64位宽
            Reg::F0, Reg::F1, Reg::F2, Reg::F3,
            Reg::F4, Reg::F5, Reg::F6, Reg::F7,
            Reg::F8, Reg::F9, Reg::F10, Reg::F11,
            Reg::F12, Reg::F13, Reg::F14, Reg::F15,
            Reg::F16, Reg::F17, Reg::F18, Reg::F19,
            Reg::F20, Reg::F21, Reg::F22, Reg::F23,
            Reg::F24, Reg::F25, Reg::F26, Reg::F27,
            Reg::F28, Reg::F29, Reg::F30, Reg::F31},
        2, // 拷贝代价
        2  // 较高的权重(因为是64位)
    };

    // 根据目标ABI添加寄存器类
    add_register_class(gr32_class); // 所有ABI都有32位整型寄存器类

    // 64位ABI添加64位整型寄存器类
    if (abi == ABIVersion::LP64 || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D)
    {
        add_register_class(gr64_class);
    }

    // 根据ABI添加浮点寄存器类
    if (abi == ABIVersion::ILP32F || abi == ABIVersion::LP64F)
    {
        add_register_class(fp32_class); // 单精度浮点
    }

    if (abi == ABIVersion::LP64D)
    {
        add_register_class(fp32_class); // 单精度浮点
        add_register_class(fp64_class); // 双精度浮点
    }

    // 更新寄存器描述符中的寄存器类信息
    for (unsigned reg = 0; reg < reg_descs_.size(); reg++)
    {
        if (reg == Reg::ZERO)
        {
            // 零寄存器不可分配
            reg_descs_[reg].is_allocatable = false;
            reg_descs_[reg].is_reserved = true;
            continue;
        }

        if (reg >= Reg::F0 && reg <= Reg::F31)
        {
            // 浮点寄存器
            if (abi == ABIVersion::ILP32F || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D)
            {
                reg_descs_[reg].primary_rc_id = RegClass::FP32;
                reg_descs_[reg].rc_mask.set(RegClass::FP32);

                // 如果支持D扩展
                if (abi == ABIVersion::LP64D)
                {
                    reg_descs_[reg].rc_mask.set(RegClass::FP64);
                }
            }
            else
            {
                // 没有浮点支持的ABI中，浮点寄存器不可分配
                reg_descs_[reg].is_allocatable = false;
            }
        }
        else if (reg < Reg::F0)
        {
            // 整型寄存器
            reg_descs_[reg].primary_rc_id = RegClass::GR32;
            reg_descs_[reg].rc_mask.set(RegClass::GR32);

            // 64位ABI下
            if (abi == ABIVersion::LP64 || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D)
            {
                reg_descs_[reg].rc_mask.set(RegClass::GR64);
            }
        }

        // 设置被调用者保存的寄存器
        if ((reg >= Reg::S0 && reg <= Reg::S1) ||
            (reg >= Reg::S2 && reg <= Reg::S11) ||
            (reg >= Reg::F8 && reg <= Reg::F9) ||
            (reg >= Reg::F18 && reg <= Reg::F27))
        {
            reg_descs_[reg].is_callee_saved = true;
        }
    }
}

// 初始化调用约定
void RISCVRegisterInfo::initializeCallingConventions(ABIVersion abi)
{
    // 确保调用约定映射有足够空间
    callee_saved_map_.resize(CallingConv::ID::NUM_CALLING_CONV);
    caller_saved_map_.resize(CallingConv::ID::NUM_CALLING_CONV);

    // 整型被调用者保存寄存器
    std::vector<unsigned> callee_saved = {
        Reg::S0, Reg::S1,
        Reg::S2, Reg::S3, Reg::S4, Reg::S5,
        Reg::S6, Reg::S7, Reg::S8, Reg::S9,
        Reg::S10, Reg::S11};

    // 整型调用者保存寄存器
    std::vector<unsigned> caller_saved = {
        Reg::RA,
        Reg::T0, Reg::T1, Reg::T2,
        Reg::A0, Reg::A1, Reg::A2, Reg::A3,
        Reg::A4, Reg::A5, Reg::A6, Reg::A7,
        Reg::T3, Reg::T4, Reg::T5, Reg::T6};

    // 添加浮点寄存器（如果支持）
    bool has_float = (abi == ABIVersion::ILP32F || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D);

    if (has_float)
    {
        // 浮点被调用者保存寄存器
        const std::vector<unsigned> callee_saved_fp = {
            Reg::F8, Reg::F9,
            Reg::F18, Reg::F19, Reg::F20, Reg::F21,
            Reg::F22, Reg::F23, Reg::F24, Reg::F25,
            Reg::F26, Reg::F27};
        callee_saved.insert(callee_saved.end(), callee_saved_fp.begin(), callee_saved_fp.end());

        // 浮点调用者保存寄存器
        const std::vector<unsigned> caller_saved_fp = {
            Reg::F0, Reg::F1, Reg::F2, Reg::F3,
            Reg::F4, Reg::F5, Reg::F6, Reg::F7,
            Reg::F10, Reg::F11, Reg::F12, Reg::F13,
            Reg::F14, Reg::F15, Reg::F16, Reg::F17,
            Reg::F28, Reg::F29, Reg::F30, Reg::F31};
        caller_saved.insert(caller_saved.end(), caller_saved_fp.begin(), caller_saved_fp.end());
    }

    // 设置C调用约定
    callee_saved_map_[CallingConv::C] = callee_saved;
    caller_saved_map_[CallingConv::C] = caller_saved;

    // 参数传递规则
    ArgPassingRule c_rule;

    // 整型参数使用a0-a7
    c_rule.int_regs = {
        Reg::A0, Reg::A1, Reg::A2, Reg::A3,
        Reg::A4, Reg::A5, Reg::A6, Reg::A7};

    // 浮点参数使用fa0-fa7（如果支持）
    if (has_float)
    {
        c_rule.fp_regs = {
            Reg::F10, Reg::F11, Reg::F12, Reg::F13,
            Reg::F14, Reg::F15, Reg::F16, Reg::F17};
    }

    // 设置C调用约定的参数传递规则
    arg_rules_[CallingConv::C] = c_rule;
}

//===----------------------------------------------------------------------===//
// Instruction Selection: RISCVTargetInstInfo Implementation
//===----------------------------------------------------------------------===//
const std::map<unsigned, std::string> opcode_names_ = {
    {RISCV::LUI, "lui"},
    {RISCV::AUIPC, "auipc"},
    {RISCV::JAL, "jal"},
    {RISCV::JALR, "jalr"},
    {RISCV::BEQ, "beq"},
    {RISCV::BNE, "bne"},
    {RISCV::BLT, "blt"},
    {RISCV::BGE, "bge"},
    {RISCV::BLTU, "bltu"},
    {RISCV::BGEU, "bgeu"},
    {RISCV::LB, "lb"},
    {RISCV::LH, "lh"},
    {RISCV::LW, "lw"},
    {RISCV::LBU, "lbu"},
    {RISCV::LHU, "lhu"},
    {RISCV::SB, "sb"},
    {RISCV::SH, "sh"},
    {RISCV::SW, "sw"},
    {RISCV::ADDI, "addi"},
    {RISCV::SLTI, "slti"},
    {RISCV::SLTIU, "sltiu"},
    {RISCV::XORI, "xori"},
    {RISCV::ORI, "ori"},
    {RISCV::ANDI, "andi"},
    {RISCV::SLLI, "slli"},
    {RISCV::SRLI, "srli"},
    {RISCV::SRAI, "srai"},
    {RISCV::ADD, "add"},
    {RISCV::SUB, "sub"},
    {RISCV::SLL, "sll"},
    {RISCV::SLT, "slt"},
    {RISCV::SLTU, "sltu"},
    {RISCV::XOR, "xor"},
    {RISCV::SRL, "srl"},
    {RISCV::SRA, "sra"},
    {RISCV::OR, "or"},
    {RISCV::AND, "and"},
    {RISCV::RET, "ret"},
    {RISCV::NOP, "nop"},
    {RISCV::LI, "li"},
    {RISCV::MV, "mv"},
    {RISCV::LA, "la"},
    {RISCV::CALL, "call"},
    {RISCV::J, "j"},
    {RISCV::FLW, "flw"},
    {RISCV::FSW, "fsw"},
    {RISCV::FADD_S, "fadd.s"},
    {RISCV::FSUB_S, "fsub.s"},
    {RISCV::FMUL_S, "fmul.s"},
    {RISCV::FDIV_S, "fdiv.s"},
    {RISCV::FLD, "fld"},
    {RISCV::FSD, "fsd"},
    {RISCV::FADD_D, "fadd.d"},
    {RISCV::FSUB_D, "fsub.d"},
    {RISCV::FMUL_D, "fmul.d"},
    {RISCV::FDIV_D, "fdiv.d"},
};

RISCVTargetInstInfo::RISCVTargetInstInfo(ABIVersion abi)
    : abi_version_(abi)
{
    // 如果支持浮点，添加浮点指令
    bool has_float = (abi == ABIVersion::ILP32F || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D);
    // 初始化指令延迟表
    inst_latency_ = {
        {RISCV::LUI, 1},
        {RISCV::AUIPC, 1},
        {RISCV::JAL, 2},
        {RISCV::JALR, 2},
        {RISCV::BEQ, 1},
        {RISCV::BNE, 1},
        {RISCV::BLT, 1},
        {RISCV::BGE, 1},
        {RISCV::BLTU, 1},
        {RISCV::BGEU, 1},
        {RISCV::LB, 3},
        {RISCV::LH, 3},
        {RISCV::LW, 3},
        {RISCV::LBU, 3},
        {RISCV::LHU, 3},
        {RISCV::SB, 1},
        {RISCV::SH, 1},
        {RISCV::SW, 1},
        {RISCV::ADDI, 1},
        {RISCV::SLTI, 1},
        {RISCV::SLTIU, 1},
        {RISCV::XORI, 1},
        {RISCV::ORI, 1},
        {RISCV::ANDI, 1},
        {RISCV::SLLI, 1},
        {RISCV::SRLI, 1},
        {RISCV::SRAI, 1},
        {RISCV::ADD, 1},
        {RISCV::SUB, 1},
        {RISCV::SLL, 1},
        {RISCV::SLT, 1},
        {RISCV::SLTU, 1},
        {RISCV::XOR, 1},
        {RISCV::SRL, 1},
        {RISCV::SRA, 1},
        {RISCV::OR, 1},
        {RISCV::AND, 1}};

    // 如果支持浮点，添加浮点指令延迟
    if (has_float)
    {
        inst_latency_[RISCV::FLW] = 3;
        inst_latency_[RISCV::FSW] = 1;
        inst_latency_[RISCV::FADD_S] = 4;
        inst_latency_[RISCV::FSUB_S] = 4;
        inst_latency_[RISCV::FMUL_S] = 5;
        inst_latency_[RISCV::FDIV_S] = 10;
    }

    // 如果支持双精度浮点，添加双精度浮点指令延迟
    if (abi == ABIVersion::LP64D)
    {
        inst_latency_[RISCV::FLD] = 3;
        inst_latency_[RISCV::FSD] = 1;
        inst_latency_[RISCV::FADD_D] = 5;
        inst_latency_[RISCV::FSUB_D] = 5;
        inst_latency_[RISCV::FMUL_D] = 6;
        inst_latency_[RISCV::FDIV_D] = 15;
    }
}

const char *RISCVTargetInstInfo::opcode_name(unsigned opcode) const
{
    auto it = opcode_names_.find(opcode);
    if (it != opcode_names_.end())
    {
        return it->second.c_str();
    }
    return "unknown";
}

unsigned RISCVTargetInstInfo::get_inst_size(const MachineInst &MI) const
{
    // RISC-V基本指令均为4字节
    return 4;
}

bool RISCVTargetInstInfo::verify_instruction(const MachineInst &MI, std::string &error_msg) const
{
    unsigned opcode = MI.opcode();

    // 验证操作码是否已知
    if (opcode_names_.find(opcode) == opcode_names_.end())
    {
        error_msg = "Unknown opcode";
        return false;
    }

    // 根据指令类型验证操作数数量和类型
    switch (opcode)
    {
    // R类型指令: rd, rs1, rs2
    case RISCV::ADD:
    case RISCV::SUB:
    case RISCV::SLL:
    case RISCV::SLT:
    case RISCV::SLTU:
    case RISCV::XOR:
    case RISCV::SRL:
    case RISCV::SRA:
    case RISCV::OR:
    case RISCV::AND:
        if (MI.operands().size() != 3)
        {
            error_msg = "R-type instruction requires 3 operands";
            return false;
        }
        break;

    // I类型指令: rd, rs1, imm
    case RISCV::ADDI:
    case RISCV::SLTI:
    case RISCV::SLTIU:
    case RISCV::XORI:
    case RISCV::ORI:
    case RISCV::ANDI:
    case RISCV::SLLI:
    case RISCV::SRLI:
    case RISCV::SRAI:
    case RISCV::LB:
    case RISCV::LH:
    case RISCV::LW:
    case RISCV::LBU:
    case RISCV::LHU:
    case RISCV::JALR:
        if (MI.operands().size() != 3)
        {
            error_msg = "I-type instruction requires 3 operands";
            return false;
        }
        break;

    // S类型指令: rs2, offset(rs1)
    case RISCV::SB:
    case RISCV::SH:
    case RISCV::SW:
        if (MI.operands().size() != 3)
        {
            error_msg = "S-type instruction requires 3 operands";
            return false;
        }
        break;

    // B类型指令: rs1, rs2, offset
    case RISCV::BEQ:
    case RISCV::BNE:
    case RISCV::BLT:
    case RISCV::BGE:
    case RISCV::BLTU:
    case RISCV::BGEU:
        if (MI.operands().size() != 3)
        {
            error_msg = "B-type instruction requires 3 operands";
            return false;
        }
        break;

    // U类型指令: rd, imm
    case RISCV::LUI:
    case RISCV::AUIPC:
        if (MI.operands().size() != 2)
        {
            error_msg = "U-type instruction requires 2 operands";
            return false;
        }
        break;

    // J类型指令: rd, offset
    case RISCV::JAL:
        if (MI.operands().size() != 2)
        {
            error_msg = "J-type instruction requires 2 operands";
            return false;
        }
        break;

    // 伪指令验证
    case RISCV::RET:
        if (!MI.operands().empty())
        {
            error_msg = "RET instruction requires no operands";
            return false;
        }
        break;

    case RISCV::NOP:
        if (!MI.operands().empty())
        {
            error_msg = "NOP instruction requires no operands";
            return false;
        }
        break;

    case RISCV::LI:
        if (MI.operands().size() != 2)
        {
            error_msg = "LI instruction requires 2 operands";
            return false;
        }
        break;

    case RISCV::MV:
        if (MI.operands().size() != 2)
        {
            error_msg = "MV instruction requires 2 operands";
            return false;
        }
        break;

    case RISCV::LA:
    case RISCV::CALL:
        if (MI.operands().size() < 1)
        {
            error_msg = "Instruction requires at least 1 operand";
            return false;
        }
        break;
    }

    return true;
}

uint32_t RISCVTargetInstInfo::get_binary_encoding(const MachineInst &MI) const
{
    unsigned opcode = MI.opcode();

    // 根据指令类型选择适当的编码方法
    switch (opcode)
    {
    // R类型指令
    case RISCV::ADD:
        return encode_R(0x33, 0x0, 0x00, MI);
    case RISCV::SUB:
        return encode_R(0x33, 0x0, 0x20, MI);
    case RISCV::SLL:
        return encode_R(0x33, 0x1, 0x00, MI);
    case RISCV::SLT:
        return encode_R(0x33, 0x2, 0x00, MI);
    case RISCV::SLTU:
        return encode_R(0x33, 0x3, 0x00, MI);
    case RISCV::XOR:
        return encode_R(0x33, 0x4, 0x00, MI);
    case RISCV::SRL:
        return encode_R(0x33, 0x5, 0x00, MI);
    case RISCV::SRA:
        return encode_R(0x33, 0x5, 0x20, MI);
    case RISCV::OR:
        return encode_R(0x33, 0x6, 0x00, MI);
    case RISCV::AND:
        return encode_R(0x33, 0x7, 0x00, MI);

    // I类型指令
    case RISCV::ADDI:
        return encode_I(0x13, 0x0, MI);
    case RISCV::SLTI:
        return encode_I(0x13, 0x2, MI);
    case RISCV::SLTIU:
        return encode_I(0x13, 0x3, MI);
    case RISCV::XORI:
        return encode_I(0x13, 0x4, MI);
    case RISCV::ORI:
        return encode_I(0x13, 0x6, MI);
    case RISCV::ANDI:
        return encode_I(0x13, 0x7, MI);
    case RISCV::SLLI:
        return encode_I(0x13, 0x1, MI);
    case RISCV::SRLI:
        return encode_I(0x13, 0x5, MI);
    case RISCV::SRAI:
        return encode_I(0x13, 0x5, MI) | (1 << 30);
    case RISCV::LB:
        return encode_I(0x03, 0x0, MI);
    case RISCV::LH:
        return encode_I(0x03, 0x1, MI);
    case RISCV::LW:
        return encode_I(0x03, 0x2, MI);
    case RISCV::LBU:
        return encode_I(0x03, 0x4, MI);
    case RISCV::LHU:
        return encode_I(0x03, 0x5, MI);
    case RISCV::JALR:
        return encode_I(0x67, 0x0, MI);

    // S类型指令
    case RISCV::SB:
        return encode_S(0x23, 0x0, MI);
    case RISCV::SH:
        return encode_S(0x23, 0x1, MI);
    case RISCV::SW:
        return encode_S(0x23, 0x2, MI);

    // B类型指令
    case RISCV::BEQ:
        return encode_B(0x63, 0x0, MI);
    case RISCV::BNE:
        return encode_B(0x63, 0x1, MI);
    case RISCV::BLT:
        return encode_B(0x63, 0x4, MI);
    case RISCV::BGE:
        return encode_B(0x63, 0x5, MI);
    case RISCV::BLTU:
        return encode_B(0x63, 0x6, MI);
    case RISCV::BGEU:
        return encode_B(0x63, 0x7, MI);

    // U类型指令
    case RISCV::LUI:
        return encode_U(0x37, MI);
    case RISCV::AUIPC:
        return encode_U(0x17, MI);

    // J类型指令
    case RISCV::JAL:
        return encode_J(0x6F, MI);

    // 伪指令和特殊指令
    case RISCV::RET:
        return 0x8067; // jalr x0, 0(ra)
    case RISCV::NOP:
        return 0x13; // addi x0, x0, 0

    default:
        // 未知指令或未实现的编码
        return 0xFFFFFFFF;
    }
}

void RISCVTargetInstInfo::expand_pseudo(MachineBasicBlock &MBB, MachineBasicBlock::iterator mii) const
{
    MachineInst &MI = **mii;
    unsigned opcode = MI.opcode();

    switch (opcode)
    {
    case RISCV::LI:
        expand_li(MBB, mii);
        break;
    case RISCV::LA:
        expand_la(MBB, mii);
        break;
    case RISCV::MV:
        expand_mv(MBB, mii);
        break;
    case RISCV::CALL:
        expand_call(MBB, mii);
        break;
    case RISCV::J:
        expand_j(MBB, mii);
        break;
    }
}

bool RISCVTargetInstInfo::is_return(const MachineInst &MI) const
{
    return MI.opcode() == RISCV::RET;
}

bool RISCVTargetInstInfo::is_call(const MachineInst &MI) const
{
    return MI.opcode() == RISCV::CALL || (MI.opcode() == RISCV::JAL && MI.operands()[0].is_reg() && MI.operands()[0].reg() == Reg::RA);
}

bool RISCVTargetInstInfo::is_legal_immediate(int64_t imm, unsigned operand_size) const
{
    switch (operand_size)
    {
    case 12: // I类型立即数
        return (imm >= -2048 && imm <= 2047);
    case 20: // U类型立即数
        return (imm >= 0 && imm <= 0xFFFFF);
    case 21: // J类型立即数
        return (imm >= -(1 << 20) && imm < (1 << 20));
    case 13: // B类型立即数
        return (imm >= -(1 << 12) && imm < (1 << 12));
    default:
        return false;
    }
}

unsigned RISCVTargetInstInfo::get_instruction_latency(unsigned opcode) const
{
    auto it = inst_latency_.find(opcode);
    if (it != inst_latency_.end())
    {
        return it->second;
    }
    return 1; // 默认延迟
}
void RISCVTargetInstInfo::copy_phys_reg(MachineBasicBlock &MBB, MachineBasicBlock::iterator mii,
                                        unsigned dest_reg, unsigned src_reg) const
{
    // 使用mv伪指令实现寄存器复制
    if (dest_reg >= Reg::ZERO && dest_reg <= Reg::T6 &&
        src_reg >= Reg::ZERO && src_reg <= Reg::T6)
    {
        // 整型寄存器之间的复制
        auto copy_inst = new MachineInst(RISCV::ADD, {MOperand::create_reg(dest_reg, true),
                                                      MOperand::create_reg(src_reg, false),
                                                      MOperand::create_reg(Reg::ZERO, false)});
        mii = MBB.insert(mii, std::unique_ptr<MachineInst>(copy_inst));
        mii++;
    }
    else if (dest_reg >= Reg::F0 && dest_reg <= Reg::F31 &&
             src_reg >= Reg::F0 && src_reg <= Reg::F31)
    {
        // 浮点寄存器之间的复制
        if (abi_version_ == ABIVersion::LP64D)
        {
            // 双精度复制
            auto copy_inst = new MachineInst(RISCV::FADD_D, {MOperand::create_reg(dest_reg, true),
                                                             MOperand::create_reg(src_reg, false),
                                                             MOperand::create_reg(src_reg, false)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(copy_inst));
            mii++;
        }
        else
        {
            // 单精度复制
            auto copy_inst = new MachineInst(RISCV::FADD_S, {MOperand::create_reg(dest_reg, true),
                                                             MOperand::create_reg(src_reg, false),
                                                             MOperand::create_reg(src_reg, false)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(copy_inst));
            mii++;
        }
    }
    // 其他情况（如混合整型/浮点）需要使用内存进行复制
}
bool RISCVTargetInstInfo::legalize_inst(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii, MachineFunction &MF) const
{
    MachineInst &MI = **mii;
    unsigned opcode = MI.opcode();

    switch (opcode)
    {
    case RISCV::ADDI:
    case RISCV::SLTI:
    case RISCV::SLTIU:
    case RISCV::XORI:
    case RISCV::ORI:
    case RISCV::ANDI:
    {
        // I类型指令的立即数范围是12位有符号整数
        if (MI.operands().size() == 3 && MI.operands()[2].is_imm())
        {
            int64_t imm = MI.operands()[2].imm();
            if (imm < -2048 || imm > 2047)
            {
                // 立即数超出范围，需要扩展
                int64_t hi, lo;
                bool can_handle_with_lui_addi = split_imm(imm, hi, lo);
                if (can_handle_with_lui_addi)
                {
                    // 使用LUI+ADDI序列处理大立即数（32位范围内）
                    unsigned dest_reg = MI.operands()[0].reg();
                    auto lui = new MachineInst(RISCV::LUI, {MOperand::create_reg(dest_reg, true),
                                                            MOperand::create_imm(hi)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(lui));
                    mii++;

                    // 更新当前指令为ADDI
                    auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg, true),
                                                              MOperand::create_reg(dest_reg, false),
                                                              MOperand::create_imm(lo)});

                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(addi));
                    mii++;
                    mbb.erase(mii);
                    return true;
                }
                else
                {
                    // 处理64位值 - 使用多条指令序列
                    unsigned dest_reg = MI.operands()[0].reg();
                    unsigned src_reg = MI.operands()[1].reg();
                    unsigned temp_reg = Reg::T0; // 使用T0作为临时寄存器
                    MO_NOP(src_reg);             // FIXME: src_reg not used

                    // 先处理低32位
                    int64_t lower32 = imm & 0xFFFFFFFF;
                    int64_t hi_lower, lo_lower;
                    split_imm(lower32, hi_lower, lo_lower);

                    // 构建低32位
                    auto lui = new MachineInst(RISCV::LUI, {MOperand::create_reg(dest_reg, true),
                                                            MOperand::create_imm(hi_lower)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(lui));
                    mii++;

                    auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg, true),
                                                              MOperand::create_reg(dest_reg, false),
                                                              MOperand::create_imm(lo_lower)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(addi));
                    mii++;

                    // 处理高32位
                    int64_t upper32 = imm >> 32;

                    // 加载高32位到临时寄存器
                    int64_t hi_upper, lo_upper;
                    split_imm(upper32, hi_upper, lo_upper);

                    auto lui_upper = new MachineInst(RISCV::LUI, {MOperand::create_reg(temp_reg, true),
                                                                  MOperand::create_imm(hi_upper)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(lui_upper));
                    mii++;

                    auto addi_upper = new MachineInst(RISCV::ADDI, {MOperand::create_reg(temp_reg, true),
                                                                    MOperand::create_reg(temp_reg, false),
                                                                    MOperand::create_imm(lo_upper)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(addi_upper));
                    mii++;

                    // 将低32位左移32位
                    auto slli = new MachineInst(RISCV::SLLI, {MOperand::create_reg(dest_reg, true),
                                                              MOperand::create_reg(dest_reg, false),
                                                              MOperand::create_imm(32)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(slli));
                    mii++;

                    // 将高32位与左移后的低32位进行OR操作
                    auto orr = new MachineInst(RISCV::OR, {MOperand::create_reg(dest_reg, true),
                                                           MOperand::create_reg(dest_reg, false),
                                                           MOperand::create_reg(temp_reg, false)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(orr));
                    mii++;

                    mbb.erase(mii);
                    return true;
                }
            }
        }
        break;
    }
    }

    return false; // 指令未修改
}

// 编码辅助函数实现
uint32_t RISCVTargetInstInfo::encode_R(uint32_t opcode, uint32_t funct3, uint32_t funct7,
                                       const MachineInst &MI) const
{
    uint32_t rd = MI.operands()[0].reg() & 0x1F;
    uint32_t rs1 = MI.operands()[1].reg() & 0x1F;
    uint32_t rs2 = MI.operands()[2].reg() & 0x1F;

    return (funct7 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | opcode;
}

uint32_t RISCVTargetInstInfo::encode_I(uint32_t opcode, uint32_t funct3,
                                       const MachineInst &MI) const
{
    uint32_t rd = MI.operands()[0].reg() & 0x1F;
    uint32_t rs1 = MI.operands()[1].reg() & 0x1F;
    uint32_t imm = MI.operands()[2].imm() & 0xFFF;

    return (imm << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | opcode;
}

uint32_t RISCVTargetInstInfo::encode_S(uint32_t opcode, uint32_t funct3,
                                       const MachineInst &MI) const
{
    uint32_t rs1 = MI.operands()[1].reg() & 0x1F;
    uint32_t rs2 = MI.operands()[0].reg() & 0x1F;
    uint32_t imm = MI.operands()[2].imm() & 0xFFF;

    uint32_t imm11_5 = (imm >> 5) & 0x7F;
    uint32_t imm4_0 = imm & 0x1F;

    return (imm11_5 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (imm4_0 << 7) | opcode;
}

uint32_t RISCVTargetInstInfo::encode_B(uint32_t opcode, uint32_t funct3,
                                       const MachineInst &MI) const
{
    uint32_t rs1 = MI.operands()[0].reg() & 0x1F;
    uint32_t rs2 = MI.operands()[1].reg() & 0x1F;
    uint32_t imm = MI.operands()[2].imm() & 0x1FFE; // 13-bit, LSB always 0

    uint32_t imm12 = (imm >> 12) & 0x1;
    uint32_t imm11 = (imm >> 11) & 0x1;
    uint32_t imm10_5 = (imm >> 5) & 0x3F;
    uint32_t imm4_1 = (imm >> 1) & 0xF;

    return (imm12 << 31) | (imm10_5 << 25) | (rs2 << 20) | (rs1 << 15) |
           (funct3 << 12) | (imm4_1 << 8) | (imm11 << 7) | opcode;
}

uint32_t RISCVTargetInstInfo::encode_U(uint32_t opcode, const MachineInst &MI) const
{
    uint32_t rd = MI.operands()[0].reg() & 0x1F;
    uint32_t imm = MI.operands()[1].imm() & 0xFFFFF;

    return (imm << 12) | (rd << 7) | opcode;
}

uint32_t RISCVTargetInstInfo::encode_J(uint32_t opcode, const MachineInst &MI) const
{
    uint32_t rd = MI.operands()[0].reg() & 0x1F;
    uint32_t imm = MI.operands()[1].imm() & 0x1FFFFE; // 20-bit, LSB always 0

    uint32_t imm20 = (imm >> 20) & 0x1;
    uint32_t imm10_1 = (imm >> 1) & 0x3FF;
    uint32_t imm11 = (imm >> 11) & 0x1;
    uint32_t imm19_12 = (imm >> 12) & 0xFF;

    return (imm20 << 31) | (imm10_1 << 21) | (imm11 << 20) |
           (imm19_12 << 12) | (rd << 7) | opcode;
}

// 立即数处理辅助函数
int64_t RISCVTargetInstInfo::adjust_imm(int64_t imm, unsigned bits) const
{
    // 截断到指定位数的有符号数
    int64_t mask = (1LL << bits) - 1;
    int64_t sign_bit = 1LL << (bits - 1);

    imm &= mask;
    if (imm & sign_bit)
    {
        // 负数，进行符号扩展
        imm |= ~mask;
    }

    return imm;
}

bool RISCVTargetInstInfo::split_imm(int64_t imm, int64_t &hi, int64_t &lo) const
{
    // Check if this is a true 64-bit value (can't fit in 32 bits with sign extension)
    bool is_64bit = (imm > INT32_MAX || imm < INT32_MIN);

    // 将立即数分解为高20位(用于LUI)和低12位(用于ADDI)
    lo = adjust_imm(imm, 12);
    hi = ((imm - lo) >> 12) & 0xFFFFF;

    // 如果低12位是负数，需要调整高位
    if (lo < 0)
    {
        hi += 1;
    }

    // Return false if this is a 64-bit value that needs special handling
    return !is_64bit;
}

// 伪指令扩展实现
void RISCVTargetInstInfo::expand_li(MachineBasicBlock &MBB, MachineBasicBlock::iterator mii) const
{
    MachineInst &MI = **mii;
    unsigned dest_reg = MI.operands()[0].reg();
    int64_t imm = MI.operands()[1].imm();

    if (imm >= -2048 && imm <= 2047)
    {
        // 小立即数可以直接使用ADDI
        auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg, true),
                                                  MOperand::create_reg(Reg::ZERO, false),
                                                  MOperand::create_imm(imm)});
        mii = MBB.insert(mii, std::unique_ptr<MachineInst>(addi));
        mii++;
    }
    else
    {
        // 大立即数需要使用LUI+ADDI
        int64_t hi, lo;
        bool can_handle_with_lui_addi = split_imm(imm, hi, lo);
        if (can_handle_with_lui_addi)
        {
            auto lui = new MachineInst(RISCV::LUI, {MOperand::create_reg(dest_reg, true),
                                                    MOperand::create_imm(hi)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(lui));
            mii++;
            auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg, true),
                                                      MOperand::create_reg(dest_reg, false),
                                                      MOperand::create_imm(lo)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(addi));
            mii++;
        }
        else
        {
            // 处理64位立即数 - 优化实现
            unsigned temp_reg = Reg::T0; // 使用T0作为临时寄存器

            // 先处理低32位
            int64_t lower32 = imm & 0xFFFFFFFF;
            int64_t hi_lower, lo_lower;
            split_imm(lower32, hi_lower, lo_lower);

            auto lui = new MachineInst(RISCV::LUI, {MOperand::create_reg(dest_reg, true),
                                                    MOperand::create_imm(hi_lower)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(lui));
            mii++;

            auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg, true),
                                                      MOperand::create_reg(dest_reg, false),
                                                      MOperand::create_imm(lo_lower)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(addi));
            mii++;

            // 如果高32位为零，可以优化不处理
            int64_t upper32 = imm >> 32;
            if (upper32 != 0)
            {
                // 处理高32位
                int64_t hi_upper, lo_upper;
                split_imm(upper32, hi_upper, lo_upper);

                auto lui_upper = new MachineInst(RISCV::LUI, {MOperand::create_reg(temp_reg, true),
                                                              MOperand::create_imm(hi_upper)});
                mii = MBB.insert(mii, std::unique_ptr<MachineInst>(lui_upper));
                mii++;

                auto addi_upper = new MachineInst(RISCV::ADDI, {MOperand::create_reg(temp_reg, true),
                                                                MOperand::create_reg(temp_reg, false),
                                                                MOperand::create_imm(lo_upper)});
                mii = MBB.insert(mii, std::unique_ptr<MachineInst>(addi_upper));
                mii++;

                // 将低32位左移32位
                auto slli = new MachineInst(RISCV::SLLI, {MOperand::create_reg(dest_reg, true),
                                                          MOperand::create_reg(dest_reg, false),
                                                          MOperand::create_imm(32)});
                mii = MBB.insert(mii, std::unique_ptr<MachineInst>(slli));
                mii++;

                // 将高32位与左移后的低32位进行OR操作
                auto orr = new MachineInst(RISCV::OR, {MOperand::create_reg(dest_reg, true),
                                                       MOperand::create_reg(dest_reg, false),
                                                       MOperand::create_reg(temp_reg, false)});
                mii = MBB.insert(mii, std::unique_ptr<MachineInst>(orr));
                mii++;
            }
        }
    }
    MBB.erase(mii);
}
void RISCVTargetInstInfo::expand_la(MachineBasicBlock &MBB, MachineBasicBlock::iterator mii) const
{
    MachineInst &MI = **mii;
    // 加载地址伪指令 - 通常展开为 AUIPC + ADDI 序列
    unsigned dest_reg = MI.operands()[0].reg();

    // 创建auipc加载高20位地址
    auto auipc = new MachineInst(RISCV::AUIPC, {
                                                   MOperand::create_reg(dest_reg, true),
                                                   MOperand::create_imm(0) // 重定位器会填充正确的值
                                               });

    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(auipc));
    mii++;
    // 创建addi加载低12位偏移
    auto addi = new MachineInst(RISCV::ADDI, {
                                                 MOperand::create_reg(dest_reg, true),
                                                 MOperand::create_reg(dest_reg, false),
                                                 MOperand::create_imm(0) // 重定位器会填充正确的值
                                             });

    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(addi));
    mii++;
    MBB.erase(mii);
}

void RISCVTargetInstInfo::expand_mv(MachineBasicBlock &MBB, MachineBasicBlock::iterator mii) const
{
    MachineInst &MI = **mii;
    // 移动寄存器伪指令 - 展开为 ADDI rd, rs, 0
    unsigned dest_reg = MI.operands()[0].reg();
    unsigned src_reg = MI.operands()[1].reg();

    auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg, true),
                                              MOperand::create_reg(src_reg, false),
                                              MOperand::create_imm(0)});

    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(addi));
    mii++;
    MBB.erase(mii);
}

void RISCVTargetInstInfo::expand_call(MachineBasicBlock &MBB, MachineBasicBlock::iterator mii) const
{
    // 调用函数伪指令 - 展开为 AUIPC + JALR 序列
    // 创建auipc加载高20位地址
    auto auipc = new MachineInst(RISCV::AUIPC, {
                                                   MOperand::create_reg(Reg::T0, true), // 使用临时寄存器
                                                   MOperand::create_imm(0)              // 重定位器会填充正确的值
                                               });

    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(auipc));
    mii++;
    // 创建jalr进行调用
    auto jalr = new MachineInst(RISCV::JALR, {
                                                 MOperand::create_reg(Reg::RA, true), // 保存返回地址
                                                 MOperand::create_reg(Reg::T0, false),
                                                 MOperand::create_imm(0) // 重定位器会填充正确的值
                                             });
    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(jalr));
    mii++;
    MBB.erase(mii);
}

void RISCVTargetInstInfo::expand_j(MachineBasicBlock &MBB, MachineBasicBlock::iterator mii) const
{
    MachineInst &MI = **mii;
    // 无条件跳转伪指令 - 展开为 JAL x0, offset
    auto jal = new MachineInst(RISCV::JAL, {
                                               MOperand::create_reg(Reg::ZERO, false), // 不保存返回地址
                                               MI.operands()[0]                        // 直接使用原始偏移量操作数
                                           });
    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(jal));
    mii++;
    MBB.erase(mii);
}

void RISCVTargetInstInfo::insert_load_from_stack(MachineBasicBlock &mbb,
                                                 MachineBasicBlock::iterator insert_point,
                                                 unsigned dest_reg, int frame_index,
                                                 int64_t offset) const
{
    // 获取函数和帧索引信息
    MachineFunction *mf = mbb.parent();
    const FrameObjectMetadata *fobjinfo = mf->frame()->get_frame_object(frame_index);

    // 选择适当的加载指令
    unsigned load_op = RISCV::LW; // 默认使用字加载

    // 根据寄存器类型选择加载指令
    if (dest_reg >= Reg::F0 && dest_reg <= Reg::F31)
    {
        // 浮点寄存器加载
        if (fobjinfo && fobjinfo->size == 8)
        {
            load_op = RISCV::FLD; // 双精度浮点加载
        }
        else
        {
            load_op = RISCV::FLW; // 单精度浮点加载
        }
    }
    else
    {
        // 整型寄存器加载
        if (fobjinfo && fobjinfo->size == 8 &&
            (abi_version_ == ABIVersion::LP64 ||
             abi_version_ == ABIVersion::LP64F ||
             abi_version_ == ABIVersion::LP64D))
        {
            // 在64位模式下可以使用64位加载，这里简化处理仍使用LW
            load_op = RISCV::LW;
        }
    }

    // 使用帧指针或栈指针作为基址寄存器
    unsigned base_reg = Reg::S0; // 默认使用栈指针

    // 创建内存操作数和指令
    std::vector<MOperand> ops;
    ops.push_back(MOperand::create_reg(dest_reg, true));      // 目标寄存器操作数
    ops.push_back(MOperand::create_mem_ri(base_reg, offset)); // 内存操作数

    // 创建并插入加载指令
    auto instr = std::make_unique<MachineInst>(load_op, ops);
    instr->set_flag(MIFlag::MayLoad);

    mbb.insert(insert_point, std::move(instr));
}
void RISCVTargetInstInfo::insert_store_to_stack(MachineBasicBlock &mbb,
                                                MachineBasicBlock::iterator insert_point,
                                                unsigned src_reg, int frame_index,
                                                int64_t offset) const
{
    // 获取函数和帧索引信息
    MachineFunction *mf = mbb.parent();
    const FrameObjectMetadata *fobjinfo = mf->frame()->get_frame_object(frame_index);

    // 选择适当的存储指令
    unsigned store_op = RISCV::SW; // 默认使用字存储

    // 根据寄存器类型选择存储指令
    if (src_reg >= Reg::F0 && src_reg <= Reg::F31)
    {
        // 浮点寄存器存储
        if (fobjinfo && fobjinfo->size == 8)
        {
            store_op = RISCV::FSD; // 双精度浮点存储
        }
        else
        {
            store_op = RISCV::FSW; // 单精度浮点存储
        }
    }
    else
    {
        // 整型寄存器存储
        if (fobjinfo && fobjinfo->size == 8 &&
            (abi_version_ == ABIVersion::LP64 ||
             abi_version_ == ABIVersion::LP64F ||
             abi_version_ == ABIVersion::LP64D))
        {
            // 在64位模式下可以使用64位存储，这里简化处理仍使用SW
            store_op = RISCV::SW;
        }
    }

    // 使用帧指针或栈指针作为基址寄存器
    unsigned base_reg = Reg::SP; // 默认使用栈指针

    // 创建内存操作数和指令
    std::vector<MOperand> ops;
    ops.push_back(MOperand::create_reg(src_reg, false));      // 源寄存器操作数
    ops.push_back(MOperand::create_mem_ri(base_reg, offset)); // 内存操作数

    // 创建并插入存储指令
    auto instr = std::make_unique<MachineInst>(store_op, ops);
    instr->set_flag(MIFlag::MayStore);

    mbb.insert(insert_point, std::move(instr));
}

namespace RISCV
{

    MachineBasicBlock *find_target_block_by_label(const MachineFunction &mf, const std::string &label)
    {
        const auto &blocks = mf.basic_blocks();
        for (auto &target_mb_ptr : blocks)
        {
            MachineBasicBlock *target_mbb = target_mb_ptr.get();
            if (target_mbb->label() == label)
            {
                return target_mbb;
            }
        }
        return nullptr; // 未找到目标块
    }

    // 辅助函数：添加跳转目标作为后继节点
    void add_target_as_successor(MachineBasicBlock *source_block,
                                 const MOperand &target_operand,
                                 MachineFunction &mf)
    {
        if (target_operand.is_global())
        {
            GlobalVariable *gv = target_operand.global();
            MachineBasicBlock *target_block = find_target_block_by_label(mf, gv->name());
            if (target_block)
            {
                source_block->add_successor(target_block);
            }
            else
            {
                MO_DEBUG("Invalid branch target global: %s", gv->name().c_str());
            }
        }
        else if (target_operand.is_external_sym())
        {
            MachineBasicBlock *target_block = find_target_block_by_label(mf, target_operand.external_sym());
            if (target_block)
            {
                source_block->add_successor(target_block);
            }
            else
            {
                MO_DEBUG("Invalid branch target external symbol: %s", target_operand.external_sym());
            }
        }
        else if (target_operand.is_imm())
        {
            // 注意：对立即数偏移的处理需要更多上下文信息
            // 在某些情况下，立即数可能是相对于PC的偏移
            MO_DEBUG("Branch with immediate offset not fully supported yet");
        }
        else
        {
            MO_DEBUG("Target not processed: %s", target_operand.to_string().c_str());
        }
        // 注意：对于寄存器间接跳转，通常无法静态确定目标
    }

    void build_cfg_from_instructions(MachineFunction &mf)
    {
        auto module_ = mf.parent();
        MO_ASSERT(module_, "Module not set");
        const TargetInstInfo *tii = module_->target_inst_info();

        // 清理所有基本块的CFG信息
        for (auto &mb_ptr : mf.basic_blocks())
        {
            mb_ptr->clear_cfg();
        }

        // 创建标签到基本块的映射
        std::unordered_map<std::string, MachineBasicBlock *> label_to_block;
        for (auto &mb_ptr : mf.basic_blocks())
        {
            MachineBasicBlock *mbb = mb_ptr.get();
            if (!mbb->label().empty())
            {
                label_to_block[mbb->label()] = mbb;
            }
        }

        // 遍历所有基本块
        const auto &blocks = mf.basic_blocks();
        for (size_t i = 0; i < blocks.size(); ++i)
        {
            MachineBasicBlock *mbb = blocks[i].get();

            // 如果基本块为空，跳过
            if (mbb->begin() == mbb->end())
            {
                continue;
            }

            // 获取基本块的最后一条指令
            auto last_inst_it = std::prev(mbb->end());
            MachineInst &last_inst = **last_inst_it;
            unsigned opcode = last_inst.opcode();
            MO_DEBUG("Processing block %s, inst opcode: %s with %zu operands",
                     mbb->label().c_str(), opcode_to_str(static_cast<RISCV::Opcode>(opcode)), last_inst.operands().size());
            bool is_fallthrough = true; // 默认情况下会落入下一块

            // 检查最后一条指令的类型
            if (tii->is_return(last_inst))
            {
                // 返回指令没有后继
                is_fallthrough = false;
            }
            else if (opcode == RISCV::JAL || opcode == RISCV::J)
            {
                // 无条件跳转指令
                is_fallthrough = false; // 无条件跳转不会落入下一块

                // 添加跳转目标作为后继节点
                if (last_inst.operands().size() >= 2)
                {                                                          // JAL rd, target
                    const MOperand &jump_target = last_inst.operands()[1]; // 第二个操作数是目标
                    add_target_as_successor(mbb, jump_target, mf);
                }
                else if (opcode == RISCV::J && !last_inst.operands().empty())
                {
                    // J 伪指令可能只有一个操作数
                    const MOperand &jump_target = last_inst.operands()[0];
                    add_target_as_successor(mbb, jump_target, mf);
                }
            }
            else if (opcode == RISCV::JALR)
            {
                // 间接跳转指令 - 使用寄存器作为目标
                // 通常无法静态确定目标，除非是返回指令的特殊情况
                is_fallthrough = false; // 通常不会落入下一块

                // 特殊情况：ret 伪指令 (jalr x0, 0(ra))
                // 不添加后继节点，因为目标在运行时确定
            }
            else if (opcode == RISCV::BEQ || opcode == RISCV::BNE ||
                     opcode == RISCV::BLT || opcode == RISCV::BGE ||
                     opcode == RISCV::BLTU || opcode == RISCV::BGEU)
            {
                // 条件分支指令

                // 添加分支目标作为后继节点
                if (last_inst.operands().size() >= 3)
                {                                                            // 分支指令格式：rs1, rs2, target
                    const MOperand &branch_target = last_inst.operands()[2]; // 第三个操作数是目标
                    add_target_as_successor(mbb, branch_target, mf);
                }

                // 条件分支有fallthrough，将在下面处理
                is_fallthrough = true;
            }
            else if (opcode == RISCV::CALL)
            {
                // 调用指令通常有fallthrough
                is_fallthrough = true;

                // 如果是间接调用，可能无法确定目标
                // 直接调用会被扩展为伪指令序列，在这里不需特殊处理
            }

            // 处理fallthrough情况 - 连续的两个块之间的隐式连接
            if (is_fallthrough && i + 1 < blocks.size())
            {
                MachineBasicBlock *next_block = blocks[i + 1].get();
                mbb->add_successor(next_block);
            }
        }

        // 验证CFG
        for (auto &mb_ptr : mf.basic_blocks())
        {
            MachineBasicBlock *mbb = mb_ptr.get();
            if (mbb->begin() != mbb->end())
            {
                MachineInst &last_inst = **std::prev(mbb->end());

                // 返回指令不应有后继
                if (tii->is_return(last_inst) && !mbb->successors().empty())
                {
                    MO_DEBUG("Warning: Return instruction has successors");
                }

                // 无条件跳转应该只有一个后继（除非是外部调用）
                if ((last_inst.opcode() == RISCV::JAL || last_inst.opcode() == RISCV::J) && mbb->successors().size() > 1)
                {
                    MO_DEBUG("Warning: Unconditional jump has multiple successors");
                }
            }
        }
    }

    bool RISCVTargetInstInfo::analyze_branch(
        MachineBasicBlock &mbb,
        MachineInst *terminator,
        std::unordered_set<MachineBasicBlock *> &branch_targets,
        MachineBasicBlock *&fall_through) const
    {
        // 初始化输出参数
        branch_targets.clear();
        fall_through = nullptr;

        // 1. 检查终止指令有效性
        if (!terminator || !terminator->has_flag(MIFlag::Terminator))
        {
            // 非终止指令，尝试获取fall-through块
            fall_through = mbb.next_physical_block();
            return false;
        }

        unsigned opcode = terminator->opcode();
        const auto &operands = terminator->operands();

        // 2. 根据指令类型处理分支
        switch (opcode)
        {
        // 条件分支指令 (B-type): beq, bne, blt等
        case RISCV::BEQ:
        case RISCV::BNE:
        case RISCV::BLT:
        case RISCV::BGE:
        case RISCV::BLTU:
        case RISCV::BGEU:
        {
            // 操作数格式: rs1, rs2, target_label
            if (operands.size() != 3)
                return false;

            // 提取分支目标
            const MOperand &target_op = operands[2];
            if (target_op.is_basic_block())
            {
                branch_targets.insert(target_op.basic_block());
            }
            else if (target_op.is_label())
            {
                // 通过标签查找基本块
                MachineFunction *mf = mbb.parent();
                MachineBasicBlock *target_bb = mf->get_basic_block_by_label(target_op.label());
                if (target_bb)
                    branch_targets.insert(target_bb);
            }

            // Fall-through块是物理顺序的下一个块
            fall_through = mbb.next_physical_block();
            return true;
        }

        // 无条件跳转指令 (J-type): jal
        case RISCV::JAL:
        {
            // 操作数格式: rd, target_label (rd通常为x0或ra)
            if (operands.size() != 2)
                return false;

            // 提取跳转目标
            const MOperand &target_op = operands[1];
            if (target_op.is_basic_block())
            {
                branch_targets.insert(target_op.basic_block());
            }
            else if (target_op.is_label())
            {
                MachineFunction *mf = mbb.parent();
                MachineBasicBlock *target_bb = mf->get_basic_block_by_label(target_op.label());
                if (target_bb)
                    branch_targets.insert(target_bb);
            }

            // 无条件跳转无fall-through
            fall_through = nullptr;
            return true;
        }

        // 间接跳转指令 (I-type): jalr
        case RISCV::JALR:
        {
            // 操作数格式: rd, offset(rs1)
            // 特殊处理返回指令 (ret伪指令)
            if (is_return(*terminator))
            {
                // ret指令无后继
                fall_through = nullptr;
                return false;
            }

            // 无法静态解析间接跳转目标
            fall_through = nullptr;
            return false;
        }

        // 伪指令: j (等效于jal x0, target)
        case RISCV::J:
        {
            if (operands.size() != 1)
                return false;

            const MOperand &target_op = operands[0];
            if (target_op.is_basic_block())
            {
                branch_targets.insert(target_op.basic_block());
            }
            else if (target_op.is_label())
            {
                MachineFunction *mf = mbb.parent();
                MachineBasicBlock *target_bb = mf->get_basic_block_by_label(target_op.label());
                if (target_bb)
                    branch_targets.insert(target_bb);
            }

            fall_through = nullptr;
            return true;
        }

        // 函数调用伪指令: call
        case RISCV::CALL:
        {
            // 展开后的call通常包含多个指令，此处处理未展开的伪指令
            if (operands.size() != 1)
                return false;

            const MOperand &target_op = operands[0];
            if (target_op.is_basic_block())
            {
                branch_targets.insert(target_op.basic_block());
            }
            else if (target_op.is_label())
            {
                MachineFunction *mf = mbb.parent();
                MachineBasicBlock *target_bb = mf->get_basic_block_by_label(target_op.label());
                if (target_bb)
                    branch_targets.insert(target_bb);
            }

            // 函数调用后的下一条指令是fall-through
            fall_through = mbb.next_physical_block();
            return true;
        }

        // 返回指令: ret (jalr x0, 0(x1))
        case RISCV::RET:
        {
            fall_through = nullptr;
            return false;
        }

        default:
        {
            // 未知终止指令，尝试获取fall-through
            fall_through = mbb.next_physical_block();
            return false;
        }
        }
    }

} // namespace RISCV
