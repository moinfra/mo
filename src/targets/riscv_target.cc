
#include "riscv_target.h"

//===----------------------------------------------------------------------===//
// RISCVRegisterInfo Implementation
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
    for (unsigned reg = RISCVReg::ZERO; reg <= RISCVReg::T6; ++reg)
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
        RISCVReg::ZERO, // 零寄存器
        RISCVReg::SP,   // 栈指针
        RISCVReg::GP,   // 全局指针
        RISCVReg::TP    // 线程指针
    };

    for (auto reg : reserved_int_regs)
    {
        reg_descs_[reg].is_reserved = true;
        reg_descs_[reg].is_allocatable = false;
    }

    // 配置被调用者保存的整型寄存器 (s0-s11)
    const std::vector<unsigned> callee_saved_int = {
        RISCVReg::S0, RISCVReg::S1,
        RISCVReg::S2, RISCVReg::S3, RISCVReg::S4, RISCVReg::S5,
        RISCVReg::S6, RISCVReg::S7, RISCVReg::S8, RISCVReg::S9,
        RISCVReg::S10, RISCVReg::S11};

    for (auto reg : callee_saved_int)
    {
        reg_descs_[reg].is_callee_saved = true;
        reg_descs_[reg].spill_cost = 10; // 更高的溢出代价
    }

    // 初始化浮点寄存器 (f0-f31)，根据ABI是否支持浮点
    bool has_float = (abi == ABIVersion::ILP32F || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D);

    if (has_float)
    {
        for (unsigned reg = RISCVReg::F0; reg <= RISCVReg::F31; ++reg)
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
            RISCVReg::F8, RISCVReg::F9,
            RISCVReg::F18, RISCVReg::F19, RISCVReg::F20, RISCVReg::F21,
            RISCVReg::F22, RISCVReg::F23, RISCVReg::F24, RISCVReg::F25,
            RISCVReg::F26, RISCVReg::F27};

        for (auto reg : callee_saved_fp)
        {
            reg_descs_[reg].is_callee_saved = true;
            reg_descs_[reg].spill_cost = 12;
        }
    }
    else
    {
        // 如果不支持浮点，则标记所有浮点寄存器为不可分配
        for (unsigned reg = RISCVReg::F0; reg <= RISCVReg::F31; ++reg)
        {
            reg_descs_[reg].is_reserved = true;
            reg_descs_[reg].is_allocatable = false;
        }
    }
}

// 初始化寄存器类
void RISCVRegisterInfo::initializeRegisterClasses(ABIVersion abi)
{
    // 通用寄存器类 (GPR)
    RegisterClass gpr = {
        "GPR",
        /*regs=*/{},
        /*copy_cost=*/1,
        /*weight=*/1};

    for (unsigned reg = RISCVReg::ZERO; reg <= RISCVReg::T6; ++reg)
    {
        if (reg_descs_[reg].is_allocatable && !reg_descs_[reg].is_reserved)
        {
            gpr.regs.push_back(reg);
        }
    }
    add_register_class(gpr);

    // 浮点寄存器类 (FPR)，如果支持
    bool has_float = (abi == ABIVersion::ILP32F || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D);

    if (has_float)
    {
        RegisterClass fpr = {
            (abi == ABIVersion::LP64D) ? "FPR64" : "FPR32",
            /*regs=*/{},
            /*copy_cost=*/2,
            /*weight=*/2};

        for (unsigned reg = RISCVReg::F0; reg <= RISCVReg::F31; ++reg)
        {
            if (reg_descs_[reg].is_allocatable && !reg_descs_[reg].is_reserved)
            {
                fpr.regs.push_back(reg);
            }
        }
        add_register_class(fpr);
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
        RISCVReg::S0, RISCVReg::S1,
        RISCVReg::S2, RISCVReg::S3, RISCVReg::S4, RISCVReg::S5,
        RISCVReg::S6, RISCVReg::S7, RISCVReg::S8, RISCVReg::S9,
        RISCVReg::S10, RISCVReg::S11};

    // 整型调用者保存寄存器
    std::vector<unsigned> caller_saved = {
        RISCVReg::RA,
        RISCVReg::T0, RISCVReg::T1, RISCVReg::T2,
        RISCVReg::A0, RISCVReg::A1, RISCVReg::A2, RISCVReg::A3,
        RISCVReg::A4, RISCVReg::A5, RISCVReg::A6, RISCVReg::A7,
        RISCVReg::T3, RISCVReg::T4, RISCVReg::T5, RISCVReg::T6};

    // 添加浮点寄存器（如果支持）
    bool has_float = (abi == ABIVersion::ILP32F || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D);

    if (has_float)
    {
        // 浮点被调用者保存寄存器
        const std::vector<unsigned> callee_saved_fp = {
            RISCVReg::F8, RISCVReg::F9,
            RISCVReg::F18, RISCVReg::F19, RISCVReg::F20, RISCVReg::F21,
            RISCVReg::F22, RISCVReg::F23, RISCVReg::F24, RISCVReg::F25,
            RISCVReg::F26, RISCVReg::F27};
        callee_saved.insert(callee_saved.end(), callee_saved_fp.begin(), callee_saved_fp.end());

        // 浮点调用者保存寄存器
        const std::vector<unsigned> caller_saved_fp = {
            RISCVReg::F0, RISCVReg::F1, RISCVReg::F2, RISCVReg::F3,
            RISCVReg::F4, RISCVReg::F5, RISCVReg::F6, RISCVReg::F7,
            RISCVReg::F10, RISCVReg::F11, RISCVReg::F12, RISCVReg::F13,
            RISCVReg::F14, RISCVReg::F15, RISCVReg::F16, RISCVReg::F17,
            RISCVReg::F28, RISCVReg::F29, RISCVReg::F30, RISCVReg::F31};
        caller_saved.insert(caller_saved.end(), caller_saved_fp.begin(), caller_saved_fp.end());
    }

    // 设置C调用约定
    callee_saved_map_[CallingConv::C] = callee_saved;
    caller_saved_map_[CallingConv::C] = caller_saved;

    // 参数传递规则
    ArgPassingRule c_rule;

    // 整型参数使用a0-a7
    c_rule.int_regs = {
        RISCVReg::A0, RISCVReg::A1, RISCVReg::A2, RISCVReg::A3,
        RISCVReg::A4, RISCVReg::A5, RISCVReg::A6, RISCVReg::A7};

    // 浮点参数使用fa0-fa7（如果支持）
    if (has_float)
    {
        c_rule.fp_regs = {
            RISCVReg::F10, RISCVReg::F11, RISCVReg::F12, RISCVReg::F13,
            RISCVReg::F14, RISCVReg::F15, RISCVReg::F16, RISCVReg::F17};
    }

    // 设置C调用约定的参数传递规则
    arg_rules_[CallingConv::C] = c_rule;
}

//===----------------------------------------------------------------------===//
// Instruction Selection: RISCVTargetInstInfo Implementation
//===----------------------------------------------------------------------===//

RISCVTargetInstInfo::RISCVTargetInstInfo(ABIVersion abi)
    : abi_version_(abi)
{
    // 初始化指令名称映射
    opcode_names_ = {
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
        {RISCV::J, "j"}};

    // 如果支持浮点，添加浮点指令
    bool has_float = (abi == ABIVersion::ILP32F || abi == ABIVersion::LP64F || abi == ABIVersion::LP64D);
    if (has_float)
    {
        // 单精度浮点指令
        opcode_names_[RISCV::FLW] = "flw";
        opcode_names_[RISCV::FSW] = "fsw";
        opcode_names_[RISCV::FADD_S] = "fadd.s";
        opcode_names_[RISCV::FSUB_S] = "fsub.s";
        opcode_names_[RISCV::FMUL_S] = "fmul.s";
        opcode_names_[RISCV::FDIV_S] = "fdiv.s";
    }

    // 如果支持双精度浮点，添加双精度浮点指令
    if (abi == ABIVersion::LP64D)
    {
        opcode_names_[RISCV::FLD] = "fld";
        opcode_names_[RISCV::FSD] = "fsd";
        opcode_names_[RISCV::FADD_D] = "fadd.d";
        opcode_names_[RISCV::FSUB_D] = "fsub.d";
        opcode_names_[RISCV::FMUL_D] = "fmul.d";
        opcode_names_[RISCV::FDIV_D] = "fdiv.d";
    }

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

const char *RISCVTargetInstInfo::get_opcode_name(unsigned opcode) const
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
    return MI.opcode() == RISCV::CALL || (MI.opcode() == RISCV::JAL && MI.operands()[0].reg() == RISCVReg::RA);
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
    if (dest_reg >= RISCVReg::ZERO && dest_reg <= RISCVReg::T6 &&
        src_reg >= RISCVReg::ZERO && src_reg <= RISCVReg::T6)
    {
        // 整型寄存器之间的复制
        auto copy_inst = new MachineInst(RISCV::ADD, {MOperand::create_reg(dest_reg),
                                                      MOperand::create_reg(src_reg),
                                                      MOperand::create_reg(RISCVReg::ZERO)});
        mii = MBB.insert(mii, std::unique_ptr<MachineInst>(copy_inst));
        mii++;
    }
    else if (dest_reg >= RISCVReg::F0 && dest_reg <= RISCVReg::F31 &&
             src_reg >= RISCVReg::F0 && src_reg <= RISCVReg::F31)
    {
        // 浮点寄存器之间的复制
        if (abi_version_ == ABIVersion::LP64D)
        {
            // 双精度复制
            auto copy_inst = new MachineInst(RISCV::FADD_D, {MOperand::create_reg(dest_reg),
                                                             MOperand::create_reg(src_reg),
                                                             MOperand::create_reg(src_reg)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(copy_inst));
            mii++;
        }
        else
        {
            // 单精度复制
            auto copy_inst = new MachineInst(RISCV::FADD_S, {MOperand::create_reg(dest_reg),
                                                             MOperand::create_reg(src_reg),
                                                             MOperand::create_reg(src_reg)});
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

    // 检查并调整立即数范围
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
                    auto lui = new MachineInst(RISCV::LUI, {MOperand::create_reg(dest_reg),
                                                            MOperand::create_imm(hi)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(lui));
                    mii++;

                    // 更新当前指令为ADDI
                    auto addi = new MachineInst(RISCV::ADDI, {MI.operands()[0],
                                                              MI.operands()[1],
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
                    unsigned temp_reg = RISCVReg::T0; // 使用T0作为临时寄存器

                    // 先处理低32位
                    int64_t lower32 = imm & 0xFFFFFFFF;
                    int64_t hi_lower, lo_lower;
                    split_imm(lower32, hi_lower, lo_lower);

                    // 构建低32位
                    auto lui = new MachineInst(RISCV::LUI, {MOperand::create_reg(dest_reg),
                                                            MOperand::create_imm(hi_lower)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(lui));
                    mii++;

                    auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg),
                                                              MOperand::create_reg(dest_reg),
                                                              MOperand::create_imm(lo_lower)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(addi));
                    mii++;

                    // 处理高32位
                    int64_t upper32 = imm >> 32;

                    // 加载高32位到临时寄存器
                    int64_t hi_upper, lo_upper;
                    split_imm(upper32, hi_upper, lo_upper);

                    auto lui_upper = new MachineInst(RISCV::LUI, {MOperand::create_reg(temp_reg),
                                                                  MOperand::create_imm(hi_upper)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(lui_upper));
                    mii++;

                    auto addi_upper = new MachineInst(RISCV::ADDI, {MOperand::create_reg(temp_reg),
                                                                    MOperand::create_reg(temp_reg),
                                                                    MOperand::create_imm(lo_upper)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(addi_upper));
                    mii++;

                    // 将低32位左移32位
                    auto slli = new MachineInst(RISCV::SLLI, {MOperand::create_reg(dest_reg),
                                                              MOperand::create_reg(dest_reg),
                                                              MOperand::create_imm(32)});
                    mii = mbb.insert(mii, std::unique_ptr<MachineInst>(slli));
                    mii++;

                    // 将高32位与左移后的低32位进行OR操作
                    auto orr = new MachineInst(RISCV::OR, {MOperand::create_reg(dest_reg),
                                                           MOperand::create_reg(dest_reg),
                                                           MOperand::create_reg(temp_reg)});
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
        auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg),
                                                  MOperand::create_reg(RISCVReg::ZERO),
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
            auto lui = new MachineInst(RISCV::LUI, {MOperand::create_reg(dest_reg),
                                                    MOperand::create_imm(hi)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(lui));
            mii++;
            auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg),
                                                      MOperand::create_reg(dest_reg),
                                                      MOperand::create_imm(lo)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(addi));
            mii++;
        }
        else
        {
            // 处理64位立即数 - 优化实现
            unsigned temp_reg = RISCVReg::T0; // 使用T0作为临时寄存器

            // 先处理低32位
            int64_t lower32 = imm & 0xFFFFFFFF;
            int64_t hi_lower, lo_lower;
            split_imm(lower32, hi_lower, lo_lower);

            auto lui = new MachineInst(RISCV::LUI, {MOperand::create_reg(dest_reg),
                                                    MOperand::create_imm(hi_lower)});
            mii = MBB.insert(mii, std::unique_ptr<MachineInst>(lui));
            mii++;

            auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg),
                                                      MOperand::create_reg(dest_reg),
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

                auto lui_upper = new MachineInst(RISCV::LUI, {MOperand::create_reg(temp_reg),
                                                              MOperand::create_imm(hi_upper)});
                mii = MBB.insert(mii, std::unique_ptr<MachineInst>(lui_upper));
                mii++;

                auto addi_upper = new MachineInst(RISCV::ADDI, {MOperand::create_reg(temp_reg),
                                                                MOperand::create_reg(temp_reg),
                                                                MOperand::create_imm(lo_upper)});
                mii = MBB.insert(mii, std::unique_ptr<MachineInst>(addi_upper));
                mii++;

                // 将低32位左移32位
                auto slli = new MachineInst(RISCV::SLLI, {MOperand::create_reg(dest_reg),
                                                          MOperand::create_reg(dest_reg),
                                                          MOperand::create_imm(32)});
                mii = MBB.insert(mii, std::unique_ptr<MachineInst>(slli));
                mii++;

                // 将高32位与左移后的低32位进行OR操作
                auto orr = new MachineInst(RISCV::OR, {MOperand::create_reg(dest_reg),
                                                       MOperand::create_reg(dest_reg),
                                                       MOperand::create_reg(temp_reg)});
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
                                                   MOperand::create_reg(dest_reg),
                                                   MOperand::create_imm(0) // 重定位器会填充正确的值
                                               });

    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(auipc));
    mii++;
    // 创建addi加载低12位偏移
    auto addi = new MachineInst(RISCV::ADDI, {
                                                 MOperand::create_reg(dest_reg),
                                                 MOperand::create_reg(dest_reg),
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

    auto addi = new MachineInst(RISCV::ADDI, {MOperand::create_reg(dest_reg),
                                              MOperand::create_reg(src_reg),
                                              MOperand::create_imm(0)});

    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(addi));
    mii++;
    MBB.erase(mii);
}

void RISCVTargetInstInfo::expand_call(MachineBasicBlock &MBB, MachineBasicBlock::iterator mii) const
{
    MachineInst &MI = **mii;
    // 调用函数伪指令 - 展开为 AUIPC + JALR 序列

    // 创建auipc加载高20位地址
    auto auipc = new MachineInst(RISCV::AUIPC, {
                                                   MOperand::create_reg(RISCVReg::T0), // 使用临时寄存器
                                                   MOperand::create_imm(0)             // 重定位器会填充正确的值
                                               });

    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(auipc));
    mii++;
    // 创建jalr进行调用
    auto jalr = new MachineInst(RISCV::JALR, {
                                                 MOperand::create_reg(RISCVReg::RA), // 保存返回地址
                                                 MOperand::create_reg(RISCVReg::T0),
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
                                               MOperand::create_reg(RISCVReg::ZERO), // 不保存返回地址
                                               MI.operands()[0]                      // 直接使用原始偏移量操作数
                                           });
    mii = MBB.insert(mii, std::unique_ptr<MachineInst>(jal));
    mii++;
    MBB.erase(mii);
}
