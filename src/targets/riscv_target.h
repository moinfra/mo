// riscv_target.h - RISC-V target definitions

#pragma once

#include "../machine.h"
#include <array>
#include <vector>
#include <bitset>
#include <string>
#include <stdexcept>

namespace RISCV
{
    enum RegClass : unsigned
    {
        GR32 = 0, // 32-bit general purpose registers
        GR64 = 1, // 64-bit general purpose registers
        FP32 = 2, // 32-bit floating point registers
        FP64 = 3, // 64-bit floating point registers
        TOTAL_RC = 4
    };

    // RISC-V寄存器枚举定义
    // https://riscv.org/wp-content/uploads/2024/12/riscv-calling.pdf
    enum Reg : unsigned
    {
        // 整型寄存器 (x0-x31)
        ZERO = 0, // x0: 零寄存器
        RA = 1,   // x1: 返回地址
        SP = 2,   // x2: 栈指针
        GP = 3,   // x3: 全局指针
        TP = 4,   // x4: 线程指针
        T0 = 5,   // x5: 临时寄存器/备用链接寄存器
        T1 = 6,   // x6: 临时寄存器
        T2 = 7,   // x7: 临时寄存器
        S0 = 8,   // x8: 保存寄存器/帧指针 S0/FP
        S1 = 9,   // x9: 保存寄存器
        A0 = 10,  // x10: 函数参数/返回值
        A1 = 11,  // x11: 函数参数/返回值
        A2 = 12,  // x12: 函数参数
        A3 = 13,  // x13: 函数参数
        A4 = 14,  // x14: 函数参数
        A5 = 15,  // x15: 函数参数
        A6 = 16,  // x16: 函数参数
        A7 = 17,  // x17: 函数参数
        S2 = 18,  // x18: 保存寄存器
        S3 = 19,  // x19: 保存寄存器
        S4 = 20,  // x20: 保存寄存器
        S5 = 21,  // x21: 保存寄存器
        S6 = 22,  // x22: 保存寄存器
        S7 = 23,  // x23: 保存寄存器
        S8 = 24,  // x24: 保存寄存器
        S9 = 25,  // x25: 保存寄存器
        S10 = 26, // x26: 保存寄存器
        S11 = 27, // x27: 保存寄存器
        T3 = 28,  // x28: 临时寄存器
        T4 = 29,  // x29: 临时寄存器
        T5 = 30,  // x30: 临时寄存器
        T6 = 31,  // x31: 临时寄存器

        // 浮点寄存器 (f0-f31)
        F0 = 32,  // f0: 浮点临时寄存器
        F1 = 33,  // f1: 浮点临时寄存器
        F2 = 34,  // f2: 浮点临时寄存器
        F3 = 35,  // f3: 浮点临时寄存器
        F4 = 36,  // f4: 浮点临时寄存器
        F5 = 37,  // f5: 浮点临时寄存器
        F6 = 38,  // f6: 浮点临时寄存器
        F7 = 39,  // f7: 浮点临时寄存器
        F8 = 40,  // f8: 浮点保存寄存器
        F9 = 41,  // f9: 浮点保存寄存器
        F10 = 42, // f10: 浮点函数参数
        F11 = 43, // f11: 浮点函数参数
        F12 = 44, // f12: 浮点函数参数
        F13 = 45, // f13: 浮点函数参数
        F14 = 46, // f14: 浮点函数参数
        F15 = 47, // f15: 浮点函数参数
        F16 = 48, // f16: 浮点函数参数
        F17 = 49, // f17: 浮点函数参数
        F18 = 50, // f18: 浮点保存寄存器
        F19 = 51, // f19: 浮点保存寄存器
        F20 = 52, // f20: 浮点保存寄存器
        F21 = 53, // f21: 浮点保存寄存器
        F22 = 54, // f22: 浮点保存寄存器
        F23 = 55, // f23: 浮点保存寄存器
        F24 = 56, // f24: 浮点保存寄存器
        F25 = 57, // f25: 浮点保存寄存器
        F26 = 58, // f26: 浮点保存寄存器
        F27 = 59, // f27: 浮点保存寄存器
        F28 = 60, // f28: 浮点临时寄存器
        F29 = 61, // f29: 浮点临时寄存器
        F30 = 62, // f30: 浮点临时寄存器
        F31 = 63, // f31: 浮点临时寄存器

        // 特殊寄存器
        PC = 64 // 程序计数器
    };

    // RISC-V ABI版本
    enum class ABIVersion
    {
        ILP32,  // 32-bit integer, no floating-point
        LP64,   // 64-bit integer, no floating-point
        ILP32F, // 32-bit with single-precision floating-point (F extension)
        LP64F,  // 64-bit with single-precision floating-point
        LP64D   // 64-bit with double-precision floating-point (D extension)
    };

    class RISCVRegisterInfo : public TargetRegisterInfo
    {
    public:
        explicit RISCVRegisterInfo(ABIVersion abi = ABIVersion::LP64D);

    private:
        void initializeRegisters(ABIVersion abi);
        void initializeRegisterClasses(ABIVersion abi);
        void initializeCallingConventions(ABIVersion abi);
    };
    // RISC-V 指令操作码定义

    enum Opcode : unsigned
    {
        // RV32I 基本指令集
        // U-type: [imm[31:12]][rd][opcode]
        LUI = 0x37, // Load Upper Immediate

        // U-type: [imm[31:12]][rd][opcode]
        AUIPC = 0x17, // Add Upper Immediate to PC

        // J-type: [imm[20|10:1|11|19:12]][rd][opcode]
        JAL = 0x6F, // Jump and Link

        // I-type: [imm[11:0]][rs1][funct3(000)][rd][opcode]
        JALR = 0x67, // Jump and Link Register

        // B-type: [imm[12|10:5]][rs2][rs1][funct3(000)][imm[4:1|11]][opcode]
        BEQ = 0x63, // Branch Equal

        // B-type: [imm[12|10:5]][rs2][rs1][funct3(001)][imm[4:1|11]][opcode]
        BNE = 0x1063, // Branch Not Equal

        // B-type: [imm[12|10:5]][rs2][rs1][funct3(100)][imm[4:1|11]][opcode]
        BLT = 0x4063, // Branch Less Than

        // B-type: [imm[12|10:5]][rs2][rs1][funct3(101)][imm[4:1|11]][opcode]
        BGE = 0x5063, // Branch Greater or Equal

        // B-type: [imm[12|10:5]][rs2][rs1][funct3(110)][imm[4:1|11]][opcode]
        BLTU = 0x6063, // Branch Less Than Unsigned

        // B-type: [imm[12|10:5]][rs2][rs1][funct3(111)][imm[4:1|11]][opcode]
        BGEU = 0x7063, // Branch Greater or Equal Unsigned

        // I-type: [imm[11:0]][rs1][funct3(000)][rd][opcode]
        LB = 0x03, // Load Byte

        // I-type: [imm[11:0]][rs1][funct3(001)][rd][opcode]
        LH = 0x1003, // Load Half-word

        // I-type: [imm[11:0]][rs1][funct3(010)][rd][opcode]
        LW = 0x2003, // Load Word

        // I-type: [imm[11:0]][rs1][funct3(100)][rd][opcode]
        LBU = 0x4003, // Load Byte Unsigned

        // I-type: [imm[11:0]][rs1][funct3(101)][rd][opcode]
        LHU = 0x5003, // Load Half-word Unsigned

        // S-type: [imm[11:5]][rs2][rs1][funct3(000)][imm[4:0]][opcode]
        SB = 0x23, // Store Byte

        // S-type: [imm[11:5]][rs2][rs1][funct3(001)][imm[4:0]][opcode]
        SH = 0x1023, // Store Half-word

        // S-type: [imm[11:5]][rs2][rs1][funct3(010)][imm[4:0]][opcode]
        SW = 0x2023, // Store Word

        // I-type: [imm[11:0]][rs1][funct3(000)][rd][opcode]
        ADDI = 0x13, // Add Immediate

        // I-type: [imm[11:0]][rs1][funct3(010)][rd][opcode]
        SLTI = 0x2013, // Set Less Than Immediate

        // I-type: [imm[11:0]][rs1][funct3(011)][rd][opcode]
        SLTIU = 0x3013, // Set Less Than Immediate Unsigned

        // I-type: [imm[11:0]][rs1][funct3(100)][rd][opcode]
        XORI = 0x4013, // XOR Immediate

        // I-type: [imm[11:0]][rs1][funct3(110)][rd][opcode]
        ORI = 0x6013, // OR Immediate

        // I-type: [imm[11:0]][rs1][funct3(111)][rd][opcode]
        ANDI = 0x7013, // AND Immediate

        // I-type: [funct7(0000000)][shamt][rs1][funct3(001)][rd][opcode]
        SLLI = 0x1013, // Shift Left Logical Immediate

        // I-type: [funct7(0000000)][shamt][rs1][funct3(101)][rd][opcode]
        SRLI = 0x5013, // Shift Right Logical Immediate

        // I-type: [funct7(0100000)][shamt][rs1][funct3(101)][rd][opcode]
        SRAI = 0x40005013, // Shift Right Arithmetic Immediate

        // R-type: [funct7(0000000)][rs2][rs1][funct3(000)][rd][opcode]
        ADD = 0x33, // Add

        // R-type: [funct7(0100000)][rs2][rs1][funct3(000)][rd][opcode]
        SUB = 0x40000033, // Subtract

        // R-type: [funct7(0000000)][rs2][rs1][funct3(001)][rd][opcode]
        SLL = 0x1033, // Shift Left Logical

        // R-type: [funct7(0000000)][rs2][rs1][funct3(010)][rd][opcode]
        SLT = 0x2033, // Set Less Than

        // R-type: [funct7(0000000)][rs2][rs1][funct3(011)][rd][opcode]
        SLTU = 0x3033, // Set Less Than Unsigned

        // R-type: [funct7(0000000)][rs2][rs1][funct3(100)][rd][opcode]
        XOR = 0x4033, // XOR

        // R-type: [funct7(0000000)][rs2][rs1][funct3(101)][rd][opcode]
        SRL = 0x5033, // Shift Right Logical

        // R-type: [funct7(0100000)][rs2][rs1][funct3(101)][rd][opcode]
        SRA = 0x40005033, // Shift Right Arithmetic

        // R-type: [funct7(0000000)][rs2][rs1][funct3(110)][rd][opcode]
        OR = 0x6033, // OR

        // R-type: [funct7(0000000)][rs2][rs1][funct3(111)][rd][opcode]
        AND = 0x7033, // AND

        // RV32F/RV64F 浮点指令
        // I-type: [imm[11:0]][rs1][funct3(010)][rd][opcode]
        FLW = 0x2007, // Float Load Word

        // S-type: [imm[11:5]][rs2][rs1][funct3(010)][imm[4:0]][opcode]
        FSW = 0x2027, // Float Store Word

        // R4-type: [rs3][fmt][rs2][rs1][rm][rd][opcode]
        FADD_S = 0x53, // Float Add (Single)

        // R4-type: [rs3][fmt][rs2][rs1][rm][rd][opcode]
        FSUB_S = 0x08000053, // Float Subtract (Single)

        // R4-type: [rs3][fmt][rs2][rs1][rm][rd][opcode]
        FMUL_S = 0x10000053, // Float Multiply (Single)

        // R4-type: [rs3][fmt][rs2][rs1][rm][rd][opcode]
        FDIV_S = 0x18000053, // Float Divide (Single)

        // RV32D/RV64D 双精度浮点指令
        // I-type: [imm[11:0]][rs1][funct3(011)][rd][opcode]
        FLD = 0x3007, // Float Load Double

        // S-type: [imm[11:5]][rs2][rs1][funct3(011)][imm[4:0]][opcode]
        FSD = 0x3027, // Float Store Double

        // R4-type: [rs3][fmt][rs2][rs1][rm][rd][opcode]
        FADD_D = 0x02000053, // Float Add (Double)

        // R4-type: [rs3][fmt][rs2][rs1][rm][rd][opcode]
        FSUB_D = 0x0A000053, // Float Subtract (Double)

        // R4-type: [rs3][fmt][rs2][rs1][rm][rd][opcode]
        FMUL_D = 0x12000053, // Float Multiply (Double)

        // R4-type: [rs3][fmt][rs2][rs1][rm][rd][opcode]
        FDIV_D = 0x1A000053, // Float Divide (Double)

        // 伪指令操作码
        // I-type (JALR): [imm=0][rs1(x1)][funct3(000)][rd(x0)][opcode]
        RET = 0x8067, // 返回指令 (jalr x0, 0(x1))

        // I-type (ADDI): [imm=0][rs1(x0)][funct3(000)][rd(x0)][opcode]
        NOP = 0x13FA1E, // 空操作 (addi x0, x0, 0)

        // U-type (LUI) + I-type (ADDI) 组合实现
        LI = 0x90000013, // 加载立即数 (伪指令)

        // R-type (ADDI): [funct7][rs2(x0)][rs1][funct3][rd][opcode]
        MV = 0x90000033, // 移动寄存器 (伪指令)

        // U-type (AUIPC) + I-type (ADDI) 组合实现
        LA = 0x90000017, // 加载地址 (伪指令)

        // J-type (JAL): [imm][rd(x1)][opcode]
        CALL = 0x9000006F, // 调用函数 (伪指令)

        // J-type (JAL): [imm][rd(x0)][opcode]
        J = 0x9000007F // 无条件跳转 (伪指令)
    };

    enum OpType
    {
        OP_TYPE_R,
        OP_TYPE_I,
        OP_TYPE_S,
        OP_TYPE_B,
        OP_TYPE_U,
        OP_TYPE_J,
        OP_TYPE_R4
    };

    OpType opcode_to_type(RISCV::Opcode op)
    {
        switch (op)
        {
        // U-type 指令
        case RISCV::LUI:
        case RISCV::AUIPC:
        case RISCV::LA: // 伪指令（AUIPC实现）
            return OpType::OP_TYPE_U;

        // J-type 指令
        case RISCV::JAL:
        case RISCV::CALL: // 伪指令（JAL实现）
        case RISCV::J:    // 伪指令（JAL实现）
            return OpType::OP_TYPE_J;

        // I-type 指令
        case RISCV::JALR:
        case RISCV::LB:
        case RISCV::LH:
        case RISCV::LW:
        case RISCV::LBU:
        case RISCV::LHU:
        case RISCV::ADDI:
        case RISCV::SLTI:
        case RISCV::SLTIU:
        case RISCV::XORI:
        case RISCV::ORI:
        case RISCV::ANDI:
        case RISCV::SLLI:
        case RISCV::SRLI:
        case RISCV::SRAI:
        case RISCV::RET: // 伪指令（JALR实现）
        case RISCV::NOP: // 伪指令（ADDI实现）
        case RISCV::LI:  // 伪指令（ADDI实现）
        case RISCV::FLW:
        case RISCV::FLD:
            return OpType::OP_TYPE_I;

        // B-type 指令
        case RISCV::BEQ:
        case RISCV::BNE:
        case RISCV::BLT:
        case RISCV::BGE:
        case RISCV::BLTU:
        case RISCV::BGEU:
            return OpType::OP_TYPE_B;

        // S-type 指令
        case RISCV::SB:
        case RISCV::SH:
        case RISCV::SW:
        case RISCV::FSW:
        case RISCV::FSD:
            return OpType::OP_TYPE_S;

        // R-type 指令
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
        case RISCV::MV: // 伪指令（ADD实现）
            return OpType::OP_TYPE_R;

        // R4-type 浮点运算指令
        case RISCV::FADD_S:
        case RISCV::FSUB_S:
        case RISCV::FMUL_S:
        case RISCV::FDIV_S:
        case RISCV::FADD_D:
        case RISCV::FSUB_D:
        case RISCV::FMUL_D:
        case RISCV::FDIV_D:
            return OpType::OP_TYPE_R4;

        default:
            throw std::invalid_argument("Unknown opcode");
        }
    }

    const char *opcode_to_str(RISCV::Opcode op)
    {
        switch (op)
        {
        case RISCV::LUI:
            return "LUI";
        case RISCV::AUIPC:
            return "AUIPC";
        case RISCV::LA:
            return "LA";
        case RISCV::JAL:
            return "JAL";
        case RISCV::CALL:
            return "CALL";
        case RISCV::J:
            return "J";
        case RISCV::JALR:
            return "JALR";
        case RISCV::LB:
            return "LB";
        case RISCV::LH:
            return "LH";
        case RISCV::LW:
            return "LW";
        case RISCV::LBU:
            return "LBU";
        case RISCV::LHU:
            return "LHU";
        case RISCV::ADDI:
            return "ADDI";
        case RISCV::SLTI:
            return "SLTI";
        case RISCV::SLTIU:
            return "SLTIU";
        case RISCV::XORI:
            return "XORI";
        case RISCV::ORI:
            return "ORI";
        case RISCV::ANDI:
            return "ANDI";
        case RISCV::SLLI:
            return "SLLI";
        case RISCV::SRLI:
            return "SRLI";
        case RISCV::SRAI:
            return "SRAI";
        case RISCV::RET:
            return "RET";
        case RISCV::NOP:
            return "NOP";
        case RISCV::LI:
            return "LI";
        case RISCV::FLW:
            return "FLW";
        case RISCV::FLD:
            return "FLD";
        case RISCV::BEQ:
            return "BEQ";
        case RISCV::BNE:
            return "BNE";
        case RISCV::BLT:
            return "BLT";
        case RISCV::BGE:
            return "BGE";
        case RISCV::BLTU:
            return "BLTU";
        case RISCV::BGEU:
            return "BGEU";
        case RISCV::SB:
            return "SB";
        case RISCV::SH:
            return "SH";
        case RISCV::SW:
            return "SW";
        case RISCV::FSW:
            return "FSW";
        case RISCV::FSD:
            return "FSD";
        case RISCV::ADD:
            return "ADD";
        case RISCV::SUB:
            return "SUB";
        case RISCV::SLL:
            return "SLL";
        case RISCV::SLT:
            return "SLT";
        case RISCV::SLTU:
            return "SLTU";
        case RISCV::XOR:
            return "XOR";
        case RISCV::SRL:
            return "SRL";
        case RISCV::SRA:
            return "SRA";
        case RISCV::OR:
            return "OR";
        case RISCV::AND:
            return "AND";
        case RISCV::MV:
            return "MV";
        case RISCV::FADD_S:
            return "FADD_S";
        case RISCV::FSUB_S:
            return "FSUB_S";
        case RISCV::FMUL_S:
            return "FMUL_S";
        case RISCV::FDIV_S:
            return "FDIV_S";
        case RISCV::FADD_D:
            return "FADD_D";
        case RISCV::FSUB_D:
            return "FSUB_D";
        case RISCV::FMUL_D:
            return "FMUL_D";
        case RISCV::FDIV_D:
            return "FDIV_D";
        default:
            throw std::invalid_argument("Unknown opcode");
        }
    }

    class RISCVTargetInstInfo : public TargetInstInfo
    {
    public:
        RISCVTargetInstInfo(ABIVersion abi = ABIVersion::LP64D);

        // TargetInstInfo接口实现
        const char *opcode_name(unsigned opcode) const override;
        unsigned get_inst_size(const MachineInst &MI) const override;
        bool verify_instruction(const MachineInst &MI, std::string &error_msg) const override;
        uint32_t get_binary_encoding(const MachineInst &MI) const override;
        void expand_pseudo(MachineBasicBlock &mbb,
                           MachineBasicBlock::iterator MI) const override;
        bool is_return(const MachineInst &MI) const override;
        bool is_call(const MachineInst &MI) const override;
        bool is_legal_immediate(int64_t imm, unsigned operand_size) const override;

        bool is_operand_def(unsigned op, unsigned index) const override
        {
            return is_op_type_def(opcode_to_type(static_cast<RISCV::Opcode>(op)), index);
        }
        bool is_op_type_def(OpType type, unsigned index) const
        {
            if (index < 0)
                return false;

            switch (type)
            {
            case OpType::OP_TYPE_U: // LUI/AUIPC: rd,imm
            case OpType::OP_TYPE_J: // JAL: rd,imm
                return index == 0;
            case OpType::OP_TYPE_I:  // ADDI/LW: rd,rs1,imm
            case OpType::OP_TYPE_R:  // ADD/SUB: rd,rs1,rs2
            case OpType::OP_TYPE_R4: // FADD.D: rd,rs1,rs2,rs3
                return index == 0;
            default:
                return false; // B/S-type和其他未知类型
            }
        }

        bool is_operand_use(unsigned op, unsigned index) const override
        {
            return is_op_type_use(opcode_to_type(static_cast<RISCV::Opcode>(op)), index);
        }
        bool is_op_type_use(OpType type, unsigned index) const
        {
            if (index < 0)
                return false;

            switch (type)
            {
            case OpType::OP_TYPE_I: // ADDI/LW: rd,rs1,imm
                return index == 1;
            case OpType::OP_TYPE_B: // BEQ: rs1,rs2,imm
            case OpType::OP_TYPE_S: // SW: rs2,imm(rs1)
                return index <= 1;
            case OpType::OP_TYPE_R: // ADD/SUB: rd,rs1,rs2
                return index == 1 || index == 2;
            case OpType::OP_TYPE_R4: // FADD.D: rd,rs1,rs2,rs3
                return index >= 1 && index <= 3;
            default:
                return false; // U/J-type和其他未知类型
            }
        }

        unsigned get_instruction_latency(unsigned opcode) const override;
        void copy_phys_reg(MachineBasicBlock &mbb,
                           MachineBasicBlock::iterator insert,
                           unsigned dest_reg, unsigned src_reg) const override;
        bool legalize_inst(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii,
                           MachineFunction &MF) const override;

        // Implement stack load/store methods
        MachineBasicBlock::iterator insert_load_from_stack(MachineBasicBlock &mbb,
                                                           MachineBasicBlock::iterator insert_point,
                                                           unsigned dest_reg, int frame_index,
                                                           int64_t offset = 0) const override;

        MachineBasicBlock::iterator insert_store_to_stack(MachineBasicBlock &mbb,
                                                          MachineBasicBlock::iterator insert_point,
                                                          unsigned src_reg, int frame_index,
                                                          int64_t offset = 0) const override;
        bool analyze_branch(
            MachineBasicBlock &mbb,
            MachineInst *terminator,
            std::unordered_set<MachineBasicBlock *> &branch_targets,
            MachineBasicBlock *&fall_through) const override;

    private:
        // 指令编码辅助函数
        uint32_t encode_R(uint32_t opcode, uint32_t funct3, uint32_t funct7,
                          const MachineInst &MI) const;
        uint32_t encode_I(uint32_t opcode, uint32_t funct3,
                          const MachineInst &MI) const;
        uint32_t encode_S(uint32_t opcode, uint32_t funct3,
                          const MachineInst &MI) const;
        uint32_t encode_B(uint32_t opcode, uint32_t funct3,
                          const MachineInst &MI) const;
        uint32_t encode_U(uint32_t opcode, const MachineInst &MI) const;
        uint32_t encode_J(uint32_t opcode, const MachineInst &MI) const;

        // 立即数处理
        int64_t adjust_imm(int64_t imm, unsigned bits) const;
        bool split_imm(int64_t imm, int64_t &hi, int64_t &lo) const;

        // 伪指令扩展实现
        void expand_li(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii) const;
        void expand_la(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii) const;
        void expand_mv(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii) const;
        void expand_call(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii) const;
        void expand_j(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii) const;

        // 当前目标ABI版本
        ABIVersion abi_version_;
        // 指令名称映射
        // 指令延迟表
        std::map<unsigned, unsigned> inst_latency_;
    };

    void build_cfg_from_instructions(MachineFunction &mf);
}; // namespace RISCV
