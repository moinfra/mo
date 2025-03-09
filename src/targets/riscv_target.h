// riscv_target.h - RISC-V target definitions

#pragma once

#include "machine.h"
#include <array>
#include <vector>
#include <bitset>

// RISC-V寄存器枚举定义
namespace RISCVReg
{
    enum : unsigned
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
        S0 = 8,   // x8: 保存寄存器/帧指针
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
}

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

    std::array<RegisterDesc, 65> reg_descs_; // 64个寄存器 + PC
    std::vector<std::vector<unsigned>> callee_saved_map_;
    std::vector<std::vector<unsigned>> caller_saved_map_;
};

// RISC-V 指令操作码定义
namespace RISCV
{
    enum Opcode : unsigned
    {
        // RV32I 基本指令集
        LUI = 0x37,        // Load Upper Immediate
        AUIPC = 0x17,      // Add Upper Immediate to PC
        JAL = 0x6F,        // Jump and Link
        JALR = 0x67,       // Jump and Link Register
        BEQ = 0x63,        // Branch Equal
        BNE = 0x1063,      // Branch Not Equal
        BLT = 0x4063,      // Branch Less Than
        BGE = 0x5063,      // Branch Greater or Equal
        BLTU = 0x6063,     // Branch Less Than Unsigned
        BGEU = 0x7063,     // Branch Greater or Equal Unsigned
        LB = 0x03,         // Load Byte
        LH = 0x1003,       // Load Half-word
        LW = 0x2003,       // Load Word
        LBU = 0x4003,      // Load Byte Unsigned
        LHU = 0x5003,      // Load Half-word Unsigned
        SB = 0x23,         // Store Byte
        SH = 0x1023,       // Store Half-word
        SW = 0x2023,       // Store Word
        ADDI = 0x13,       // Add Immediate
        SLTI = 0x2013,     // Set Less Than Immediate
        SLTIU = 0x3013,    // Set Less Than Immediate Unsigned
        XORI = 0x4013,     // XOR Immediate
        ORI = 0x6013,      // OR Immediate
        ANDI = 0x7013,     // AND Immediate
        SLLI = 0x1013,     // Shift Left Logical Immediate
        SRLI = 0x5013,     // Shift Right Logical Immediate
        SRAI = 0x40005013, // Shift Right Arithmetic Immediate
        ADD = 0x33,        // Add
        SUB = 0x40000033,  // Subtract
        SLL = 0x1033,      // Shift Left Logical
        SLT = 0x2033,      // Set Less Than
        SLTU = 0x3033,     // Set Less Than Unsigned
        XOR = 0x4033,      // XOR
        SRL = 0x5033,      // Shift Right Logical
        SRA = 0x40005033,  // Shift Right Arithmetic
        OR = 0x6033,       // OR
        AND = 0x7033,      // AND

        // RV32F/RV64F 浮点指令
        FLW = 0x2007,        // Float Load Word
        FSW = 0x2027,        // Float Store Word
        FADD_S = 0x53,       // Float Add (Single)
        FSUB_S = 0x08000053, // Float Subtract (Single)
        FMUL_S = 0x10000053, // Float Multiply (Single)
        FDIV_S = 0x18000053, // Float Divide (Single)

        // RV32D/RV64D 双精度浮点指令
        FLD = 0x3007,        // Float Load Double
        FSD = 0x3027,        // Float Store Double
        FADD_D = 0x02000053, // Float Add (Double)
        FSUB_D = 0x0A000053, // Float Subtract (Double)
        FMUL_D = 0x12000053, // Float Multiply (Double)
        FDIV_D = 0x1A000053, // Float Divide (Double)

        // 伪指令操作码
        RET = 0x8067,      // 返回指令 (jalr x0, 0(x1))
        NOP = 0x13FA1E,    // 空操作 (addi x0, x0, 0)
        LI = 0x90000013,   // 加载立即数 (伪指令)
        MV = 0x90000033,   // 移动寄存器 (伪指令)
        LA = 0x90000017,   // 加载地址 (伪指令)
        CALL = 0x9000006F, // 调用函数 (伪指令)
        J = 0x9000007F     // 无条件跳转 (伪指令)
    };
} // namespace RISCV

class RISCVTargetInstInfo : public TargetInstInfo
{
public:
    RISCVTargetInstInfo(ABIVersion abi = ABIVersion::LP64D);

    // TargetInstInfo接口实现
    const char *get_opcode_name(unsigned opcode) const override;
    unsigned get_inst_size(const MachineInst &MI) const override;
    bool verify_instruction(const MachineInst &MI, std::string &error_msg) const override;
    uint32_t get_binary_encoding(const MachineInst &MI) const override;
    void expand_pseudo(MachineBasicBlock &mbb,
                       MachineBasicBlock::iterator MI) const override;
    bool is_return(const MachineInst &MI) const override;
    bool is_call(const MachineInst &MI) const override;
    bool is_legal_immediate(int64_t imm, unsigned operand_size) const override;
    unsigned get_instruction_latency(unsigned opcode) const override;
    void copy_phys_reg(MachineBasicBlock &mbb,
                       MachineBasicBlock::iterator insert,
                       unsigned dest_reg, unsigned src_reg) const override;
    bool legalize_inst(MachineBasicBlock &mbb,MachineBasicBlock::iterator mii,
                       MachineFunction &MF) const override;

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
    std::map<unsigned, std::string> opcode_names_;
    // 指令延迟表
    std::map<unsigned, unsigned> inst_latency_;
};
