// asimov_target.h - A SImple MOdel Validation target for testing

#ifndef ASIMOV_TARGET_H
#define ASIMOV_TARGET_H

#include "../machine.h"
#include <array>
#include <vector>
#include <bitset>
#include <string>
#include <stdexcept>

namespace ASIMOV
{

    // 寄存器类别定义
    enum RegClass : unsigned
    {
        GR32 = 0, // 32位通用寄存器
        FP32 = 1, // 32位浮点寄存器
        TOTAL_RC = 2
    };

    // 寄存器枚举定义
    enum Reg : unsigned
    {
        // 整型寄存器 (R0-R7)
        R0 = 0, // 返回值寄存器
        R1,
        R2,
        R3,
        R4,
        R5,
        R6, // 栈指针寄存器
        R7,
        // 浮点寄存器 (F0-F7)
        F0 = 8,
        F1,
        F2,
        F3,
        F4,
        F5,
        F6,
        F7,

        TOTAL_REG
    };

    // 指令操作码定义
    enum Opcode : unsigned
    {
        // 整数算术指令
        ADD,
        SUB,
        MUL,
        DIV,
        // 浮点算术指令
        FADD,
        FSUB,
        FMUL,
        FDIV,
        // 内存指令
        LOAD,
        STORE,
        // 控制流指令
        JMP,
        JZ,
        JNZ,
        // 立即数指令
        MOV,
        MOVD,
        // 其他指令
        NOP,
        HALT = 0xFF,
        // 伪指令
        RET,
        CALL
    };

    // 指令类型分类
    enum OpType
    {
        OP_TYPE_R, // 寄存器操作（ADD等）
        OP_TYPE_I, // 立即数操作（MOV）
        OP_TYPE_J, // 直接跳转（JMP）
        OP_TYPE_B, // 条件跳转（JZ/JNZ）
        OP_TYPE_M  // 内存访问（LOAD/STORE）
    };

    // 寄存器信息类
    class ASIMOVRegisterInfo : public TargetRegisterInfo
    {
    public:
        explicit ASIMOVRegisterInfo();

    private:
        void initializeRegisters();
        void initializeRegisterClasses();
        void initializeCallingConventions();

        std::array<RegisterDesc, TOTAL_REG> reg_descs_;
        std::vector<std::vector<unsigned>> callee_saved_map_;
        std::vector<std::vector<unsigned>> caller_saved_map_;
    };

    // 目标指令信息类
    class ASIMOVTargetInstInfo : public TargetInstInfo
    {
    public:
        ASIMOVTargetInstInfo();

        // 实现接口方法
        const char *opcode_name(unsigned opcode) const override;
        unsigned get_inst_size(const MachineInst &MI) const override;
        bool verify_instruction(const MachineInst &MI, std::string &error_msg) const override;
        uint32_t get_binary_encoding(const MachineInst &MI) const override;
        void expand_pseudo(MachineBasicBlock &mbb,
                           MachineBasicBlock::iterator MI) const override;
        bool is_return(const MachineInst &MI) const override;
        bool is_call(const MachineInst &MI) const override;
        bool is_legal_immediate(int64_t imm, unsigned size) const override;

        // 操作数类型判断
        bool is_operand_def(unsigned op, unsigned index) const override;
        bool is_operand_use(unsigned op, unsigned index) const override;

    private:
        // 编码辅助函数
        uint32_t encode_R(unsigned opcode, const MachineInst &MI) const;
        uint32_t encode_I(unsigned opcode, const MachineInst &MI) const;
        uint32_t encode_M(unsigned opcode, const MachineInst &MI) const;
        uint32_t encode_J(unsigned opcode, const MachineInst &MI) const;
        uint32_t encode_B(unsigned opcode, const MachineInst &MI) const;
    };

    // 操作码到指令类型映射
    OpType opcode_to_type(ASIMOV::Opcode op);

    // 操作码到字符串映射
    const char *opcode_to_str(ASIMOV::Opcode op);

} // namespace ASIMOV

#endif // ASIMOV_TARGET_H
