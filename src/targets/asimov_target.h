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

    const size_t SIZE_DWORD = 4;
    const size_t SIZE_WORD = 2;
    const size_t ALIGN = 4;

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
        R6, // SP 栈指针寄存器
        R7, // BP 基址指针寄存器（不强制要求）
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
        ADD,         // add rd, rs1, rs2 (R-type)
        SUB,         // sub rd, rs1, rs2 (R-type)
        MUL,         // mul rd, rs1, rs2 (R-type)
        DIV,         // div rd, rs1, rs2 (R-type)
        FADD,        // fadd rd, rs1, rs2 (R-type)
        FSUB,        // fsub rd, rs1, rs2 (R-type)
        FMUL,        // fmul rd, rs1, rs2 (R-type)
        FDIV,        // fdiv rd, rs1, rs2 (R-type)
        LOAD,        // load rd, offset(rs1) (M-type)
        STORE,       // store rs1, offset(rs2) (M-type)
        JMP,         // jmp target (J-type)
        JZ,          // jz rs1, target (B-type)
        JNZ,         // jnz rs1, target (B-type)
        MOVW,        // mov rd, imm (I-type)
        MOVD,        // movd rd; (next dword) imm (I-type)
        NOP,         // nop (无操作数)
        HALT = 0xFF, // halt (无操作数)
        // 伪指令
        MOV,  // 根据 imm 展开为 movw 或 movd
        CMP,  // cmp rd, rs1, rs2 (R-type) 展开为 sub rd, rs1, rs2
        RET,  // ret (无操作数)
        CALL, // call target (J-type)
    };
    // 指令类型分类
    enum OpType
    {
        OP_TYPE_R, // 寄存器操作（ADD等）
        OP_TYPE_I, // 立即数操作（MOV）
        OP_TYPE_J, // 直接跳转（JMP）
        OP_TYPE_B, // 条件跳转（JZ/JNZ）
        OP_TYPE_M, // 内存访问（LOAD/STORE）
    };

    constexpr std::array<Reg, 2> RESERVED_REGS = {
        Reg::R6, // 栈指针
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

        unsigned get_instruction_latency(unsigned opcode) const override;

        void copy_phys_reg(MachineBasicBlock &mbb, MachineBasicBlock::iterator insert,
                           unsigned dest_reg,
                           unsigned src_reg) const override;
        bool legalize_inst(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii,
                           MachineFunction &mf) const override;

        void insert_load_from_stack(MachineBasicBlock &mbb, MachineBasicBlock::iterator insert_point,
                                    unsigned dest_reg, int frame_index,
                                    int64_t offset) const override;
        void insert_store_to_stack(MachineBasicBlock &mbb, MachineBasicBlock::iterator insert_point,
                                   unsigned src_reg, int frame_index,
                                   int64_t offset) const override;
        bool analyze_branch(
            MachineBasicBlock &mbb,
            MachineInst *terminator,
            std::unordered_set<MachineBasicBlock *> &branch_targets,
            MachineBasicBlock *&fall_through) const override;

    private:
        // 编码辅助函数
        uint32_t encode_R(unsigned opcode, const MachineInst &MI) const;
        uint32_t encode_I(unsigned opcode, const MachineInst &MI) const;
        uint32_t encode_M(unsigned opcode, const MachineInst &MI) const;
        uint32_t encode_J(unsigned opcode, const MachineInst &MI) const;
        uint32_t encode_B(unsigned opcode, const MachineInst &MI) const;
    }; // class ASIMOVTargetInstInfo

    class ASIMOVFrameLowering : public TargetFrameLowering
    {
    public:
        void emit_prologue(MachineFunction &mf) const override
        {
            MachineFrame &frame = *mf.frame();
            int stack_size = frame.get_total_frame_size();

            if (stack_size > 0)
            {
                // SUB R6, R6, stack_size
                auto &mbb = *mf.basic_blocks().front();
                auto mi = std::make_unique<MachineInst>(SUB);
                mi->add_operand(MOperand::create_reg(R6, true));
                mi->add_operand(MOperand::create_reg(R6));
                mi->add_operand(MOperand::create_imm(stack_size));
                mbb.insert(mbb.begin(), std::move(mi));
            }
        }

        void emit_epilogue(MachineFunction &mf) const override
        {
            MachineFrame &frame = *mf.frame();
            int stack_size = frame.get_total_frame_size();

            if (stack_size > 0)
            {
                // ADD R6, R6, stack_size
                auto &mbb = *mf.basic_blocks().back();
                auto mi = std::make_unique<MachineInst>(ADD);
                mi->add_operand(MOperand::create_reg(R6, true));
                mi->add_operand(MOperand::create_reg(R6));
                mi->add_operand(MOperand::create_imm(stack_size));
                mbb.insert(std::prev(mbb.end()), std::move(mi));
            }
        }
    }; // class ASIMOVFrameLowering

    // 操作码到指令类型映射
    OpType opcode_to_type(ASIMOV::Opcode op);

    // 操作码到字符串映射
    const char *opcode_to_str(ASIMOV::Opcode op);

} // namespace ASIMOV

#endif // ASIMOV_TARGET_H
