#include <vector>
#include <gtest/gtest.h>

#include "src/vm/asimov_vm.h"

namespace ASIMOV
{
    // 辅助函数：创建指令
    uint32_t asimv_inst(Opcode opcode, unsigned rd = 0, unsigned rs1 = 0, unsigned rs2 = 0, int imm = 0)
    {
        switch (opcode)
        {
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case FADD:
        case FSUB:
        case FMUL:
        case FDIV:
            return (opcode << 24) | (rd << 16) | (rs1 << 8) | rs2;
        case MOV:
            return (opcode << 24) | (rd << 16) | (imm & 0xFFFF);
        case MOVD:
            return (opcode << 24) | (rd << 16);
        case LOAD:
        case STORE:
            return (opcode << 24) | (rd << 16) | (rs1 << 8) | (imm & 0xFF);
        case JMP:
            return (opcode << 24) | (imm & 0xFFFFFF);
        case JZ:
        case JNZ:
            return (opcode << 24) | (rs1 << 16) | (imm & 0xFFFF);
        case NOP:
            return 0x00000000;
        case HALT:
            return 0xFFFFFFFF;
        case RET:
            return 0x00000001;
        default:
            MO_UNREACHABLE();
            return 0; // 未知指令
        }
    }

    // 辅助函数：比较两个浮点数是否相等
    bool float_equal(float a, float b, float epsilon)
    {
        MO_DEBUG("float_equal: a = %f, b = %f, epsilon = %f", a, b, epsilon);
        return std::abs(a - b) < epsilon;
    }

} // namespace ASIMOV

namespace ASIMOV
{

    // 测试夹具类：提供公共的虚拟机实例和辅助函数
    class ASIMOVVMTest : public ::testing::Test
    {
    protected:
        const size_t memory_size = 1024;
        ASIMOVVM vm{memory_size};

        void load_and_run(const std::vector<uint32_t> &program, uint32_t start_address = 0)
        {
            vm.load_program(program, start_address);
            vm.run(start_address);
        }

        int32_t get_register_value(Reg reg)
        {
            return vm.read_register(reg);
        }

        void set_register_value(Reg reg, int32_t value)
        {
            vm.write_register(reg, value);
        }

        uint32_t read_memory(uint32_t address)
        {
            return vm.read_memory(address);
        }

        void write_memory(uint32_t address, uint32_t value)
        {
            vm.write_memory(address, value);
        }
    };

    // 测试 ADD 指令
    TEST_F(ASIMOVVMTest, AddInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R1, 0, 0, 10),
            asimv_inst(MOV, R2, 0, 0, 20),
            asimv_inst(ADD, R0, R1, R2),
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 30);
    }

    // 测试 SUB 指令
    TEST_F(ASIMOVVMTest, SubInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R1, 0, 0, 20),
            asimv_inst(MOV, R2, 0, 0, 10),
            asimv_inst(SUB, R0, R1, R2),
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 10);
    }

    // 测试 MUL 指令
    TEST_F(ASIMOVVMTest, MulInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R1, 0, 0, 5),
            asimv_inst(MOV, R2, 0, 0, 6),
            asimv_inst(MUL, R0, R1, R2),
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 30);
    }

    // 测试 DIV 指令
    TEST_F(ASIMOVVMTest, DivInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R1, 0, 0, 30),
            asimv_inst(MOV, R2, 0, 0, 5),
            asimv_inst(DIV, R0, R1, R2),
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 6);
    }
    // 辅助函数：将浮点数转换为 IEEE 754 二进制表示
    uint32_t float_to_bits(float value)
    {
        return *reinterpret_cast<uint32_t *>(&value);
    }

    float bits_to_float(uint32_t bits)
    {
        return *reinterpret_cast<float *>(&bits);
    }

    // 测试 FADD 指令
    TEST_F(ASIMOVVMTest, FaddInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOVD, F1, 0, 0, 0), // F1 = 2.0
            float_to_bits(2.0f),
            asimv_inst(MOVD, F2, 0, 0, 0), // F2 = 3.0
            float_to_bits(3.0f),
            asimv_inst(FADD, F0, F1, F2), // F0 = F1 + F2
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_TRUE(float_equal(bits_to_float(get_register_value(F0)), 5.0f, 1e-6f));
    }

    // 测试 FSUB 指令
    TEST_F(ASIMOVVMTest, FsubInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOVD, F1, 0, 0, 0), // F1 = 3.0
            float_to_bits(3.0f),
            asimv_inst(MOVD, F2, 0, 0, 0), // F2 = 2.0
            float_to_bits(2.0f),
            asimv_inst(FSUB, F0, F1, F2), // F0 = F1 - F2
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_TRUE(float_equal(bits_to_float(get_register_value(F0)), 1.0f, 1e-6f));
    }

    // 测试 FMUL 指令
    TEST_F(ASIMOVVMTest, FmulInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOVD, F1, 0, 0, 0), // F1 = 2.0
            float_to_bits(2.0f),
            asimv_inst(MOVD, F2, 0, 0, 0), // F2 = 3.0
            float_to_bits(3.0f),
            asimv_inst(FMUL, F0, F1, F2), // F0 = F1 * F2
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_TRUE(float_equal(bits_to_float(get_register_value(F0)), 6.0f, 1e-6f));
    }

    // 测试 FDIV 指令
    TEST_F(ASIMOVVMTest, FdivInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOVD, F1, 0, 0, 0), // F1 = 5.0
            float_to_bits(5.0f),
            asimv_inst(MOVD, F2, 0, 0, 0), // F2 = 2.0
            float_to_bits(2.0f),
            asimv_inst(FDIV, F0, F1, F2), // F0 = F1 / F2
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_TRUE(float_equal(bits_to_float(get_register_value(F0)), 2.5f, 1e-6f));
    }

    // 测试 MOV 指令
    TEST_F(ASIMOVVMTest, MovInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R0, 0, 0, 12345),
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 12345);
    }

    // 测试 LOAD 和 STORE 指令
    TEST_F(ASIMOVVMTest, LoadStoreInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R1, 0, 0, 100),  // R1 = 100 (address)
            asimv_inst(MOV, R2, 0, 0, 42),   // R2 = 42 (value to store)
            asimv_inst(STORE, R2, R1, 0, 0), // [R1 + 0] = R2
            asimv_inst(LOAD, R0, R1, 0, 0),  // R0 = [R1 + 0]
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 42);
    }

    // 测试 JMP 指令
    TEST_F(ASIMOVVMTest, JmpInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(JMP, 0, 0, 0, 12), // 0:  JMP to address 12
            asimv_inst(MOV, R0, 0, 0, 1), // 4:  This should be skipped
            asimv_inst(HALT),             // 8:  This should be skipped
            asimv_inst(MOV, R0, 0, 0, 2), // 12: Jump target
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 2);
    }

    // 测试 JZ 指令
    TEST_F(ASIMOVVMTest, JzInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R1, 0, 0, 0), // 0:  R1 = 0
            asimv_inst(JZ, 0, R1, 0, 16), // 4:  JZ R1, to address 16
            asimv_inst(MOV, R0, 0, 0, 1), // 8:  This should be skipped
            asimv_inst(HALT),             // 12: This should be skipped
            asimv_inst(MOV, R0, 0, 0, 2), // 16: Jump target
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 2);
    }

    // 测试 JNZ 指令
    TEST_F(ASIMOVVMTest, JnzInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R1, 0, 0, 1),  // 0:  R1 = 1
            asimv_inst(JNZ, 0, R1, 0, 16), // 4:  JNZ R1, to address 16
            asimv_inst(MOV, R0, 0, 0, 1),  // 8:  This should be skipped
            asimv_inst(HALT),              // 12: This should be skipped
            asimv_inst(MOV, R0, 0, 0, 2),  // 16: Jump target
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 2);
    }

    // 测试 HALT 指令
    TEST_F(ASIMOVVMTest, HaltInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R0, 0, 0, 1), // R0 = 1
            asimv_inst(HALT),
            asimv_inst(MOV, R0, 0, 0, 2) // This should be skipped
        };
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 1);
    }

    // 测试内存越界访问
    TEST_F(ASIMOVVMTest, MemoryOutOfBounds)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R1, 0, 0, memory_size + 1), // R1 = memory_size_ + 1 (超出内存范围)
            asimv_inst(STORE, R2, R1, 0, 0),            // [R1 + 0] = R2 (应该抛出异常)
            asimv_inst(HALT)};
        vm.load_program(program, 0);
        EXPECT_THROW(vm.run(0), std::runtime_error);
    }

    // 测试除以零异常
    TEST_F(ASIMOVVMTest, DivideByZero)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R1, 0, 0, 10),
            asimv_inst(MOV, R2, 0, 0, 0),
            asimv_inst(DIV, R0, R1, R2), // R0 = R1 / R2 (应该输出错误信息)
            asimv_inst(HALT)};
        load_and_run(program);
        // 虚拟机实现中已经输出了错误信息，这里不需要断言什么
    }

    // 测试 NOP 指令
    TEST_F(ASIMOVVMTest, NopInstruction)
    {
        std::vector<uint32_t> program = {
            asimv_inst(MOV, R0, 0, 0, 1),
            asimv_inst(NOP),
            asimv_inst(MOV, R0, 0, 0, 2),
            asimv_inst(HALT)};
        load_and_run(program);
        EXPECT_EQ(get_register_value(R0), 2);
    }

} // namespace ASIMOV
