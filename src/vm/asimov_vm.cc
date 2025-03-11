#include <iostream>
#include <stdexcept>
#include <vector>
#include <sstream>

#include "asimov_vm.h"
#include "../mo_debug.h"

namespace ASIMOV
{

    ASIMOVVM::ASIMOVVM(size_t memory_size) : memory_size_(memory_size), memory_(memory_size), pc_(0)
    {
        MO_ASSERT(memory_size % 4 == 0, "Memory size must be a multiple of 4");
        MO_ASSERT(memory_.size() == memory_size, "Memory allocation failed");
        MO_DEBUG("ASIMOVVM initialized with memory size %lu bytes, PC=0x%08X", memory_size, pc_);

        // 初始化寄存器
        registers_.fill(0);
    }

    void ASIMOVVM::load_program(const std::vector<uint32_t> &program, uint32_t start_address)
    {
        if (start_address + program.size() * 4 > memory_size_)
        {
            throw std::runtime_error("Program too large to fit in memory");
        }

        for (size_t i = 0; i < program.size(); ++i)
        {
            write_memory(start_address + i * 4, program[i]);
        }

        pc_ = start_address;
    }

    void ASIMOVVM::run(uint32_t start_address)
    {
        pc_ = start_address;

        MO_DEBUG("Starting execution at PC=0x%08X", start_address);

        while (true)
        {
            // 1. Fetch
            uint32_t instruction = read_memory(pc_);

            // 2. Decode and Execute
            execute_instruction(instruction);

            // 3. Update PC (unless the instruction modifies it)
            //    HALT 指令会停止执行
            if (instruction == 0xFFFFFFFF)
            {
                break;
            }

            if (pc_ == memory_size_ - 4)
            {
                MO_WARN("PC overflowed, stopping execution");
                break;
            }

            pc_ += 4;
        }
    }

    bool is_int_register(Reg reg)
    {
        unsigned reg_num = static_cast<unsigned>(reg);
        return reg_num < static_cast<unsigned>(Reg::F0);
    }

    bool is_float_register(Reg reg)
    {
        unsigned reg_num = static_cast<unsigned>(reg);
        return reg_num >= static_cast<unsigned>(Reg::F0) && reg_num < static_cast<unsigned>(Reg::TOTAL_REG);
    }

    // 验证整型寄存器
    void validate_int_register(Reg reg)
    {
        if (!is_int_register(reg))
        {
            std::stringstream ss;
            ss << "Register " << static_cast<unsigned>(reg) << " is not an integer register";
            throw std::runtime_error(ss.str());
        }
    }

    // 验证浮点寄存器
    void validate_float_register(Reg reg)
    {
        if (!is_float_register(reg))
        {
            std::stringstream ss;
            ss << "Register " << static_cast<unsigned>(reg) << " is not a float register";
            throw std::runtime_error(ss.str());
        }
    }

    void ASIMOVVM::execute_instruction(uint32_t instruction)
    {
        Opcode opcode = static_cast<Opcode>((instruction >> 24) & 0xFF);
        MO_DEBUG("Executing instruction: %s", opcode_to_str(opcode));
        switch (opcode)
        {
        case ADD:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;
            unsigned rs2 = instruction & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);
            Reg rs2_reg = static_cast<Reg>(rs2);

            validate_int_register(rd_reg);
            validate_int_register(rs1_reg);
            validate_int_register(rs2_reg);

            int32_t val1 = read_register(rs1_reg);
            int32_t val2 = read_register(rs2_reg);
            write_register(rd_reg, val1 + val2);
            break;
        }
        case SUB:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;
            unsigned rs2 = instruction & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);
            Reg rs2_reg = static_cast<Reg>(rs2);

            validate_int_register(rd_reg);
            validate_int_register(rs1_reg);
            validate_int_register(rs2_reg);

            int32_t val1 = read_register(rs1_reg);
            int32_t val2 = read_register(rs2_reg);
            write_register(rd_reg, val1 - val2);
            break;
        }
        case MUL:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;
            unsigned rs2 = instruction & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);
            Reg rs2_reg = static_cast<Reg>(rs2);

            validate_int_register(rd_reg);
            validate_int_register(rs1_reg);
            validate_int_register(rs2_reg);

            int32_t val1 = read_register(rs1_reg);
            int32_t val2 = read_register(rs2_reg);
            write_register(rd_reg, val1 * val2);
            break;
        }
        case DIV:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;
            unsigned rs2 = instruction & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);
            Reg rs2_reg = static_cast<Reg>(rs2);

            validate_int_register(rd_reg);
            validate_int_register(rs1_reg);
            validate_int_register(rs2_reg);

            int32_t val1 = read_register(rs1_reg);
            int32_t val2 = read_register(rs2_reg);
            if (val2 == 0)
            {
                std::cerr << "Division by zero!" << std::endl;
                break;
            }
            write_register(rd_reg, val1 / val2);
            break;
        }
        case FADD:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;
            unsigned rs2 = instruction & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);
            Reg rs2_reg = static_cast<Reg>(rs2);

            validate_float_register(rd_reg);
            validate_float_register(rs1_reg);
            validate_float_register(rs2_reg);

            uint32_t v1 = read_register(rs1_reg);
            float val1 = *reinterpret_cast<float *>(&v1);
            uint32_t v2 = read_register(rs2_reg);
            float val2 = *reinterpret_cast<float *>(&v2);
            float result = val1 + val2;
            write_register(rd_reg, *reinterpret_cast<uint32_t *>(&result));
            break;
        }
        case FSUB:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;
            unsigned rs2 = instruction & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);
            Reg rs2_reg = static_cast<Reg>(rs2);

            validate_float_register(rd_reg);
            validate_float_register(rs1_reg);
            validate_float_register(rs2_reg);

            uint32_t v1 = read_register(rs1_reg);
            float val1 = *reinterpret_cast<float *>(&v1);
            uint32_t v2 = read_register(rs2_reg);
            float val2 = *reinterpret_cast<float *>(&v2);
            float result = val1 - val2;
            write_register(rd_reg, *reinterpret_cast<uint32_t *>(&result));
            break;
        }
        case FMUL:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;
            unsigned rs2 = instruction & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);
            Reg rs2_reg = static_cast<Reg>(rs2);

            validate_float_register(rd_reg);
            validate_float_register(rs1_reg);
            validate_float_register(rs2_reg);

            uint32_t v1 = read_register(rs1_reg);
            float val1 = *reinterpret_cast<float *>(&v1);
            uint32_t v2 = read_register(rs2_reg);
            float val2 = *reinterpret_cast<float *>(&v2);
            float result = val1 * val2;
            write_register(rd_reg, *reinterpret_cast<uint32_t *>(&result));
            break;
        }
        case FDIV:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;
            unsigned rs2 = instruction & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);
            Reg rs2_reg = static_cast<Reg>(rs2);

            validate_float_register(rd_reg);
            validate_float_register(rs1_reg);
            validate_float_register(rs2_reg);

            uint32_t v1 = read_register(rs1_reg);
            float val1 = *reinterpret_cast<float *>(&v1);
            uint32_t v2 = read_register(rs2_reg);
            float val2 = *reinterpret_cast<float *>(&v2);
            if (val2 == 0.0f)
            {
                std::cerr << "Division by zero!" << std::endl;
                break;
            }
            float result = val1 / val2;
            write_register(rd_reg, *reinterpret_cast<uint32_t *>(&result));
            break;
        }
        case MOV:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            Reg rd_reg = static_cast<Reg>(rd);
            validate_int_register(rd_reg);

            int32_t imm = instruction & 0xFFFF;
            write_register(rd_reg, imm);
            break;
        }
        case MOVD:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            Reg rd_reg = static_cast<Reg>(rd);

            uint32_t imm = read_memory(pc_ + 4);
            // 直接存储原始32位值到寄存器
            write_register(rd_reg, imm);
            pc_ += 4; // 跳过立即数字
            break;
        }
        case LOAD:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);

            validate_int_register(rd_reg);
            validate_int_register(rs1_reg);

            int32_t offset = instruction & 0xFF;
            int32_t address = read_register(rs1_reg) + offset;
            int32_t value = read_memory(address);
            write_register(rd_reg, value);
            break;
        }
        case STORE:
        {
            unsigned rd = (instruction >> 16) & 0xFF;
            unsigned rs1 = (instruction >> 8) & 0xFF;

            Reg rd_reg = static_cast<Reg>(rd);
            Reg rs1_reg = static_cast<Reg>(rs1);

            validate_int_register(rd_reg);
            validate_int_register(rs1_reg);

            int32_t offset = instruction & 0xFF;
            int32_t address = read_register(rs1_reg) + offset;
            int32_t value = read_register(rd_reg);
            write_memory(address, value);
            break;
        }
        case JMP:
        {
            int32_t target = instruction & 0xFFFFFF;
            if (target % 4 != 0)
            {
                throw std::runtime_error("JMP target must be a multiple of 4");
            }
            pc_ = target - 4; // 下一次循环会 +4
            break;
        }
        case JZ:
        {
            unsigned rs1 = (instruction >> 16) & 0xFF;
            Reg rs1_reg = static_cast<Reg>(rs1);
            validate_int_register(rs1_reg);

            int32_t target = instruction & 0xFFFF;
            if (target % 4 != 0)
            {
                throw std::runtime_error("JZ target must be a multiple of 4");
            }
            if (read_register(rs1_reg) == 0)
            {
                pc_ = target - 4; // 下一次循环会 +4
            }
            break;
        }
        case JNZ:
        {
            unsigned rs1 = (instruction >> 16) & 0xFF;
            Reg rs1_reg = static_cast<Reg>(rs1);
            validate_int_register(rs1_reg);

            int32_t target = instruction & 0xFFFF;
            if (target % 4 != 0)
            {
                throw std::runtime_error("JNZ target must be a multiple of 4");
            }
            if (read_register(rs1_reg) != 0)
            {
                pc_ = target - 4; // 下一次循环会 +4
            }
            break;
        }
        case HALT:
            return;
        case RET:
            MO_ERROR("Pseudo-instruction RET should not be executed");
            return;
        default:
            std::cerr << "Unknown opcode: " << std::hex << opcode << "H"
                      << std::dec << "(" << opcode << ")" << std::endl;
            MO_UNREACHABLE();
            break;
        }
    }

    uint32_t ASIMOVVM::read_memory(uint32_t address) const
    {
        if (address + 4 > memory_size_)
        {
            std::stringstream ss;
            ss << "Memory address " << std::hex << address << "H (" << std::dec
               << address << ") cannot be read (out of range)";
            MO_ERROR(ss.str().c_str());
            throw std::runtime_error(ss.str());
        }

        return (memory_[address] << 24) | (memory_[address + 1] << 16) |
               (memory_[address + 2] << 8) | memory_[address + 3];
    }

    void ASIMOVVM::write_memory(uint32_t address, uint32_t value)
    {
        if (address + 4 > memory_size_)
        {
            std::stringstream ss;
            ss << "Memory address " << std::hex << "H (" << std::dec
               << address << ") cannot be written (out of range)";
            MO_ERROR(ss.str().c_str());
            throw std::runtime_error(ss.str());
        }

        memory_[address] = (value >> 24) & 0xFF;
        memory_[address + 1] = (value >> 16) & 0xFF;
        memory_[address + 2] = (value >> 8) & 0xFF;
        memory_[address + 3] = value & 0xFF;
    }

    int32_t ASIMOVVM::read_register(Reg reg) const
    {
        return registers_[reg];
    }

    void ASIMOVVM::write_register(Reg reg, int32_t value)
    {
        registers_[reg] = value;
    }

    void ASIMOVVM::dump_registers() const
    {
        std::cout << "Registers:" << std::endl;
        for (int i = 0; i < static_cast<int>(Reg::TOTAL_REG); ++i)
        {
            std::cout << "R" << i << ": " << registers_[i] << std::endl;
        }
    }

    void ASIMOVVM::dump_memory(uint32_t start_address, size_t size) const
    {
        std::cout << "Memory from " << start_address << " to " << start_address + size << ":" << std::endl;
        for (size_t i = 0; i < size; ++i)
        {
            std::cout << std::hex << (int)memory_[start_address + i] << " ";
        }

        std::cout << std::endl;
    }

} // namespace ASIMOV
