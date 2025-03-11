#ifndef ASIMOV_VM_H
#define ASIMOV_VM_H

#include "../targets/asimov_target.h"
#include <array>
#include <iostream>

namespace ASIMOV
{
    class ASIMOVVM
    {
        friend class ASIMOVVMTest;
    public:
        ASIMOVVM(size_t memory_size = 4096); // 默认内存大小为4K字节
        ~ASIMOVVM() {}

        void load_program(const std::vector<uint32_t> &program, uint32_t start_address = 0);
        void run(uint32_t start_address = 0);

        // 调试辅助函数
        void dump_registers() const;
        void dump_memory(uint32_t start_address, size_t size) const;

    private:
        size_t memory_size_;
        std::vector<uint8_t> memory_;

        std::array<int32_t, Reg::TOTAL_REG> registers_;

        uint32_t pc_;
    
        void execute_instruction(uint32_t instruction);

        uint32_t read_memory(uint32_t address) const;
        void write_memory(uint32_t address, uint32_t value);
        int32_t read_register(Reg reg) const;
        void write_register(Reg reg, int32_t value);
    };

} // namespace ASIMOV

#endif // ASIMOV_VM_H
