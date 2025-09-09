// simple_instruction_selection_test.cc - Test simple IR to RISC-V instruction mapping

#include <gtest/gtest.h>
#include <memory>
#include <string>

#include "../src/ir.h"
#include "../src/ir_builder.h"
#include "../src/machine.h"
#include "../src/targets/riscv_target.h"
#include "../src/targets/riscv_instruction_selector.h"
#include "../src/targets/riscv_assembly_emitter.h"
#include "../src/codegen/machine_module.h"

class SimpleInstructionSelectionTest : public ::testing::Test {
protected:
    std::unique_ptr<Module> createTestModule() {
        return std::make_unique<Module>("test_module");
    }
    
    // Helper to create a simple function with two i32 parameters
    Function* createBinaryOpFunction(Module* module, const std::string& name) {
        auto int_type = module->get_integer_type(32);
        auto func_type = module->get_function_type(int_type, {int_type, int_type});
        return module->create_function(name, func_type);
    }
};

TEST_F(SimpleInstructionSelectionTest, AddInstructionMapping) {
    // Test IR: %2 = add i32 %0, %1 → RISC-V: add vreg_result, vreg_a, vreg_b
    auto module = createTestModule();
    auto* func = createBinaryOpFunction(module.get(), "test_add");
    
    auto* entry = func->create_basic_block("entry");
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    // Create: %result = add i32 %arg0, %arg1; ret %result
    auto* add_inst = builder.create_add(func->arg(0), func->arg(1));
    builder.create_ret(add_inst);
    
    // Convert to machine code
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // Generate assembly to verify instruction
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    std::string assembly = emitter->emit(*machine_module);
    
    // Verify assembly contains ADD instruction
    EXPECT_NE(assembly.find("add vreg"), std::string::npos);
    
    std::cout << "ADD Instruction Test Assembly:\n" << assembly << std::endl;
}

TEST_F(SimpleInstructionSelectionTest, SubInstructionMapping) {
    // Test IR: %2 = sub i32 %0, %1 → RISC-V: sub vreg_result, vreg_a, vreg_b
    auto module = createTestModule();
    auto* func = createBinaryOpFunction(module.get(), "test_sub");
    
    auto* entry = func->create_basic_block("entry");
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    // Create: %result = sub i32 %arg0, %arg1; ret %result
    auto* sub_inst = builder.create_sub(func->arg(0), func->arg(1));
    builder.create_ret(sub_inst);
    
    // Convert to machine code
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // Generate assembly to verify instruction
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    std::string assembly = emitter->emit(*machine_module);
    
    // Verify assembly contains SUB instruction
    EXPECT_NE(assembly.find("sub vreg"), std::string::npos);
    
    std::cout << "SUB Instruction Test Assembly:\n" << assembly << std::endl;
}

TEST_F(SimpleInstructionSelectionTest, MulInstructionMapping) {
    // Test IR: %2 = mul i32 %0, %1 → RISC-V: mul vreg_result, vreg_a, vreg_b
    auto module = createTestModule();
    auto* func = createBinaryOpFunction(module.get(), "test_mul");
    
    auto* entry = func->create_basic_block("entry");
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    // Create: %result = mul i32 %arg0, %arg1; ret %result
    auto* mul_inst = builder.create_mul(func->arg(0), func->arg(1));
    builder.create_ret(mul_inst);
    
    // Convert to machine code
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // Generate assembly to verify instruction
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    std::string assembly = emitter->emit(*machine_module);
    
    // Verify assembly contains MUL instruction
    EXPECT_NE(assembly.find("mul vreg"), std::string::npos);
    
    std::cout << "MUL Instruction Test Assembly:\n" << assembly << std::endl;
}

TEST_F(SimpleInstructionSelectionTest, DivInstructionMapping) {
    // Test IR: %2 = sdiv i32 %0, %1 → RISC-V: div vreg_result, vreg_a, vreg_b
    auto module = createTestModule();
    auto* func = createBinaryOpFunction(module.get(), "test_div");
    
    auto* entry = func->create_basic_block("entry");
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    // Create: %result = sdiv i32 %arg0, %arg1; ret %result
    auto* div_inst = builder.create_sdiv(func->arg(0), func->arg(1));
    builder.create_ret(div_inst);
    
    // Convert to machine code
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // Generate assembly to verify instruction
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    std::string assembly = emitter->emit(*machine_module);
    
    // Verify assembly contains DIV instruction
    EXPECT_NE(assembly.find("div vreg"), std::string::npos);
    
    std::cout << "DIV Instruction Test Assembly:\n" << assembly << std::endl;
}

TEST_F(SimpleInstructionSelectionTest, AllArithmeticOperations) {
    // Test function that uses all four operations: add, sub, mul, div
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {int_type, int_type});
    auto* func = module->create_function("test_all_ops", func_type);
    
    auto* entry = func->create_basic_block("entry");
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    // Create complex expression: ((a + b) - (a * b)) / (a - b)
    auto* add_result = builder.create_add(func->arg(0), func->arg(1));    // a + b
    auto* mul_result = builder.create_mul(func->arg(0), func->arg(1));    // a * b  
    auto* sub1_result = builder.create_sub(add_result, mul_result);       // (a+b) - (a*b)
    auto* sub2_result = builder.create_sub(func->arg(0), func->arg(1));   // a - b
    auto* div_result = builder.create_sdiv(sub1_result, sub2_result);     // result / (a-b)
    
    builder.create_ret(div_result);
    
    // Convert to machine code
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // Generate assembly to verify all instructions
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    std::string assembly = emitter->emit(*machine_module);
    
    // Verify assembly contains all four instruction types
    EXPECT_NE(assembly.find("add vreg"), std::string::npos);
    EXPECT_NE(assembly.find("sub vreg"), std::string::npos);
    EXPECT_NE(assembly.find("mul vreg"), std::string::npos);
    EXPECT_NE(assembly.find("div vreg"), std::string::npos);
    
    std::cout << "All Operations Test Assembly:\n" << assembly << std::endl;
}