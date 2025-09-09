// function_lowering_test.cc - Test function prologue/epilogue and calling convention

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

class FunctionLoweringTest : public ::testing::Test {
protected:
    std::unique_ptr<Module> createTestModule() {
        return std::make_unique<Module>("test_module");
    }
};

TEST_F(FunctionLoweringTest, EmptyFunctionPrologueEpilogue) {
    // Test empty function generates correct prologue/epilogue
    // Expected assembly:
    // function:
    //   addi sp, sp, -16    ; prologue: allocate stack frame
    //   sw ra, 12(sp)       ; prologue: save return address
    //   lw ra, 12(sp)       ; epilogue: restore return address  
    //   addi sp, sp, 16     ; epilogue: deallocate stack frame
    //   ret                 ; return
    
    auto module = createTestModule();
    auto void_type = module->get_void_type();
    auto func_type = module->get_function_type(void_type, {});
    auto* func = module->create_function("empty_func", func_type);
    
    auto* entry = func->create_basic_block("entry");
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    builder.create_ret_void();
    
    // Convert to machine code
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // Generate assembly to verify prologue/epilogue
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    std::string assembly = emitter->emit(*machine_module);
    
    // Verify prologue instructions
    EXPECT_NE(assembly.find("addi sp, sp, -16"), std::string::npos); // Allocate frame
    EXPECT_NE(assembly.find("sw ra, 12(sp)"), std::string::npos);    // Save return address
    
    // Verify epilogue instructions
    EXPECT_NE(assembly.find("lw ra, 12(sp)"), std::string::npos);    // Restore return address
    EXPECT_NE(assembly.find("addi sp, sp, 16"), std::string::npos);  // Deallocate frame
    
    // Verify return instruction
    EXPECT_NE(assembly.find("ret"), std::string::npos);
    
    std::cout << "Empty Function Prologue/Epilogue Test Assembly:\n" << assembly << std::endl;
}

TEST_F(FunctionLoweringTest, SimpleReturnValueFunction) {
    // Test function with return value has correct calling convention
    // Expected: return value moved to a0 before epilogue
    
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {});
    auto* func = module->create_function("return_42", func_type);
    
    auto* entry = func->create_basic_block("entry");
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    // Create: return 42;
    auto* const_42 = module->get_constant_int(int_type, 42);
    builder.create_ret(const_42);
    
    // Convert to machine code
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // Generate assembly
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    std::string assembly = emitter->emit(*machine_module);
    
    // Verify prologue
    EXPECT_NE(assembly.find("addi sp, sp, -16"), std::string::npos);
    EXPECT_NE(assembly.find("sw ra, 12(sp)"), std::string::npos);
    
    // Verify return value is moved to a0 (RISC-V calling convention)
    EXPECT_NE(assembly.find("add a0,"), std::string::npos); // mv a0, value
    
    // Verify epilogue
    EXPECT_NE(assembly.find("lw ra, 12(sp)"), std::string::npos);
    EXPECT_NE(assembly.find("addi sp, sp, 16"), std::string::npos);
    EXPECT_NE(assembly.find("ret"), std::string::npos);
    
    std::cout << "Return Value Function Test Assembly:\n" << assembly << std::endl;
}

TEST_F(FunctionLoweringTest, SimpleParameterFunction) {
    // Test function with parameters uses correct calling convention
    // RISC-V ABI: first 8 integer arguments passed in a0-a7 registers
    
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {int_type, int_type});
    auto* func = module->create_function("add_params", func_type);
    
    auto* entry = func->create_basic_block("entry");
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    // Create: return arg0 + arg1;
    auto* add_result = builder.create_add(func->arg(0), func->arg(1));
    builder.create_ret(add_result);
    
    // Convert to machine code
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // Generate assembly
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    std::string assembly = emitter->emit(*machine_module);
    
    // Verify prologue/epilogue are present
    EXPECT_NE(assembly.find("addi sp, sp, -16"), std::string::npos);
    EXPECT_NE(assembly.find("sw ra, 12(sp)"), std::string::npos);
    EXPECT_NE(assembly.find("lw ra, 12(sp)"), std::string::npos);
    EXPECT_NE(assembly.find("addi sp, sp, 16"), std::string::npos);
    
    // Verify add instruction is present  
    EXPECT_NE(assembly.find("add vreg"), std::string::npos);
    
    // Verify return value handling
    EXPECT_NE(assembly.find("add a0,"), std::string::npos);
    EXPECT_NE(assembly.find("ret"), std::string::npos);
    
    std::cout << "Parameter Function Test Assembly:\n" << assembly << std::endl;
}

TEST_F(FunctionLoweringTest, ComplexFunctionWithCalculations) {
    // Test more complex function with multiple operations
    // Should still have correct prologue/epilogue
    
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {int_type, int_type, int_type});
    auto* func = module->create_function("complex_calc", func_type);
    
    auto* entry = func->create_basic_block("entry");
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    // Create: return (a + b) * c;
    auto* add_result = builder.create_add(func->arg(0), func->arg(1));
    auto* mul_result = builder.create_mul(add_result, func->arg(2));
    builder.create_ret(mul_result);
    
    // Convert to machine code
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // Generate assembly
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    std::string assembly = emitter->emit(*machine_module);
    
    // Verify prologue/epilogue structure
    EXPECT_NE(assembly.find("addi sp, sp, -16"), std::string::npos);
    EXPECT_NE(assembly.find("sw ra, 12(sp)"), std::string::npos);
    EXPECT_NE(assembly.find("lw ra, 12(sp)"), std::string::npos);
    EXPECT_NE(assembly.find("addi sp, sp, 16"), std::string::npos);
    EXPECT_NE(assembly.find("ret"), std::string::npos);
    
    // Verify computation instructions
    EXPECT_NE(assembly.find("add vreg"), std::string::npos);
    EXPECT_NE(assembly.find("mul vreg"), std::string::npos);
    
    // Verify return value handling
    EXPECT_NE(assembly.find("add a0,"), std::string::npos);
    
    std::cout << "Complex Function Test Assembly:\n" << assembly << std::endl;
}