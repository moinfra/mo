// end_to_end_basic_test.cc - End-to-end integration tests

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

class EndToEndBasicTest : public ::testing::Test {
protected:
    std::unique_ptr<Module> createTestModule() {
        return std::make_unique<Module>("test_module");
    }
};

TEST_F(EndToEndBasicTest, SimpleAddFunction) {
    // Phase 1.5 - Commit 5.4: End-to-end integration test
    // 创建IR: 
    // define i32 @test_add(i32 %a, i32 %b) {
    //   %result = add i32 %a, %b
    //   ret i32 %result
    // }
    
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {int_type, int_type});
    auto* func = module->create_function("test_add", func_type);
    
    auto* entry = func->create_basic_block("entry");
    
    // Use IRBuilder to create instructions
    IRBuilder builder(module.get());
    builder.set_insert_point(entry);
    
    auto* add_inst = builder.create_add(func->arg(0), func->arg(1));
    builder.create_ret(add_inst);
    
    // 指令选择
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    machine_module->set_target_info(reg_info.get(), inst_info.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // 汇编生成
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    
    std::string assembly = emitter->emit(*machine_module);
    
    // 验证输出包含期望的指令
    EXPECT_NE(assembly.find("add vreg"), std::string::npos);
    EXPECT_NE(assembly.find("ret"), std::string::npos);
    EXPECT_NE(assembly.find(".text"), std::string::npos);
    
    // Debug output to see the actual assembly
    std::cout << "Generated Assembly:\n" << assembly << std::endl;
}

TEST_F(EndToEndBasicTest, EmptyFunction) {
    // Test empty function that just returns
    auto module = createTestModule();
    auto void_type = module->get_void_type();
    auto func_type = module->get_function_type(void_type, {});
    auto* func = module->create_function("empty_func", func_type);
    
    auto* entry = func->create_basic_block("entry");
    
    // Use IRBuilder to create instructions
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
    
    // Generate assembly
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    
    std::string assembly = emitter->emit(*machine_module);
    
    // Should contain basic assembly structure
    EXPECT_NE(assembly.find("ret"), std::string::npos);
    EXPECT_NE(assembly.find(".text"), std::string::npos);
    
    std::cout << "Empty Function Assembly:\n" << assembly << std::endl;
}