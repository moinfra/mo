#include <gtest/gtest.h>
#include <memory>

#include "src/codegen/machine_module.h"
#include "src/ir.h"
#include "src/machine.h"
#include "src/targets/riscv_target.h"

using namespace codegen;

class MachineModuleTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Create a minimal IR module for testing
        ir_module = std::make_unique<Module>("test_module");
        
        // Create a simple function for testing
        auto int_type = ir_module->get_integer_type(32);
        auto func_type = ir_module->get_function_type(int_type, {});
        test_function = ir_module->create_function("test_func", func_type);
        
        // Create RISC-V target components for testing target info
        auto abi_version = RISCV::ABIVersion::LP64D;
        reg_info = std::make_unique<RISCV::RISCVRegisterInfo>(abi_version);
        inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>(abi_version);
    }
    
    std::unique_ptr<Module> ir_module;
    Function* test_function = nullptr;
    std::unique_ptr<RISCV::RISCVRegisterInfo> reg_info;
    std::unique_ptr<RISCV::RISCVTargetInstInfo> inst_info;
};

// Test CodeGenModule creation
TEST_F(MachineModuleTest, Creation) {
    auto codegen_module = std::make_unique<CodeGenModule>(ir_module.get());
    
    ASSERT_NE(codegen_module, nullptr);
    EXPECT_EQ(codegen_module->getIRModule(), ir_module.get());
    EXPECT_NE(codegen_module->getMachineModule(), nullptr);
    EXPECT_TRUE(codegen_module->empty());
    EXPECT_EQ(codegen_module->getFunctionCount(), 0);
}

// Test adding functions
TEST_F(MachineModuleTest, AddFunction) {
    auto codegen_module = std::make_unique<CodeGenModule>(ir_module.get());
    
    // Initially empty
    EXPECT_TRUE(codegen_module->empty());
    EXPECT_EQ(codegen_module->getFunctionCount(), 0);
    
    // Add a function
    auto* machine_func = codegen_module->addFunction(test_function);
    
    EXPECT_NE(machine_func, nullptr);
    EXPECT_FALSE(codegen_module->empty());
    EXPECT_EQ(codegen_module->getFunctionCount(), 1);
}

// Test target info setting
TEST_F(MachineModuleTest, TargetInfo) {
    auto codegen_module = std::make_unique<CodeGenModule>(ir_module.get());
    
    // Initially no target info
    EXPECT_EQ(codegen_module->getTargetRegisterInfo(), nullptr);
    EXPECT_EQ(codegen_module->getTargetInstInfo(), nullptr);
    
    // Set target info
    codegen_module->setTargetInfo(reg_info.get(), inst_info.get());
    
    // Verify target info is set
    EXPECT_EQ(codegen_module->getTargetRegisterInfo(), reg_info.get());
    EXPECT_EQ(codegen_module->getTargetInstInfo(), inst_info.get());
    
    // Verify underlying MachineModule has target info
    auto* machine_module = codegen_module->getMachineModule();
    ASSERT_NE(machine_module, nullptr);
    EXPECT_EQ(machine_module->target_reg_info(), reg_info.get());
    EXPECT_EQ(machine_module->target_inst_info(), inst_info.get());
}

// Test MachineModule release
TEST_F(MachineModuleTest, MachineModuleRelease) {
    auto codegen_module = std::make_unique<CodeGenModule>(ir_module.get());
    
    // Add a function to make it non-empty
    codegen_module->addFunction(test_function);
    codegen_module->setTargetInfo(reg_info.get(), inst_info.get());
    
    // Release the MachineModule
    auto machine_module = codegen_module->releaseMachineModule();
    
    ASSERT_NE(machine_module, nullptr);
    EXPECT_EQ(machine_module->target_reg_info(), reg_info.get());
    EXPECT_EQ(machine_module->target_inst_info(), inst_info.get());
    
    // CodeGenModule should no longer have a MachineModule
    EXPECT_EQ(codegen_module->getMachineModule(), nullptr);
}

// Test MachineModuleBuilder basic creation
TEST_F(MachineModuleTest, BuilderCreate) {
    auto codegen_module = MachineModuleBuilder::create(ir_module.get());
    
    ASSERT_NE(codegen_module, nullptr);
    EXPECT_EQ(codegen_module->getIRModule(), ir_module.get());
    EXPECT_NE(codegen_module->getMachineModule(), nullptr);
    EXPECT_TRUE(codegen_module->empty());
}

// Test MachineModuleBuilder creation with target
TEST_F(MachineModuleTest, BuilderCreateWithTarget) {
    auto codegen_module = MachineModuleBuilder::createWithTarget(
        ir_module.get(),
        reg_info.get(),
        inst_info.get()
    );
    
    ASSERT_NE(codegen_module, nullptr);
    EXPECT_EQ(codegen_module->getIRModule(), ir_module.get());
    EXPECT_NE(codegen_module->getMachineModule(), nullptr);
    
    // Verify target info is set
    EXPECT_EQ(codegen_module->getTargetRegisterInfo(), reg_info.get());
    EXPECT_EQ(codegen_module->getTargetInstInfo(), inst_info.get());
}

// Test multiple function addition
TEST_F(MachineModuleTest, MultipleFunctions) {
    auto codegen_module = std::make_unique<CodeGenModule>(ir_module.get());
    
    // Create additional functions
    auto int64_type = ir_module->get_integer_type(64);
    auto func_type2 = ir_module->get_function_type(int64_type, {});
    auto* test_function2 = ir_module->create_function("test_func2", func_type2);
    
    auto void_type = ir_module->get_void_type();
    auto func_type3 = ir_module->get_function_type(void_type, {});
    auto* test_function3 = ir_module->create_function("test_func3", func_type3);
    
    // Add functions
    auto* machine_func1 = codegen_module->addFunction(test_function);
    auto* machine_func2 = codegen_module->addFunction(test_function2);
    auto* machine_func3 = codegen_module->addFunction(test_function3);
    
    EXPECT_NE(machine_func1, nullptr);
    EXPECT_NE(machine_func2, nullptr);
    EXPECT_NE(machine_func3, nullptr);
    
    EXPECT_FALSE(codegen_module->empty());
    EXPECT_EQ(codegen_module->getFunctionCount(), 3);
}

// Test error handling for null IR module
TEST_F(MachineModuleTest, NullIRModule) {
    // This should trigger an assertion in debug builds
    // In release builds, behavior is undefined but we test it doesn't crash
#ifndef NDEBUG
    EXPECT_DEATH(CodeGenModule(nullptr), ".*");
#endif
}

// Test error handling for null function
TEST_F(MachineModuleTest, NullFunction) {
    auto codegen_module = std::make_unique<CodeGenModule>(ir_module.get());
    
    // Adding null function should trigger assertion in debug builds
#ifndef NDEBUG
    EXPECT_DEATH(codegen_module->addFunction(nullptr), ".*");
#endif
}