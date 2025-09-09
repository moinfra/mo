#include <gtest/gtest.h>
#include <memory>
#include <string>

#include "src/targets/riscv_target.h"
#include "src/ir.h"
#include "src/machine.h"

using namespace RISCV;

class RISCVTargetTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Create minimal IR module for testing
        ir_module = std::make_unique<Module>("test_module");
        
        // Create RISC-V target components
        auto abi_version = ABIVersion::LP64D;
        auto reg_info = std::make_unique<RISCVRegisterInfo>(abi_version);
        auto inst_info = std::make_unique<RISCVTargetInstInfo>(abi_version);
        
        // Create RISC-V target
        riscv_target = std::make_unique<RISCVTarget>(
            std::move(reg_info), 
            std::move(inst_info), 
            abi_version
        );
    }
    
    std::unique_ptr<Module> ir_module;
    std::unique_ptr<RISCVTarget> riscv_target;
};

// Test RISCVTarget construction
TEST_F(RISCVTargetTest, Construction) {
    ASSERT_NE(riscv_target, nullptr);
    EXPECT_NE(riscv_target->getRegisterInfo(), nullptr);
    EXPECT_NE(riscv_target->getInstInfo(), nullptr);
    EXPECT_EQ(riscv_target->getABIVersion(), ABIVersion::LP64D);
}

// Test target name
TEST_F(RISCVTargetTest, TargetName) {
    EXPECT_STREQ("riscv64", riscv_target->getTargetName());
    
    // Test 32-bit variants
    auto reg_info_32 = std::make_unique<RISCVRegisterInfo>(ABIVersion::ILP32);
    auto inst_info_32 = std::make_unique<RISCVTargetInstInfo>(ABIVersion::ILP32);
    auto target_32 = std::make_unique<RISCVTarget>(
        std::move(reg_info_32), 
        std::move(inst_info_32), 
        ABIVersion::ILP32
    );
    EXPECT_STREQ("riscv32", target_32->getTargetName());
}

// Test feature support
TEST_F(RISCVTargetTest, FeatureSupport) {
    // Basic RISC-V features
    EXPECT_TRUE(riscv_target->hasFeature("riscv"));
    EXPECT_TRUE(riscv_target->hasFeature("i"));
    EXPECT_TRUE(riscv_target->hasFeature("m"));
    
    // 64-bit features (LP64D)
    EXPECT_TRUE(riscv_target->hasFeature("64bit"));
    EXPECT_FALSE(riscv_target->hasFeature("32bit"));
    
    // Floating-point features (LP64D has both F and D)
    EXPECT_TRUE(riscv_target->hasFeature("f"));
    EXPECT_TRUE(riscv_target->hasFeature("d"));
    
    // Non-existent features
    EXPECT_FALSE(riscv_target->hasFeature("unknown"));
    EXPECT_FALSE(riscv_target->hasFeature("x86"));
}

// Test machine code generation (basic interface)
TEST_F(RISCVTargetTest, MachineCodeGeneration) {
    // Test basic interface functionality
    auto machine_module = riscv_target->generateMachineCode(*ir_module);
    
    ASSERT_NE(machine_module, nullptr);
    EXPECT_NE(machine_module->target_reg_info(), nullptr);
    EXPECT_NE(machine_module->target_inst_info(), nullptr);
}

// Test assembly emission (basic interface)  
TEST_F(RISCVTargetTest, AssemblyEmission) {
    auto machine_module = riscv_target->generateMachineCode(*ir_module);
    ASSERT_NE(machine_module, nullptr);
    
    std::string assembly = riscv_target->emitAssembly(*machine_module);
    
    // Check for basic RISC-V assembly patterns
    EXPECT_FALSE(assembly.empty());
    EXPECT_NE(assembly.find(".text"), std::string::npos);
    EXPECT_NE(assembly.find("main"), std::string::npos);
    EXPECT_NE(assembly.find("ret"), std::string::npos);
}

// Test inheritance from TargetBase
TEST_F(RISCVTargetTest, TargetBaseInheritance) {
    // Test that RISCVTarget can be used through TargetBase interface
    codegen::TargetBase* base_ptr = riscv_target.get();
    ASSERT_NE(base_ptr, nullptr);
    
    // Test virtual function calls
    EXPECT_STREQ("riscv64", base_ptr->getTargetName());
    EXPECT_TRUE(base_ptr->hasFeature("riscv"));
    
    // Test machine code generation through base interface
    auto machine_module = base_ptr->generateMachineCode(*ir_module);
    ASSERT_NE(machine_module, nullptr);
    
    // Test assembly emission through base interface
    std::string assembly = base_ptr->emitAssembly(*machine_module);
    EXPECT_FALSE(assembly.empty());
}

// Test different ABI versions
TEST_F(RISCVTargetTest, ABIVersions) {
    struct TestCase {
        ABIVersion abi;
        const char* expected_name;
        bool has_32bit;
        bool has_64bit;
        bool has_f;
        bool has_d;
    };
    
    std::vector<TestCase> test_cases = {
        {ABIVersion::ILP32,  "riscv32", true,  false, false, false},
        {ABIVersion::ILP32F, "riscv32", true,  false, true,  false},
        {ABIVersion::LP64,   "riscv64", false, true,  false, false},
        {ABIVersion::LP64F,  "riscv64", false, true,  true,  false},
        {ABIVersion::LP64D,  "riscv64", false, true,  true,  true},
    };
    
    for (const auto& test_case : test_cases) {
        auto reg_info = std::make_unique<RISCVRegisterInfo>(test_case.abi);
        auto inst_info = std::make_unique<RISCVTargetInstInfo>(test_case.abi);
        auto target = std::make_unique<RISCVTarget>(
            std::move(reg_info), 
            std::move(inst_info), 
            test_case.abi
        );
        
        EXPECT_STREQ(test_case.expected_name, target->getTargetName());
        EXPECT_EQ(test_case.has_32bit, target->hasFeature("32bit"));
        EXPECT_EQ(test_case.has_64bit, target->hasFeature("64bit"));
        EXPECT_EQ(test_case.has_f, target->hasFeature("f"));
        EXPECT_EQ(test_case.has_d, target->hasFeature("d"));
    }
}

// Test component access
TEST_F(RISCVTargetTest, ComponentAccess) {
    const auto* reg_info = riscv_target->getRegisterInfo();
    const auto* inst_info = riscv_target->getInstInfo();
    
    ASSERT_NE(reg_info, nullptr);
    ASSERT_NE(inst_info, nullptr);
    
    // Test that components are properly configured
    EXPECT_EQ(ABIVersion::LP64D, riscv_target->getABIVersion());
}