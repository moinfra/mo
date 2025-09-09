#include <gtest/gtest.h>
#include <memory>
#include <string>

#include "src/codegen/assembly_emitter.h"
#include "src/targets/riscv_assembly_emitter.h"
#include "src/ir.h"
#include "src/machine.h"

using namespace codegen;

// Mock assembly emitter for testing base interface
class MockAssemblyEmitter : public AssemblyEmitter {
public:
    std::string emit(const MachineModule& module) override {
        emit_called_ = true;
        emit_result_ = ".text\n.globl main\nmain:\n    nop\n    ret\n";
        return emit_result_;
    }
    
    const char* getTargetName() const override {
        return "mock-target";
    }
    
    bool supportsFeature(const std::string& feature) const override {
        return feature == "mock-feature";
    }
    
    size_t estimateOutputSize(const MachineModule& module) const override {
        return AssemblyEmitter::estimateOutputSize(module) + 50;
    }
    
    // Test helpers
    bool wasEmitCalled() const { return emit_called_; }
    const std::string& getEmitResult() const { return emit_result_; }
    
private:
    bool emit_called_ = false;
    std::string emit_result_;
};

class AssemblyEmitterTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Create a minimal IR module and function for testing
        ir_module = std::make_unique<Module>("test_module");
        
        auto int_type = ir_module->get_integer_type(32);
        auto func_type = ir_module->get_function_type(int_type, {});
        test_function = ir_module->create_function("test_func", func_type);
        
        // Create a basic block to make the function non-empty
        auto* entry_bb = test_function->create_basic_block("entry");
        (void)entry_bb;  // Suppress unused variable warning
        
        // Create RISC-V target components
        auto abi_version = RISCV::ABIVersion::LP64D;
        reg_info = std::make_unique<RISCV::RISCVRegisterInfo>(abi_version);
        inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>(abi_version);
        
        // Create machine module
        machine_module = std::make_unique<MachineModule>(ir_module.get());
        machine_module->set_target_info(reg_info.get(), inst_info.get());
    }
    
    std::unique_ptr<Module> ir_module;
    Function* test_function = nullptr;
    std::unique_ptr<RISCV::RISCVRegisterInfo> reg_info;
    std::unique_ptr<RISCV::RISCVTargetInstInfo> inst_info;
    std::unique_ptr<MachineModule> machine_module;
};

// Test AssemblyEmitter base interface
TEST_F(AssemblyEmitterTest, BaseInterface) {
    auto mock_emitter = std::make_unique<MockAssemblyEmitter>();
    
    // Test basic interface methods
    EXPECT_STREQ("mock-target", mock_emitter->getTargetName());
    EXPECT_TRUE(mock_emitter->supportsFeature("mock-feature"));
    EXPECT_FALSE(mock_emitter->supportsFeature("non-existent-feature"));
    
    // Test size estimation
    size_t size = mock_emitter->estimateOutputSize(*machine_module);
    EXPECT_GE(size, 50);  // Should be at least base size + 50
}

// Test assembly emission interface
TEST_F(AssemblyEmitterTest, EmissionInterface) {
    auto mock_emitter = std::make_unique<MockAssemblyEmitter>();
    
    // Test emission call
    EXPECT_FALSE(mock_emitter->wasEmitCalled());
    std::string assembly = mock_emitter->emit(*machine_module);
    EXPECT_TRUE(mock_emitter->wasEmitCalled());
    EXPECT_FALSE(assembly.empty());
    EXPECT_EQ(assembly, mock_emitter->getEmitResult());
}

// Test RISCVAssemblyEmitter creation
TEST_F(AssemblyEmitterTest, RISCVEmitterCreation) {
    auto riscv_emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    
    ASSERT_NE(riscv_emitter, nullptr);
    EXPECT_STREQ("riscv64", riscv_emitter->getTargetName());
    
    // Test RISC-V specific features
    EXPECT_TRUE(riscv_emitter->supportsFeature("riscv"));
    EXPECT_TRUE(riscv_emitter->supportsFeature("assembly-emission"));
    EXPECT_TRUE(riscv_emitter->supportsFeature("64bit"));
    EXPECT_FALSE(riscv_emitter->supportsFeature("32bit"));
    EXPECT_TRUE(riscv_emitter->supportsFeature("d"));
    
    // Test component access
    EXPECT_EQ(riscv_emitter->getRegisterInfo(), reg_info.get());
    EXPECT_EQ(riscv_emitter->getInstInfo(), inst_info.get());
    EXPECT_EQ(riscv_emitter->getABIVersion(), RISCV::ABIVersion::LP64D);
}

// Test empty module assembly emission
TEST_F(AssemblyEmitterTest, EmptyModuleEmission) {
    auto riscv_emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    
    // Emit assembly for empty machine module (no functions created yet)
    std::string assembly = riscv_emitter->emit(*machine_module);
    EXPECT_FALSE(assembly.empty());
    
    // Check basic format correctness
    EXPECT_NE(assembly.find(".text"), std::string::npos);  // Should contain .text directive
    EXPECT_NE(assembly.find(".arch"), std::string::npos);  // Should contain architecture directive
}

// Test basic format correctness with function
TEST_F(AssemblyEmitterTest, BasicFormatCorrectness) {
    auto riscv_emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    
    // Create a machine function
    auto* machine_func = machine_module->create_machine_function(test_function);
    ASSERT_NE(machine_func, nullptr);
    
    // Create a basic block in the machine function
    auto* entry_bb = machine_func->create_block("entry");
    ASSERT_NE(entry_bb, nullptr);
    
    // Emit assembly
    std::string assembly = riscv_emitter->emit(*machine_module);
    EXPECT_FALSE(assembly.empty());
    
    // Check RISC-V specific format elements
    EXPECT_NE(assembly.find(".text"), std::string::npos);          // Text section
    EXPECT_NE(assembly.find(".arch rv64gc"), std::string::npos);   // Architecture
    EXPECT_NE(assembly.find(".globl"), std::string::npos);         // Global symbol
    EXPECT_NE(assembly.find("function_"), std::string::npos);      // Function label
    // Note: nop instructions only appear when there are actual machine instructions
}

// Helper function for registry test
static std::unique_ptr<codegen::AssemblyEmitter> createMockEmitter(
    const TargetRegisterInfo* reg_info, const TargetInstInfo* inst_info) {
    (void)reg_info; (void)inst_info;  // Suppress unused warnings
    return std::make_unique<MockAssemblyEmitter>();
}

// Test AssemblyEmitterRegistry
TEST_F(AssemblyEmitterTest, Registry) {
    // Test initial empty state
    auto initial_targets = AssemblyEmitterRegistry::getAvailableTargets();
    size_t initial_count = initial_targets.size();
    
    // Register a test emitter
    AssemblyEmitterRegistry::registerEmitter("test-target", createMockEmitter);
    
    // Test available targets
    auto targets = AssemblyEmitterRegistry::getAvailableTargets();
    EXPECT_EQ(targets.size(), initial_count + 1);
    
    bool found_test_target = false;
    for (const auto& target : targets) {
        if (target == "test-target") {
            found_test_target = true;
            break;
        }
    }
    EXPECT_TRUE(found_test_target);
    
    // Test emitter creation
    auto created_emitter = AssemblyEmitterRegistry::createEmitter(
        "test-target", reg_info.get(), inst_info.get());
    ASSERT_NE(created_emitter, nullptr);
    EXPECT_STREQ("mock-target", created_emitter->getTargetName());
    
    // Test non-existent emitter
    auto null_emitter = AssemblyEmitterRegistry::createEmitter(
        "non-existent", reg_info.get(), inst_info.get());
    EXPECT_EQ(null_emitter, nullptr);
}

// Test size estimation
TEST_F(AssemblyEmitterTest, SizeEstimation) {
    auto riscv_emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    
    // Test empty module
    size_t empty_size = riscv_emitter->estimateOutputSize(*machine_module);
    EXPECT_GE(empty_size, 200);  // Should include directive overhead
    
    // Add a function and test increased size
    auto* machine_func = machine_module->create_machine_function(test_function);
    (void)machine_func;  // Suppress unused variable warning
    
    size_t with_function_size = riscv_emitter->estimateOutputSize(*machine_module);
    EXPECT_GT(with_function_size, empty_size);  // Should be larger
}

// Test different ABI versions
TEST_F(AssemblyEmitterTest, ABIVersions) {
    struct TestCase {
        RISCV::ABIVersion abi;
        const char* expected_name;
        bool has_32bit;
        bool has_64bit;
        bool has_f;
        bool has_d;
    };
    
    std::vector<TestCase> test_cases = {
        {RISCV::ABIVersion::ILP32,  "riscv32", true,  false, false, false},
        {RISCV::ABIVersion::ILP32F, "riscv32", true,  false, true,  false},
        {RISCV::ABIVersion::LP64,   "riscv64", false, true,  false, false},
        {RISCV::ABIVersion::LP64F,  "riscv64", false, true,  true,  false},
        {RISCV::ABIVersion::LP64D,  "riscv64", false, true,  true,  true},
    };
    
    for (const auto& test_case : test_cases) {
        auto reg_info_local = std::make_unique<RISCV::RISCVRegisterInfo>(test_case.abi);
        auto inst_info_local = std::make_unique<RISCV::RISCVTargetInstInfo>(test_case.abi);
        auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
            reg_info_local.get(), inst_info_local.get());
        
        EXPECT_STREQ(test_case.expected_name, emitter->getTargetName());
        EXPECT_EQ(test_case.has_32bit, emitter->supportsFeature("32bit"));
        EXPECT_EQ(test_case.has_64bit, emitter->supportsFeature("64bit"));
        EXPECT_EQ(test_case.has_f, emitter->supportsFeature("f"));
        EXPECT_EQ(test_case.has_d, emitter->supportsFeature("d"));
    }
}