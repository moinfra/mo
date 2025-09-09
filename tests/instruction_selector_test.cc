#include <gtest/gtest.h>
#include <memory>
#include <string>

#include "src/codegen/instruction_selector.h"
#include "src/targets/riscv_instruction_selector.h"
#include "src/ir.h"
#include "src/machine.h"

using namespace codegen;

// Mock instruction selector for testing base interface
class MockInstructionSelector : public InstructionSelector {
public:
    bool selectInstructions(const Function& ir_func, 
                          MachineFunction& machine_func) override {
        selection_called_ = true;
        return selection_result_;
    }
    
    const char* getTargetName() const override {
        return "mock-target";
    }
    
    bool supportsFeature(const std::string& feature) const override {
        return feature == "mock-feature";
    }
    
    size_t estimateSelectionCost(const Function& ir_func) const override {
        return InstructionSelector::estimateSelectionCost(ir_func) + 100;
    }
    
    // Test helpers
    bool wasSelectionCalled() const { return selection_called_; }
    void setSelectionResult(bool result) { selection_result_ = result; }
    
private:
    bool selection_called_ = false;
    bool selection_result_ = true;
};

class InstructionSelectorTest : public ::testing::Test {
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
    }
    
    std::unique_ptr<Module> ir_module;
    Function* test_function = nullptr;
    std::unique_ptr<RISCV::RISCVRegisterInfo> reg_info;
    std::unique_ptr<RISCV::RISCVTargetInstInfo> inst_info;
};

// Test InstructionSelector base interface
TEST_F(InstructionSelectorTest, BaseInterface) {
    auto mock_selector = std::make_unique<MockInstructionSelector>();
    
    // Test basic interface methods
    EXPECT_STREQ("mock-target", mock_selector->getTargetName());
    EXPECT_TRUE(mock_selector->supportsFeature("mock-feature"));
    EXPECT_FALSE(mock_selector->supportsFeature("non-existent-feature"));
    
    // Test cost estimation
    size_t cost = mock_selector->estimateSelectionCost(*test_function);
    EXPECT_GT(cost, 100);  // Should be base cost + 100
}

// Test instruction selection interface
TEST_F(InstructionSelectorTest, InstructionSelection) {
    auto mock_selector = std::make_unique<MockInstructionSelector>();
    
    // Create a machine module and function for testing
    auto machine_module = std::make_unique<MachineModule>(ir_module.get());
    auto* machine_func = machine_module->create_machine_function(test_function);
    ASSERT_NE(machine_func, nullptr);
    
    // Test successful selection
    EXPECT_FALSE(mock_selector->wasSelectionCalled());
    bool result = mock_selector->selectInstructions(*test_function, *machine_func);
    EXPECT_TRUE(result);
    EXPECT_TRUE(mock_selector->wasSelectionCalled());
    
    // Test failed selection
    mock_selector->setSelectionResult(false);
    result = mock_selector->selectInstructions(*test_function, *machine_func);
    EXPECT_FALSE(result);
}

// Test RISCVInstructionSelector creation
TEST_F(InstructionSelectorTest, RISCVSelectorCreation) {
    auto riscv_selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    ASSERT_NE(riscv_selector, nullptr);
    EXPECT_STREQ("riscv64", riscv_selector->getTargetName());
    
    // Test RISC-V specific features
    EXPECT_TRUE(riscv_selector->supportsFeature("riscv"));
    EXPECT_TRUE(riscv_selector->supportsFeature("i"));
    EXPECT_TRUE(riscv_selector->supportsFeature("m"));
    EXPECT_TRUE(riscv_selector->supportsFeature("64bit"));
    EXPECT_FALSE(riscv_selector->supportsFeature("32bit"));
    EXPECT_TRUE(riscv_selector->supportsFeature("d"));
    
    // Test component access
    EXPECT_EQ(riscv_selector->getRegisterInfo(), reg_info.get());
    EXPECT_EQ(riscv_selector->getInstInfo(), inst_info.get());
    EXPECT_EQ(riscv_selector->getABIVersion(), RISCV::ABIVersion::LP64D);
}

// Test RISCVInstructionSelector instruction selection
TEST_F(InstructionSelectorTest, RISCVInstructionSelection) {
    auto riscv_selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    // Create machine function
    auto machine_module = std::make_unique<MachineModule>(ir_module.get());
    auto* machine_func = machine_module->create_machine_function(test_function);
    ASSERT_NE(machine_func, nullptr);
    
    // Test instruction selection (should succeed with placeholder implementation)
    bool result = riscv_selector->selectInstructions(*test_function, *machine_func);
    EXPECT_TRUE(result);
    
    // Test that at least one basic block was created
    EXPECT_GT(machine_func->basic_blocks().size(), 0);
}

// Helper function for registry test
static std::unique_ptr<codegen::InstructionSelector> createMockSelector(
    const TargetRegisterInfo* reg_info, const TargetInstInfo* inst_info) {
    (void)reg_info; (void)inst_info;  // Suppress unused warnings
    return std::make_unique<MockInstructionSelector>();
}

// Test InstructionSelectorRegistry
TEST_F(InstructionSelectorTest, Registry) {
    // Test initial empty state
    auto initial_targets = InstructionSelectorRegistry::getAvailableTargets();
    size_t initial_count = initial_targets.size();
    
    // Register a test selector
    InstructionSelectorRegistry::registerSelector("test-target", createMockSelector);
    
    // Test available targets
    auto targets = InstructionSelectorRegistry::getAvailableTargets();
    EXPECT_EQ(targets.size(), initial_count + 1);
    
    bool found_test_target = false;
    for (const auto& target : targets) {
        if (target == "test-target") {
            found_test_target = true;
            break;
        }
    }
    EXPECT_TRUE(found_test_target);
    
    // Test selector creation
    auto created_selector = InstructionSelectorRegistry::createSelector(
        "test-target", reg_info.get(), inst_info.get());
    ASSERT_NE(created_selector, nullptr);
    EXPECT_STREQ("mock-target", created_selector->getTargetName());
    
    // Test non-existent selector
    auto null_selector = InstructionSelectorRegistry::createSelector(
        "non-existent", reg_info.get(), inst_info.get());
    EXPECT_EQ(null_selector, nullptr);
}

// Test cost estimation
TEST_F(InstructionSelectorTest, CostEstimation) {
    auto riscv_selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    size_t cost = riscv_selector->estimateSelectionCost(*test_function);
    EXPECT_GT(cost, 0);
    
    // RISC-V should have lower cost than base implementation
    auto mock_selector = std::make_unique<MockInstructionSelector>();
    size_t mock_cost = mock_selector->InstructionSelector::estimateSelectionCost(*test_function);
    EXPECT_LT(cost, mock_cost);
}

// Test different ABI versions
TEST_F(InstructionSelectorTest, ABIVersions) {
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
        auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
            reg_info_local.get(), inst_info_local.get());
        
        EXPECT_STREQ(test_case.expected_name, selector->getTargetName());
        EXPECT_EQ(test_case.has_32bit, selector->supportsFeature("32bit"));
        EXPECT_EQ(test_case.has_64bit, selector->supportsFeature("64bit"));
        EXPECT_EQ(test_case.has_f, selector->supportsFeature("f"));
        EXPECT_EQ(test_case.has_d, selector->supportsFeature("d"));
    }
}