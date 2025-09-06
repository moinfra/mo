#include <gtest/gtest.h>
#include <memory>
#include <string>

#include "src/targets/target_base.h"
#include "src/ir.h"
#include "src/machine.h"

using namespace codegen;

// Mock target implementation for testing
class MockTarget : public TargetBase {
public:
    std::unique_ptr<MachineModule> generateMachineCode(const Module& ir_module) override {
        // Return a minimal MachineModule for testing
        auto machine_module = std::make_unique<MachineModule>(const_cast<Module*>(&ir_module));
        return machine_module;
    }
    
    std::string emitAssembly(const MachineModule& machine_module) override {
        return "# Mock assembly output\n.text\n.globl main\nmain:\n    ret\n";
    }
    
    const char* getTargetName() const override {
        return "mock-target";
    }
    
    bool hasFeature(const std::string& feature) const override {
        return feature == "test-feature";
    }
};

class TargetBaseTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Create a minimal IR module for testing
        ir_module = std::make_unique<Module>("test_module");
    }
    
    std::unique_ptr<Module> ir_module;
};

// Test TargetBase interface inheritance
TEST_F(TargetBaseTest, InterfaceInheritance) {
    auto mock_target = std::make_unique<MockTarget>();
    
    // Test that we can use the target through base interface
    TargetBase* base_ptr = mock_target.get();
    ASSERT_NE(base_ptr, nullptr);
    
    // Test virtual function calls
    EXPECT_STREQ("mock-target", base_ptr->getTargetName());
    EXPECT_TRUE(base_ptr->hasFeature("test-feature"));
    EXPECT_FALSE(base_ptr->hasFeature("non-existent-feature"));
}

// Test machine code generation
TEST_F(TargetBaseTest, MachineCodeGeneration) {
    auto mock_target = std::make_unique<MockTarget>();
    
    // Test machine code generation
    auto machine_module = mock_target->generateMachineCode(*ir_module);
    ASSERT_NE(machine_module, nullptr);
    
    // Test assembly emission
    std::string assembly = mock_target->emitAssembly(*machine_module);
    EXPECT_FALSE(assembly.empty());
    EXPECT_NE(assembly.find("Mock assembly"), std::string::npos);
}

// Test TargetRegistry functionality
TEST_F(TargetBaseTest, TargetRegistry) {
    // Test initial empty state
    auto initial_targets = TargetRegistry::getAvailableTargets();
    size_t initial_count = initial_targets.size();
    
    // Test target registration
    TargetRegistry::registerTarget("test-target", []() -> std::unique_ptr<TargetBase> {
        return std::make_unique<MockTarget>();
    });
    
    // Test available targets
    auto targets = TargetRegistry::getAvailableTargets();
    EXPECT_EQ(targets.size(), initial_count + 1);
    
    bool found_test_target = false;
    for (const auto& target : targets) {
        if (target == "test-target") {
            found_test_target = true;
            break;
        }
    }
    EXPECT_TRUE(found_test_target);
    
    // Test target creation
    auto created_target = TargetRegistry::createTarget("test-target");
    ASSERT_NE(created_target, nullptr);
    EXPECT_STREQ("mock-target", created_target->getTargetName());
    
    // Test non-existent target
    auto null_target = TargetRegistry::createTarget("non-existent");
    EXPECT_EQ(null_target, nullptr);
}

// Test registration macro (simplified version)
namespace {
    std::unique_ptr<TargetBase> createMacroTarget() {
        return std::make_unique<MockTarget>();
    }
    
    struct MacroTargetRegistrar {
        MacroTargetRegistrar() {
            TargetRegistry::registerTarget("macro-target", createMacroTarget);
        }
    };
    
    // This will register during static initialization
    static MacroTargetRegistrar g_macro_target_registrar;
}

TEST_F(TargetBaseTest, RegistrationMacro) {
    // Test that macro-registered target is available
    auto targets = TargetRegistry::getAvailableTargets();
    
    bool found_macro_target = false;
    for (const auto& target : targets) {
        if (target == "macro-target") {
            found_macro_target = true;
            break;
        }
    }
    EXPECT_TRUE(found_macro_target);
    
    // Test creation of macro-registered target
    auto target = TargetRegistry::createTarget("macro-target");
    ASSERT_NE(target, nullptr);
    EXPECT_STREQ("mock-target", target->getTargetName());
}

// Test error handling
TEST_F(TargetBaseTest, ErrorHandling) {
    // Test null function registration (should be ignored)
    size_t before_count = TargetRegistry::getAvailableTargets().size();
    TargetRegistry::registerTarget("null-target", nullptr);
    size_t after_count = TargetRegistry::getAvailableTargets().size();
    
    EXPECT_EQ(before_count, after_count);  // Should not add null targets
}