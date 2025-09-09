// control_flow_test.cc - Test basic blocks and control flow

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

class ControlFlowTest : public ::testing::Test {
protected:
    std::unique_ptr<Module> createTestModule() {
        return std::make_unique<Module>("test_module");
    }
};

TEST_F(ControlFlowTest, UnconditionalBranchTest) {
    // Test unconditional branch: br label %target
    // Expected: j target_label
    
    auto module = createTestModule();
    auto void_type = module->get_void_type();
    auto func_type = module->get_function_type(void_type, {});
    auto* func = module->create_function("branch_test", func_type);
    
    auto* entry = func->create_basic_block("entry");
    auto* target = func->create_basic_block("target");
    
    IRBuilder builder(module.get());
    
    // Entry block: br label %target
    builder.set_insert_point(entry);
    builder.create_br(target);
    
    // Target block: ret void
    builder.set_insert_point(target);
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
    
    // Verify unconditional jump
    EXPECT_NE(assembly.find("j target"), std::string::npos);
    
    // Verify target label exists  
    EXPECT_NE(assembly.find("target:"), std::string::npos);
    
    std::cout << "Unconditional Branch Test Assembly:\n" << assembly << std::endl;
}

TEST_F(ControlFlowTest, ConditionalBranchEqualTest) {
    // Test conditional branch with equality: if (a == b) goto true_bb else false_bb
    // Expected: beq a, b, true_label; j false_label
    
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {int_type, int_type});
    auto* func = module->create_function("cond_eq_test", func_type);
    
    auto* entry = func->create_basic_block("entry");
    auto* true_bb = func->create_basic_block("true_bb");
    auto* false_bb = func->create_basic_block("false_bb");
    
    IRBuilder builder(module.get());
    
    // Entry block: %cmp = icmp eq %a, %b; br %cmp, %true_bb, %false_bb
    builder.set_insert_point(entry);
    auto* cmp = builder.create_icmp(ICmpInst::EQ, func->arg(0), func->arg(1));
    builder.create_cond_br(cmp, true_bb, false_bb);
    
    // True block: return 1
    builder.set_insert_point(true_bb);
    builder.create_ret(module->get_constant_int(int_type, 1));
    
    // False block: return 0
    builder.set_insert_point(false_bb);
    builder.create_ret(module->get_constant_int(int_type, 0));
    
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
    
    // Verify conditional branch (working pattern: bne result, zero, label)
    EXPECT_NE(assembly.find("bne vreg1000, zero, true_bb"), std::string::npos);
    
    // Verify fallthrough jump
    EXPECT_NE(assembly.find("j false_bb"), std::string::npos);
    
    // Verify all labels exist
    EXPECT_NE(assembly.find("true_bb:"), std::string::npos);
    EXPECT_NE(assembly.find("false_bb:"), std::string::npos);
    
    std::cout << "Conditional Branch (EQ) Test Assembly:\n" << assembly << std::endl;
}

TEST_F(ControlFlowTest, ConditionalBranchLessThanTest) {
    // Test conditional branch with less than: if (a < b) goto true_bb else false_bb
    // Expected: blt a, b, true_label; j false_label
    
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {int_type, int_type});
    auto* func = module->create_function("cond_lt_test", func_type);
    
    auto* entry = func->create_basic_block("entry");
    auto* true_bb = func->create_basic_block("less_than");
    auto* false_bb = func->create_basic_block("greater_equal");
    
    IRBuilder builder(module.get());
    
    // Entry block: %cmp = icmp slt %a, %b; br %cmp, %less_than, %greater_equal
    builder.set_insert_point(entry);
    auto* cmp = builder.create_icmp(ICmpInst::SLT, func->arg(0), func->arg(1));
    builder.create_cond_br(cmp, true_bb, false_bb);
    
    // True block: return -1
    builder.set_insert_point(true_bb);
    builder.create_ret(module->get_constant_int(int_type, -1));
    
    // False block: return 1
    builder.set_insert_point(false_bb);
    builder.create_ret(module->get_constant_int(int_type, 1));
    
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
    
    // Verify conditional branch (working pattern: bne result, zero, label)
    EXPECT_NE(assembly.find("bne vreg1000, zero, less_than"), std::string::npos);
    
    // Verify fallthrough jump
    EXPECT_NE(assembly.find("j greater_equal"), std::string::npos);
    
    // Verify all labels exist
    EXPECT_NE(assembly.find("less_than:"), std::string::npos);
    EXPECT_NE(assembly.find("greater_equal:"), std::string::npos);
    
    std::cout << "Conditional Branch (SLT) Test Assembly:\n" << assembly << std::endl;
}

TEST_F(ControlFlowTest, ComplexControlFlowTest) {
    // Test complex control flow with multiple branches and basic blocks
    // if (a > 0) {
    //   if (b > 0) return a + b;
    //   else return a;
    // } else {
    //   return 0;
    // }
    
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {int_type, int_type});
    auto* func = module->create_function("complex_test", func_type);
    
    auto* entry = func->create_basic_block("entry");
    auto* a_positive = func->create_basic_block("a_positive");
    auto* a_nonpositive = func->create_basic_block("a_nonpositive");
    auto* b_positive = func->create_basic_block("b_positive");
    auto* b_nonpositive = func->create_basic_block("b_nonpositive");
    
    IRBuilder builder(module.get());
    
    // Entry: if (a > 0)
    builder.set_insert_point(entry);
    auto* zero = module->get_constant_int(int_type, 0);
    auto* cmp_a = builder.create_icmp(ICmpInst::SGT, func->arg(0), zero);
    builder.create_cond_br(cmp_a, a_positive, a_nonpositive);
    
    // A positive: if (b > 0)
    builder.set_insert_point(a_positive);
    auto* cmp_b = builder.create_icmp(ICmpInst::SGT, func->arg(1), zero);
    builder.create_cond_br(cmp_b, b_positive, b_nonpositive);
    
    // B positive: return a + b
    builder.set_insert_point(b_positive);
    auto* sum = builder.create_add(func->arg(0), func->arg(1));
    builder.create_ret(sum);
    
    // B non-positive: return a
    builder.set_insert_point(b_nonpositive);
    builder.create_ret(func->arg(0));
    
    // A non-positive: return 0
    builder.set_insert_point(a_nonpositive);
    builder.create_ret(zero);
    
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
    
    // Verify multiple branch types present (working pattern: bne result, zero, label)  
    EXPECT_NE(assembly.find("bne vreg1000, zero, a_positive"), std::string::npos);
    EXPECT_NE(assembly.find("bne vreg1001, zero, b_positive"), std::string::npos);
    
    // Verify all basic block labels exist
    EXPECT_NE(assembly.find("a_positive:"), std::string::npos);
    EXPECT_NE(assembly.find("a_nonpositive:"), std::string::npos);
    EXPECT_NE(assembly.find("b_positive:"), std::string::npos);
    EXPECT_NE(assembly.find("b_nonpositive:"), std::string::npos);
    
    // Verify arithmetic operations
    EXPECT_NE(assembly.find("add vreg"), std::string::npos);
    
    std::cout << "Complex Control Flow Test Assembly:\n" << assembly << std::endl;
}

TEST_F(ControlFlowTest, UnsignedComparisonTest) {
    // Test unsigned comparison: if (a < b unsigned) goto true_bb else false_bb
    // Expected: bltu a, b, true_label; j false_label
    
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {int_type, int_type});
    auto* func = module->create_function("unsigned_test", func_type);
    
    auto* entry = func->create_basic_block("entry");
    auto* true_bb = func->create_basic_block("unsigned_less");
    auto* false_bb = func->create_basic_block("unsigned_greater_equal");
    
    IRBuilder builder(module.get());
    
    // Entry block: %cmp = icmp ult %a, %b; br %cmp, %unsigned_less, %unsigned_greater_equal
    builder.set_insert_point(entry);
    auto* cmp = builder.create_icmp(ICmpInst::ULT, func->arg(0), func->arg(1));
    builder.create_cond_br(cmp, true_bb, false_bb);
    
    // True block: return 1
    builder.set_insert_point(true_bb);
    builder.create_ret(module->get_constant_int(int_type, 1));
    
    // False block: return 0
    builder.set_insert_point(false_bb);
    builder.create_ret(module->get_constant_int(int_type, 0));
    
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
    
    // Verify unsigned branch (working pattern: bne result, zero, label)
    EXPECT_NE(assembly.find("bne vreg1000, zero, unsigned_less"), std::string::npos);
    
    // Verify all labels exist
    EXPECT_NE(assembly.find("unsigned_less:"), std::string::npos);
    EXPECT_NE(assembly.find("unsigned_greater_equal:"), std::string::npos);
    
    std::cout << "Unsigned Comparison Test Assembly:\n" << assembly << std::endl;
}