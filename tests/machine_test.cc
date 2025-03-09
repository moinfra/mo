#include "src/machine.h"

#include <gtest/gtest.h>

TEST(MOperandTest, CreateAndAccess)
{
    // 寄存器操作数
    auto reg_op = MOperand::create_reg(123, true);
    EXPECT_TRUE(reg_op.is_reg());
    EXPECT_EQ(reg_op.reg(), 123u);
    EXPECT_TRUE(reg_op.is_def());

    // 立即数操作数
    auto imm_op = MOperand::create_imm(42);
    EXPECT_TRUE(imm_op.is_imm());
    EXPECT_EQ(imm_op.imm(), 42);

    // 内存操作数
    auto mem_rix = MOperand::create_mem_rix(1, 2, 4, 8);
    EXPECT_TRUE(mem_rix.is_mem_rix());
    auto rix = mem_rix.get_mem_rix();
    EXPECT_EQ(rix.base_reg, 1u);
    EXPECT_EQ(rix.index_reg, 2u);
    EXPECT_EQ(rix.scale, 4);
}

TEST(MOperandTest, StatusFlags)
{
    MOperand op = MOperand::create_reg(0);
    op.set_is_kill(true);
    op.set_is_dead(true);
    EXPECT_TRUE(op.is_kill());
    EXPECT_TRUE(op.is_dead());
}

TEST(MachineBasicBlockTest, CFGManagement)
{
    MachineFunction mf(nullptr);
    auto *bb1 = mf.create_block();
    auto *bb2 = mf.create_block();

    // 测试后继添加
    bb1->add_successor(bb2);
    EXPECT_EQ(bb1->succ_size(), 1u);
    EXPECT_EQ(bb2->pred_size(), 1u);

    // 测试后继移除
    bb1->remove_successor(bb2);
    EXPECT_EQ(bb1->succ_size(), 0u);
}

TEST(MachineFunctionTest, VRegAllocation)
{
    MachineFunction mf(nullptr);
    unsigned vreg = mf.create_vreg(0, 8, true);

    const auto &info = mf.get_vreg_info(vreg);
    EXPECT_EQ(info.size_, 8u);
    EXPECT_TRUE(info.is_fp_);
}

TEST(MachineFunctionTest, GlobalInstrPosition)
{
    MachineFunction mf(nullptr);
    auto *bb = mf.create_block();

    auto inst1 = std::make_unique<MachineInst>(1);
    auto inst2 = std::make_unique<MachineInst>(2);

    bb->add_instr(std::move(inst1));
    bb->add_instr(std::move(inst2));

    mf.ensure_global_positions_computed();
    EXPECT_NE(mf.get_global_instr_pos(bb->begin()->get()),
              mf.get_global_instr_pos((++bb->begin())->get()));
}

TEST(MachineFunctionTest, FrameObjectLayout)
{
    MachineFunction mf(nullptr);
    Module mod;
    Value* dummy1 = mod.get_constant_int(32, 666);
    Value* dummy2 = mod.get_constant_int(32, 777);

    int idx1 = mf.create_frame_object(dummy1);
    int idx2 = mf.create_frame_object(dummy2);

    EXPECT_NE(idx1, idx2);
    EXPECT_EQ(mf.get_frame_index(dummy1), idx1);
    EXPECT_EQ(mf.get_frame_index(dummy2), idx2);
}
