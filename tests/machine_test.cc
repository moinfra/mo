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

TEST(MachineBasicBlockTest, CFGManagement2)
{
    MachineFunction mf(nullptr, nullptr);
    auto *bb1 = mf.create_block();
    auto *bb2 = mf.create_block();

    bb1->add_successor(bb2);
    EXPECT_EQ(bb1->succ_size(), 1u);
    EXPECT_EQ(bb2->pred_size(), 1u);

    bb1->remove_successor(bb2);
    EXPECT_EQ(bb1->succ_size(), 0u);
}

TEST(MachineFunctionTest, VRegAllocation)
{
    MachineFunction mf(nullptr, nullptr);
    unsigned vreg = mf.create_vreg(0, 8, true);

    const auto &info = mf.get_vreg_info(vreg);
    EXPECT_EQ(info.size_, 8u);
    EXPECT_TRUE(info.is_fp_);
}

TEST(MachineFunctionTest, GlobalInstrPosition)
{
    MachineFunction mf(nullptr, nullptr);
    auto *bb = mf.create_block();

    auto inst1 = std::make_unique<MachineInst>(1);
    auto inst2 = std::make_unique<MachineInst>(2);

    bb->append(std::move(inst1));
    bb->append(std::move(inst2));

    mf.ensure_global_positions_computed();
    EXPECT_NE(mf.get_global_instr_pos(bb->begin()->get()),
              mf.get_global_instr_pos((++bb->begin())->get()));
}

TEST(MachineFunctionTest, FrameObjectLayout)
{
    MachineFunction mf(nullptr, nullptr);
    Module mod;
    Value *dummy1 = mod.get_constant_int(32, 666);
    Value *dummy2 = mod.get_constant_int(32, 777);

    int idx1 = mf.create_frame_object(dummy1);
    int idx2 = mf.create_frame_object(dummy2);

    EXPECT_NE(idx1, idx2);
    EXPECT_EQ(mf.get_frame_index(dummy1), idx1);
    EXPECT_EQ(mf.get_frame_index(dummy2), idx2);
}

TEST(MOperandTest, CreateAndTypeCheck)
{
    // 测试寄存器操作数
    auto reg_op = MOperand::create_reg(42, true);
    EXPECT_TRUE(reg_op.is_reg());
    EXPECT_FALSE(reg_op.is_imm());
    EXPECT_EQ(reg_op.reg(), 42);
    EXPECT_TRUE(reg_op.is_def());

    // 测试立即数操作数
    auto imm_op = MOperand::create_imm(123);
    EXPECT_TRUE(imm_op.is_imm());
    EXPECT_FALSE(imm_op.is_reg());
    EXPECT_EQ(imm_op.imm(), 123);
}

TEST(MOperandTest, MemoryOperands)
{
    // 测试基址+偏移的内存操作数
    auto mem_ri = MOperand::create_mem_ri(10, 20);
    EXPECT_TRUE(mem_ri.is_mem_ri());
    EXPECT_EQ(mem_ri.mem_ri().base_reg, 10);
    EXPECT_EQ(mem_ri.mem_ri().offset, 20);

    // 测试基址+索引的内存操作数
    auto mem_rr = MOperand::create_mem_rr(10, 20);
    EXPECT_TRUE(mem_rr.is_mem_rr());
    EXPECT_EQ(mem_rr.mem_rr().base_reg, 10);
    EXPECT_EQ(mem_rr.mem_rr().index_reg, 20);
}

TEST(MachineInstTest, BasicOperations)
{
    // 创建指令
    MachineInst inst(42);

    // 添加操作数
    inst.add_operand(MOperand::create_reg(10, true));
    inst.add_operand(MOperand::create_imm(20));

    // 验证操作数
    EXPECT_EQ(inst.operands().size(), 2);
    EXPECT_TRUE(inst.operands()[0].is_reg());
    EXPECT_TRUE(inst.operands()[1].is_imm());

    // 测试标志位
    inst.set_flag(MIFlag::Branch);
    EXPECT_TRUE(inst.has_flag(MIFlag::Branch));
    inst.set_flag(MIFlag::Branch, false);
    EXPECT_FALSE(inst.has_flag(MIFlag::Branch));
}

TEST(MachineInstTest, RegisterAnalysis)
{
    MachineInst inst(42);
    inst.add_operand(MOperand::create_reg(10, true));  // 定义
    inst.add_operand(MOperand::create_reg(20, false)); // 使用

    std::set defs = inst.defs(), uses = inst.uses();

    EXPECT_EQ(defs.size(), 1);
    EXPECT_EQ(uses.size(), 1);
    EXPECT_TRUE(defs.count(10));
    EXPECT_TRUE(uses.count(20));
}

TEST(MachineBasicBlockTest, CFGManagement)
{
    MachineFunction mf(nullptr, nullptr);
    auto &mbb1 = *mf.create_block();
    auto &mbb2 = *mf.create_block();

    mbb1.add_successor(&mbb2);
    EXPECT_EQ(mbb1.succ_size(), 1);
    EXPECT_EQ(mbb2.pred_size(), 1);

    mbb1.remove_successor(&mbb2);
    EXPECT_EQ(mbb1.succ_size(), 0);
    EXPECT_EQ(mbb2.pred_size(), 0);
}

TEST(MachineFunctionTest, VirtualRegisterManagement)
{
    MachineFunction mf(nullptr, nullptr);
    unsigned vreg = mf.create_vreg(1, 4, false);
    EXPECT_TRUE(MachineFunction::is_virtual_reg(vreg));

    const VRegInfo &info = mf.get_vreg_info(vreg);
    EXPECT_EQ(info.register_class_id_, 1);
    EXPECT_EQ(info.size_, 4);
    EXPECT_FALSE(info.is_fp_);
}

TEST(MachineFunctionTest, FrameObjectManagement)
{
    MachineFunction mf(nullptr, nullptr);
    FrameObjectInfo info;
    info.size = 8;
    info.alignment = 4;
    info.flags = FrameObjectInfo::IsFixedSize;

    int idx = mf.create_frame_object(info);
    EXPECT_GE(idx, 0);

    const FrameObjectInfo *retrieved = mf.get_frame_object(idx);
    EXPECT_EQ(retrieved->size, 8);
    EXPECT_EQ(retrieved->alignment, 4);
    EXPECT_EQ(retrieved->flags, FrameObjectInfo::IsFixedSize);
}
