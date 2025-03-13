#include <gtest/gtest.h>

#include "src/lra.h"
#include "src/machine.h"
#include "src/targets/riscv_target.h"

using namespace RISCV;

TEST(LiveIntervalTest, ConstructorAndAccessors)
{
    LiveInterval interval(10, 20);
    EXPECT_EQ(10, interval.start());
    EXPECT_EQ(20, interval.end());

// Test invalid construction (if implemented with assertions)
#ifdef NDEBUG
    EXPECT_DEATH(LiveInterval(20, 10), ".*");
#endif
}

TEST(LiveIntervalTest, OverlapDetection)
{
    LiveInterval interval1(10, 20);

    // Complete overlap
    LiveInterval interval2(10, 20);
    EXPECT_TRUE(interval1.overlaps(interval2));

    // Partial overlap
    LiveInterval interval3(15, 25);
    EXPECT_TRUE(interval1.overlaps(interval3));

    // No overlap
    LiveInterval interval4(1, 10);
    EXPECT_FALSE(interval1.overlaps(interval4));

    LiveInterval interval5(25, 30);
    EXPECT_FALSE(interval1.overlaps(interval5));

    // Adjacent intervals
    LiveInterval interval6(20, 30);
    EXPECT_FALSE(interval1.overlaps(interval6));
}

TEST(LiveIntervalTest, IntervalMerging)
{
    LiveInterval interval1(10, 20);

    // Merge with identical interval
    LiveInterval interval2(10, 20);
    LiveInterval merged1 = interval1.merge(interval2);
    EXPECT_EQ(10, merged1.start());
    EXPECT_EQ(20, merged1.end());

    // Merge with overlapping interval
    LiveInterval interval3(15, 25);
    LiveInterval merged2 = interval1.merge(interval3);
    EXPECT_EQ(10, merged2.start());
    EXPECT_EQ(25, merged2.end());

    // Merge with adjacent interval: [10,20) + [20,30) → [10,30)
    LiveInterval interval4(20, 30);
    LiveInterval merged3 = interval1.merge(interval4);
    EXPECT_EQ(10, merged3.start());
    EXPECT_EQ(30, merged3.end());
}

TEST(LiveIntervalTest, ComparisonOperator)
{
    LiveInterval interval1(10, 20);
    LiveInterval interval2(15, 25);
    LiveInterval interval3(10, 25);

    EXPECT_TRUE(interval1 < interval2); // Different start points
    EXPECT_TRUE(interval1 < interval3); // Same start, different end
    EXPECT_FALSE(interval2 < interval1);
    EXPECT_FALSE(interval3 < interval1);
}

TEST(LiveRangeTest, ConstructionAndBasicOps)
{
    LiveRange range(100); // Virtual register 100

    EXPECT_EQ(100, range.vreg());
    EXPECT_FALSE(range.is_allocated());
    EXPECT_FALSE(range.is_spilled());
    EXPECT_EQ(-1, range.spill_slot());
    EXPECT_TRUE(range.intervals().empty());
}

TEST(LiveRangeTest, AddingIntervals)
{
    LiveRange range(100);

    // Add single interval
    range.add_interval(10, 20);
    EXPECT_EQ(1, range.intervals().size());

    // Add non-overlapping interval
    range.add_interval(30, 40);
    EXPECT_EQ(2, range.intervals().size());

    // Add overlapping interval (should merge)
    range.add_interval(15, 25);
    EXPECT_EQ(2, range.intervals().size()); // Still 2 after merge

    // Add adjacent interval (should merge)
    range.add_interval(20, 30);
    EXPECT_EQ(1, range.intervals().size()); // Now 1 after merging all

    // Verify the final interval
    EXPECT_EQ(10, range.intervals()[0].start());
    EXPECT_EQ(40, range.intervals()[0].end());
}

TEST(LiveRangeTest, LiveAtPosition)
{
    LiveRange range(100);
    range.add_interval(10, 20);
    range.add_interval(30, 40);

    // Inside first interval
    EXPECT_TRUE(range.live_at(15));

    // Inside second interval
    EXPECT_TRUE(range.live_at(35));

    // Between intervals
    EXPECT_FALSE(range.live_at(25));

    // Before all intervals
    EXPECT_FALSE(range.live_at(5));

    // After all intervals
    EXPECT_FALSE(range.live_at(45));

    // At interval boundaries
    EXPECT_TRUE(range.live_at(10));  // Start is inclusive
    EXPECT_FALSE(range.live_at(20)); // End is exclusive
    EXPECT_TRUE(range.live_at(19));
}

TEST(LiveRangeTest, InterferenceDetection)
{
    LiveRange range1(100);
    range1.add_interval(10, 20);
    range1.add_interval(30, 40);

    // Completely overlapping range
    LiveRange range2(101);
    range2.add_interval(10, 40);
    EXPECT_TRUE(range1.interferes_with(range2));
    EXPECT_TRUE(range2.interferes_with(range1));

    // Partially overlapping range
    LiveRange range3(102);
    range3.add_interval(15, 35);
    EXPECT_TRUE(range1.interferes_with(range3));
    EXPECT_TRUE(range3.interferes_with(range1));

    // Non-overlapping range
    LiveRange range4(103);
    range4.add_interval(21, 29);
    EXPECT_FALSE(range1.interferes_with(range4));
    EXPECT_FALSE(range4.interferes_with(range1));

    // Adjacent intervals
    LiveRange range5(104);
    range5.add_interval(20, 30);
    EXPECT_FALSE(range1.interferes_with(range5));
    EXPECT_FALSE(range5.interferes_with(range1));
}

TEST(LiveRangeTest, MergeAdjacentIntervals)
{
    LiveRange range(100);
    range.add_interval(10, 20);
    range.add_interval(20, 30); // 相邻区间

    EXPECT_EQ(1, range.intervals().size()); // 应合并为 [10,30)
    EXPECT_EQ(10, range.intervals()[0].start());
    EXPECT_EQ(30, range.intervals()[0].end());
}

TEST(LiveRangeTest, NonAdjacentIntervalsRemainSeparate)
{
    LiveRange range(100);
    range.add_interval(10, 20);
    range.add_interval(30, 40); // 非相邻区间

    EXPECT_EQ(2, range.intervals().size());
    EXPECT_EQ(10, range.intervals()[0].start());
    EXPECT_EQ(20, range.intervals()[0].end());
    EXPECT_EQ(30, range.intervals()[1].start());
    EXPECT_EQ(40, range.intervals()[1].end());
}

class MockMachineFunction : public MachineFunction
{
    MachineModule *mm_;

public:
    MockMachineFunction() : MachineFunction(nullptr, create_riscv_module())
    {
        mm_ = parent();
    }

    ~MockMachineFunction()
    {
        if (mm_)
        {
            delete mm_->target_inst_info();
            delete mm_->target_reg_info();
            delete mm_;
        }
    }

    MachineModule *create_riscv_module()
    {
        MachineModule *mm = new MachineModule(nullptr);
        auto abiv = RISCV::ABIVersion::ILP32;
        auto reg_info = new RISCV::RISCVRegisterInfo(abiv);
        auto inst_info = new RISCV::RISCVTargetInstInfo(abiv);
        mm->set_target_info(reg_info, inst_info);

        return mm;
    }

    // 设置一个简单的指令序列，用于基本测试
    std::vector<unsigned> setup_simple_sequence()
    {
        auto *bb = create_block();

        // 创建虚拟寄存器
        unsigned vreg0 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg1 = create_vreg(RegClass::GR32, 4, false);

        // 添加指令，创建已知的活跃范围
        // vreg0 活跃范围: 10-30
        auto mi1 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi1->add_operand(MOperand::create_reg(vreg0, true)); // 定义 vreg0
        mi1->add_operand(MOperand::create_reg(Reg::ZERO));
        mi1->add_operand(MOperand::create_imm(10));
        bb->append(std::move(mi1));

        auto mi2 = std::make_unique<MachineInst>(RISCV::ADD);
        mi2->add_operand(MOperand::create_reg(vreg1, true)); // 定义 vreg1
        mi2->add_operand(MOperand::create_reg(vreg0));       // 使用 vreg0
        mi2->add_operand(MOperand::create_reg(Reg::A0));
        bb->append(std::move(mi2));

        auto mi3 = std::make_unique<MachineInst>(RISCV::SW);
        mi3->add_operand(MOperand::create_reg(vreg0)); // 最后使用 vreg0
        mi3->add_operand(MOperand::create_mem_ri(Reg::SP, 0));
        bb->append(std::move(mi3));

        this->build_cfg();

        return {vreg0, vreg1};
    }

    /*
        Live range:
            vreg0: [0, 1), [2, 3)
            vreg1: [1, 3)
            vreg2: [2, 4)

        Interference graph:
            vreg0 <-> vreg1, vreg2
            vreg1 <-> vreg2
    */
    std::vector<unsigned> setupConflictScenario()
    {
        auto *bb = create_block();

        unsigned vreg0 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg1 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg2 = create_vreg(RegClass::GR32, 4, false);

        // 0: addi vreg0, x0, 10
        auto mi1 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi1->add_operand(MOperand::create_reg(vreg0, true));
        mi1->add_operand(MOperand::create_reg(Reg::ZERO));
        mi1->add_operand(MOperand::create_imm(10));
        bb->append(std::move(mi1));

        // 1: addi vreg1, x0, 10
        auto mi2 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi2->add_operand(MOperand::create_reg(vreg1, true));
        mi2->add_operand(MOperand::create_reg(Reg::ZERO));
        mi2->add_operand(MOperand::create_imm(20));
        bb->append(std::move(mi2));

        // 2: add vreg2, vreg0, vreg1
        auto mi3 = std::make_unique<MachineInst>(RISCV::ADD);
        mi3->add_operand(MOperand::create_reg(vreg2, true));
        mi3->add_operand(MOperand::create_reg(vreg0));
        mi3->add_operand(MOperand::create_reg(vreg1));
        bb->append(std::move(mi3));

        // 3: sw vreg2, 0(sp)
        auto mi4 = std::make_unique<MachineInst>(RISCV::SW);
        mi4->add_operand(MOperand::create_reg(vreg2));
        mi4->add_operand(MOperand::create_mem_ri(Reg::SP, 0));
        bb->append(std::move(mi4));

        this->build_cfg();

        return {vreg0, vreg1, vreg2};
    }

    std::vector<unsigned> setup_graph_scenario()
    {
        auto *bb = create_block();

        // 创建虚拟寄存器
        unsigned vreg0 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg1 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg2 = create_vreg(RegClass::GR32, 4, false);

        // 创建一个已知的干涉图结构：
        // vreg0 与 vreg1 冲突
        // vreg0 与 vreg2 不冲突
        // vreg1 与 vreg2 冲突

        auto mi1 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi1->add_operand(MOperand::create_reg(vreg0, true));
        mi1->add_operand(MOperand::create_reg(Reg::ZERO));
        mi1->add_operand(MOperand::create_imm(10));
        bb->append(std::move(mi1));

        auto mi2 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi2->add_operand(MOperand::create_reg(vreg1, true));
        mi2->add_operand(MOperand::create_reg(Reg::ZERO));
        mi2->add_operand(MOperand::create_imm(20));
        bb->append(std::move(mi2));

        // vreg0 和 vreg1 同时活跃
        auto mi3 = std::make_unique<MachineInst>(RISCV::ADD);
        mi3->add_operand(MOperand::create_reg(vreg2, true));
        mi3->add_operand(MOperand::create_reg(vreg0));
        mi3->add_operand(MOperand::create_reg(vreg1));
        bb->append(std::move(mi3));

        // vreg0 结束活跃
        auto mi4 = std::make_unique<MachineInst>(RISCV::SW);
        mi4->add_operand(MOperand::create_reg(vreg1));
        mi4->add_operand(MOperand::create_mem_ri(Reg::SP, 0));
        bb->append(std::move(mi4));

        // vreg1 和 vreg2 同时活跃
        auto mi5 = std::make_unique<MachineInst>(RISCV::SW);
        mi5->add_operand(MOperand::create_reg(vreg2));
        mi5->add_operand(MOperand::create_mem_ri(Reg::SP, 4));
        bb->append(std::move(mi5));

        this->build_cfg();

        return {vreg0, vreg1, vreg2};
    }

    // 设置一个复杂场景，包含分支和循环
    std::vector<unsigned> setup_complex_scenario()
    {
        // 创建基本块
        auto *entry_bb = create_block("entry");
        auto *loop_bb = create_block("loop");
        auto *exit_bb = create_block("exit");

        auto loop_bb_sym = MOperand::create_basic_block(loop_bb);

        // 创建虚拟寄存器
        unsigned vreg0 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg1 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg2 = create_vreg(RegClass::GR32, 4, false);

        // 设置 CFG
        entry_bb->add_successor(loop_bb);
        loop_bb->add_successor(loop_bb); // 自循环
        loop_bb->add_successor(exit_bb);

        // 入口基本块
        auto mi1 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi1->add_operand(MOperand::create_reg(vreg0, true));
        mi1->add_operand(MOperand::create_reg(Reg::ZERO));
        mi1->add_operand(MOperand::create_imm(10));
        entry_bb->append(std::move(mi1));

        auto mi2 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi2->add_operand(MOperand::create_reg(vreg1, true));
        mi2->add_operand(MOperand::create_reg(Reg::ZERO));
        mi2->add_operand(MOperand::create_imm(0));
        entry_bb->append(std::move(mi2));

        auto branch1 = std::make_unique<MachineInst>(RISCV::JAL);
        branch1->add_operand(MOperand::create_reg(Reg::ZERO));
        branch1->add_operand(loop_bb_sym); // 跳转到 loop_bb
        branch1->set_flag(MIFlag::Terminator);
        entry_bb->append(std::move(branch1));

        // 循环基本块
        auto mi3 = std::make_unique<MachineInst>(RISCV::ADD);
        mi3->add_operand(MOperand::create_reg(vreg1, true));
        mi3->add_operand(MOperand::create_reg(vreg1));
        mi3->add_operand(MOperand::create_reg(vreg0));
        loop_bb->append(std::move(mi3));

        auto mi4 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi4->add_operand(MOperand::create_reg(vreg0, true));
        mi4->add_operand(MOperand::create_reg(vreg0));
        mi4->add_operand(MOperand::create_imm(-1));
        loop_bb->append(std::move(mi4));

        auto mi5 = std::make_unique<MachineInst>(RISCV::BNE);
        mi5->add_operand(MOperand::create_reg(vreg0));
        mi5->add_operand(MOperand::create_reg(Reg::ZERO));
        mi5->add_operand(loop_bb_sym); // 如果 vreg0 != 0，跳回 loop_bb
        mi5->set_flag(MIFlag::Terminator);
        loop_bb->append(std::move(mi5));

        // 出口基本块
        auto mi6 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi6->add_operand(MOperand::create_reg(vreg2, true));
        mi6->add_operand(MOperand::create_reg(vreg1));
        mi6->add_operand(MOperand::create_imm(1));
        exit_bb->append(std::move(mi6));

        auto mi7 = std::make_unique<MachineInst>(RISCV::RET);
        exit_bb->append(std::move(mi7));

        this->build_cfg();

        return {vreg0, vreg1, vreg2};
    }

    /*
    1. 包含多个基本块（BB0, BB1, BB2, BB3），模拟条件分支和汇合。
    2. 定义5个虚拟寄存器，具有复杂的活跃区间交叠。
    3. 引入物理寄存器以测试与虚拟寄存器的冲突。
    4. 覆盖以下场景：
       - 跨基本块的活跃区间
       - 同一寄存器多次定义产生的分段活跃区间
       - 物理寄存器与虚拟寄存器冲突
       - 不同寄存器类（可选项）
       - 多对寄存器间的冲突/非冲突组合

    预期冲突关系：
        vreg0 <-> vreg1 (BB0出口活跃)
        vreg0 <-> vreg2 (BB1活跃)
        vreg1 <-> vreg3 (BB2活跃)
        vreg2 <-> vreg3 (BB3入口处活跃)
        vreg2 <-> vreg4 (活跃区间交叠)
        vreg3 <-> rA (物理寄存器冲突)
        vreg4 <-> vreg0 (分段区间交叠)
*/
    std::vector<unsigned> setup_rigorous_riscv_conflict_case()
    {
        auto *bb0 = create_block("bb0");
        auto *bb1 = create_block("bb1");
        auto *bb2 = create_block("bb2");
        auto *bb3 = create_block("bb3");

        // 创建外部符号标签（模拟基本块地址）
        auto bb1_sym = MOperand::create_basic_block(bb1);
        auto bb2_sym = MOperand::create_basic_block(bb2);
        auto bb3_sym = MOperand::create_basic_block(bb3);

        // 创建虚拟寄存器（GR32）
        unsigned vreg0 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg1 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg2 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg3 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg4 = create_vreg(RegClass::GR32, 4, false);
        unsigned vreg_temp = create_vreg(RegClass::GR32, 4, false);

        /******************** BB0 ​********************/
        // 0: ADDI vreg0, zero, 10
        auto mi0 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi0->add_operand(MOperand::create_reg(vreg0, true));
        mi0->add_operand(MOperand::create_reg(Reg::ZERO));
        mi0->add_operand(MOperand::create_imm(10));
        bb0->append(std::move(mi0));

        // 1: ADDI vreg1, zero, 20
        auto mi1 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi1->add_operand(MOperand::create_reg(vreg1, true));
        mi1->add_operand(MOperand::create_reg(Reg::ZERO));
        mi1->add_operand(MOperand::create_imm(20));
        bb0->append(std::move(mi1));

        // 2: BEQ ra, zero, bb2 （条件分支，ra 作为普通寄存器使用）
        auto mi2 = std::make_unique<MachineInst>(RISCV::BEQ);
        mi2->add_operand(MOperand::create_reg(Reg::RA));   // rs1
        mi2->add_operand(MOperand::create_reg(Reg::ZERO)); // rs2
        mi2->add_operand(bb2_sym);                         // 使用符号目标
        mi2->set_flag(MIFlag::Terminator);
        bb0->append(std::move(mi2));

        /******************** BB1 ​********************/
        // 3: ADD vreg2, vreg0, ra （使用物理寄存器 ra）
        auto mi3 = std::make_unique<MachineInst>(RISCV::ADD);
        mi3->add_operand(MOperand::create_reg(vreg2, true));
        mi3->add_operand(MOperand::create_reg(vreg0));
        mi3->add_operand(MOperand::create_reg(Reg::RA));
        bb1->append(std::move(mi3));

        // 4: ADDI vreg4, vreg2, 0 （模拟 mv 指令）
        auto mi4 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi4->add_operand(MOperand::create_reg(vreg4, true));
        mi4->add_operand(MOperand::create_reg(vreg2));
        mi4->add_operand(MOperand::create_imm(0));
        bb1->append(std::move(mi4));

        // 5: JAL bb3 （直接跳转）
        auto mi5 = std::make_unique<MachineInst>(RISCV::JAL);
        mi5->add_operand(MOperand::create_reg(Reg::ZERO));
        mi5->add_operand(bb3_sym);
        mi5->set_flag(MIFlag::Terminator);
        bb1->append(std::move(mi5));

        /******************** BB2 ​********************/
        // 6: SUB vreg3, vreg1, ra
        auto mi6 = std::make_unique<MachineInst>(RISCV::SUB);
        mi6->add_operand(MOperand::create_reg(vreg3, true));
        mi6->add_operand(MOperand::create_reg(vreg1));
        mi6->add_operand(MOperand::create_reg(Reg::RA));
        bb2->append(std::move(mi6));

        // 7: ADD vreg_temp, vreg3, zero
        auto mi7 = std::make_unique<MachineInst>(RISCV::ADD);
        mi7->add_operand(MOperand::create_reg(vreg_temp, true));
        mi7->add_operand(MOperand::create_reg(vreg3));
        mi7->add_operand(MOperand::create_reg(Reg::ZERO));
        bb2->append(std::move(mi7));

        // 8: ADDI vreg4, vreg_temp, 0
        auto mi8 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi8->add_operand(MOperand::create_reg(vreg4, true));
        mi8->add_operand(MOperand::create_reg(vreg_temp));
        mi8->add_operand(MOperand::create_imm(0));
        bb2->append(std::move(mi8));

        // 9: JAL bb3
        auto mi9 = std::make_unique<MachineInst>(RISCV::JAL);
        mi9->add_operand(MOperand::create_reg(Reg::ZERO));
        mi9->add_operand(bb3_sym);
        mi9->set_flag(MIFlag::Terminator);
        bb2->append(std::move(mi9));

        /******************** BB3 ​********************/
        // 10: ADDI vreg0, zero, 30 （重新定义）
        auto mi10 = std::make_unique<MachineInst>(RISCV::ADDI);
        mi10->add_operand(MOperand::create_reg(vreg0, true));
        mi10->add_operand(MOperand::create_reg(Reg::ZERO));
        mi10->add_operand(MOperand::create_imm(30));
        bb3->append(std::move(mi10));

        // 11: SW vreg4, 0(vreg0)
        auto mi11 = std::make_unique<MachineInst>(RISCV::SW);
        mi11->add_operand(MOperand::create_reg(vreg4));
        mi11->add_operand(MOperand::create_mem_ri(vreg0, 0));
        bb3->append(std::move(mi11));

        this->build_cfg();

        return {vreg0, vreg1, vreg2, vreg3, vreg4, Reg::RA, vreg_temp};
    }

public:
    // 为测试 compute() 调用计数添加的辅助变量
    mutable size_t compute_call_count = 0;
};

class LiveRangeAnalyzerTest : public ::testing::Test
{
protected:
    std::unique_ptr<MockMachineFunction> mf;
    std::unique_ptr<LiveRangeAnalyzer> lra;

    void SetUp() override
    {
        mf = std::make_unique<MockMachineFunction>();
        lra = std::make_unique<LiveRangeAnalyzer>(*mf);
    }
};

TEST_F(LiveRangeAnalyzerTest, ComputeAndRetrieve)
{
    auto vregs = mf->setup_simple_sequence();
    unsigned vreg0 = vregs[0]; // 使用返回的编号而非硬编码的100

    lra->compute();

    // 使用返回的vreg0而非硬编码的100
    const LiveRange &range = *lra->get_live_range(vreg0); // [0, 4)
    EXPECT_EQ(vreg0, range.vreg());

    // 验证计算出的区间是否符合预期
    ASSERT_EQ(1, range.intervals().size());
    EXPECT_EQ(0, range.intervals()[0].start());
    EXPECT_EQ(3, range.intervals()[0].end());

    // 测试获取不存在的寄存器
    EXPECT_FALSE(lra->get_live_range(-1)); // 空值
}

TEST_F(LiveRangeAnalyzerTest, LiveRangeOperations)
{
    auto vregs = mf->setup_simple_sequence();
    unsigned vreg0 = vregs[0];

    lra->compute();

    const LiveRange &range = *lra->get_live_range(vreg0); // [0, 4)
    MO_DEBUG(range.to_string().c_str());

    // 测试LiveRange的基本功能
    EXPECT_EQ(vreg0, range.vreg());
    EXPECT_FALSE(range.intervals().empty());

    // 测试区间操作
    EXPECT_TRUE(range.live_at(0));  // 区间内的点
    EXPECT_TRUE(range.live_at(1));  // 区间内的点
    EXPECT_FALSE(range.live_at(4)); // 区间外的点
    EXPECT_FALSE(range.live_at(5)); // 区间外的点
}

TEST_F(LiveRangeAnalyzerTest, Construction)
{
    EXPECT_NO_THROW(auto ret = LiveRangeAnalyzer(*mf););

    auto aliasFunc = [](unsigned reg1, unsigned reg2)
    {
        return reg1 == reg2 || (reg1 == 10 && reg2 == 20);
    };
    EXPECT_NO_THROW(auto ret = LiveRangeAnalyzer(*mf, aliasFunc););
}

TEST_F(LiveRangeAnalyzerTest, ConflictDetection)
{
    auto vregs = mf->setupConflictScenario();
    unsigned vreg0 = vregs[0];
    unsigned vreg1 = vregs[1];
    unsigned vreg2 = vregs[2];

    lra->compute();

    MO_DEBUG((*lra->get_live_range(vreg0)).to_string().c_str());
    MO_DEBUG((*lra->get_live_range(vreg1)).to_string().c_str());
    MO_DEBUG((*lra->get_live_range(vreg2)).to_string().c_str());

    // 检查所有可能的冲突组合
    EXPECT_TRUE(lra->has_conflict(vreg0, vreg1));
    EXPECT_TRUE(lra->has_conflict(vreg1, vreg0)); // 验证对称性
    EXPECT_TRUE(lra->has_conflict(vreg0, vreg2));
    EXPECT_TRUE(lra->has_conflict(vreg2, vreg0)); // 验证对称性
    EXPECT_TRUE(lra->has_conflict(vreg1, vreg2));
    EXPECT_TRUE(lra->has_conflict(vreg2, vreg1)); // 验证对称性

    // 验证自身不与自身冲突
    EXPECT_FALSE(lra->has_conflict(vreg0, vreg0));
    EXPECT_FALSE(lra->has_conflict(vreg1, vreg1));
    EXPECT_FALSE(lra->has_conflict(vreg2, vreg2));
}

TEST_F(LiveRangeAnalyzerTest, RISCVRigorousConflict)
{
    auto vregs = mf->setup_rigorous_riscv_conflict_case();
    unsigned vreg0 = vregs[0];
    unsigned vreg1 = vregs[1];
    unsigned vreg2 = vregs[2];
    unsigned vreg3 = vregs[3];
    unsigned vreg4 = vregs[4];
    unsigned ra = vregs[5]; // 物理寄存器 RA
    unsigned vreg_temp = vregs[6];

    mf->dump_cfg(std::cout);
    lra->compute();

    for (unsigned i = 0; i < vregs.size(); i++)
    {
        LiveRange *lr = lra->get_live_range(vregs[i]);
        if (!lr)
        {
            MO_DEBUG("%d no live range", vregs[i]);
            continue;
        }
        MO_DEBUG((*lr).to_string().c_str());
    }

    // 关键冲突验证
    EXPECT_TRUE(lra->has_conflict(vreg0, vreg1)); // BB0出口处活跃
    EXPECT_TRUE(lra->has_conflict(vreg2, ra));    // BB1中同时活跃
    EXPECT_TRUE(lra->has_conflict(vreg3, ra));    // BB2中使用ra
    EXPECT_TRUE(lra->has_conflict(vreg4, vreg0)); // BB3中vreg4与重新定义的vreg0

    // 临时寄存器冲突
    EXPECT_TRUE(lra->has_conflict(vreg_temp, vreg3)); // BB2中的ADD指令
    EXPECT_FALSE(lra->has_conflict(vreg_temp, ra));

    // 验证不冲突的情况
    EXPECT_FALSE(lra->has_conflict(Reg::ZERO, vreg0)); // zero寄存器永不冲突
    EXPECT_FALSE(lra->has_conflict(vreg1, vreg_temp)); // 无交叠区间

    // 物理寄存器特殊处理
    EXPECT_TRUE(lra->has_conflict(ra, vreg2));             // 对称性检查
    EXPECT_FALSE(lra->has_conflict(Reg::ZERO, Reg::ZERO)); // zero自检
}

TEST_F(LiveRangeAnalyzerTest, InterferenceGraphBuilding)
{
    auto vregs = mf->setup_graph_scenario(); // 设置一个已知干涉图的场景
    unsigned vreg0 = vregs[0], vreg1 = vregs[1], vreg2 = vregs[2];

    lra->compute();
    mf->dump_cfg(std::cout);

    for (unsigned i = 0; i < vregs.size(); i++)
    {
        LiveRange *lr = lra->get_live_range(vregs[i]);
        if (!lr)
        {
            MO_DEBUG("%d no live range", vregs[i]);
            continue;
        }
        MO_DEBUG((*lr).to_string().c_str());
    }

    auto graph = lra->build_interference_graph();

    // 验证图结构
    EXPECT_TRUE(graph.find(vreg0) != graph.end());
    EXPECT_TRUE(graph[vreg0].find(vreg1) != graph[vreg0].end()); // vreg0 与 vreg1 冲突
    EXPECT_TRUE(graph[vreg1].find(vreg0) != graph[vreg1].end()); // vreg1 与 vreg0 冲突
    EXPECT_TRUE(graph[vreg0].find(vreg2) != graph[vreg0].end()); // vreg0 与 verg2 冲突
}

void print_interference_graph(const std::map<unsigned int, std::set<unsigned int>> &graph)
{
    MO_DEBUG("Interference graph:");

    auto join_as_str = [](const std::set<unsigned> &set, const std::string &sep)
    {
        std::string ret;
        auto rend = set.rbegin(); // Create a reverse iterator to the last element
        if (rend != set.rend())   // Check if the set is not empty
        {
            ret += std::to_string(*rend);
            ++rend; // Move to the second last element
            for (; rend != set.rend(); ++rend)
            {
                ret += sep;
                ret += std::to_string(*rend);
            }
        }
        return ret;
    };

    for (const auto &[reg, conflicts] : graph)
    {
        MO_DEBUG("%d -> {%s}", reg, join_as_str(conflicts, ", ").c_str());
    }
}

TEST_F(LiveRangeAnalyzerTest, ConsistentComputation)
{
    auto vregs = mf->setup_graph_scenario();

    // 第一次计算
    lra->compute();
    auto graph1 = lra->build_interference_graph();

    // 第二次计算，结果应该一致
    lra->compute();
    auto graph2 = lra->build_interference_graph();

    // 验证两次计算结果一致
    EXPECT_EQ(graph1.size(), graph2.size());
    for (const auto &[reg, conflicts] : graph1)
    {
        EXPECT_TRUE(graph2.find(reg) != graph2.end());
        EXPECT_EQ(conflicts.size(), graph2.at(reg).size());
        for (unsigned conflict_reg : conflicts)
        {
            EXPECT_TRUE(graph2.at(reg).find(conflict_reg) != graph2.at(reg).end());
        }
    }
}

TEST_F(LiveRangeAnalyzerTest, DirtyFlagHandling)
{
    lra->set_compute_metric_counter(mf->compute_call_count);
    auto vregs = mf->setup_simple_sequence();
    unsigned vreg0 = vregs[0];

    // 首次计算
    lra->compute();
    const LiveRange range1 = *lra->get_live_range(vreg0);

    // 标记为脏并重新计算
    lra->mark_dirty();
    lra->compute();

    // 验证重新计算后结果一致
    const LiveRange range2 = *lra->get_live_range(vreg0);
    EXPECT_EQ(range1.vreg(), range2.vreg());
    EXPECT_EQ(range1.intervals().size(), range2.intervals().size());

    // 验证计算调用次数增加
    EXPECT_EQ(2, mf->compute_call_count);
}

TEST_F(LiveRangeAnalyzerTest, ComplexScenario)
{
    auto vregs = mf->setup_complex_scenario(); // 设置一个复杂的指令序列，包含分支、循环等
    unsigned vreg0 = vregs[0], vreg1 = vregs[1], vreg2 = vregs[2];

    for (unsigned i = 0; i < vregs.size(); i++)
    {
        LiveRange *lr = lra->get_live_range(vregs[i]);
        if (!lr)
        {
            MO_DEBUG("%d no live range", vregs[i]);
            continue;
        }
        MO_DEBUG((*lr).to_string().c_str());
    }

    lra->compute();
    mf->dump_cfg(std::cout);

    // 验证复杂场景下的活跃范围
    const LiveRange &range1 = *lra->get_live_range(vreg0);
    MO_DEBUG("range1: %s", range1.to_string().c_str());
    EXPECT_EQ(1, range1.intervals().size()); 

    const LiveRange &range2 = *lra->get_live_range(vreg1);
    MO_DEBUG("range2: %s", range2.to_string().c_str());
    EXPECT_EQ(2, range2.intervals().size()); 

    // 验证冲突
    EXPECT_TRUE(lra->has_conflict(vreg0, vreg1));
    EXPECT_FALSE(lra->has_conflict(vreg0, vreg2));

    // 验证干涉图
    auto graph = lra->build_interference_graph();
    mf->dump_cfg(std::cout);
    print_interference_graph(graph);
    EXPECT_EQ(3, graph.size());        
    EXPECT_EQ(1, graph[vreg0].size()); 
}
