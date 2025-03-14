#include <gtest/gtest.h>
#include <fstream>

#include "src/lra.h"
#include "src/machine.h"
#include "src/reg_alloc/lsra.h"
#include "src/targets/asimov_target.h"

using namespace ASIMOV;

class MockMachineFunction : public MachineFunction
{
    MachineModule *mm_;

public:
    MockMachineFunction() : MachineFunction(nullptr, create_asimov_module())
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

    MachineModule *create_asimov_module()
    {
        MachineModule *mm = new MachineModule(nullptr);
        auto reg_info = new ASIMOVRegisterInfo();
        auto inst_info = new ASIMOVTargetInstInfo();
        mm->set_target_info(reg_info, inst_info);

        return mm;
    }

    void append_inst(MachineBasicBlock *bb, ASIMOV::Opcode opcode,
                     unsigned rd,
                     unsigned rs1,
                     unsigned rs2,
                     uint32_t imm = 0)
    {
        using namespace ASIMOV;

        auto mi = std::make_unique<MachineInst>(opcode);
        OpType type = opcode_to_type(opcode);

        switch (type)
        {
        case OP_TYPE_R:                                      // R-type: ADD, SUB等
            mi->add_operand(MOperand::create_reg(rd, true)); // def
            mi->add_operand(MOperand::create_reg(rs1));      // use
            mi->add_operand(MOperand::create_reg(rs2));      // use
            break;

        case OP_TYPE_I:                                      // I-type: MOVW等
            mi->add_operand(MOperand::create_reg(rd, true)); // def
            mi->add_operand(MOperand::create_imm(imm));      // use
            break;

        case OP_TYPE_M: // M-type: LOAD/STORE
            if (opcode == LOAD)
            {
                mi->add_operand(MOperand::create_reg(rd, true)); // def
                mi->add_operand(MOperand::create_mem_ri(rs1, imm));
            }
            else
            {                                               // STORE
                mi->add_operand(MOperand::create_reg(rs1)); // use
                mi->add_operand(MOperand::create_mem_ri(rs2, imm));
            }
            break;

        default:
            throw std::runtime_error("Unsupported instruction type");
        }

        bb->append(std::move(mi));
    }

    void append_jump(MachineBasicBlock *bb, ASIMOV::Opcode opcode, unsigned rs1, MachineBasicBlock *target, uint16_t imm = 0)
    {
        auto mi = std::make_unique<MachineInst>(opcode);
        mi->set_flag(MIFlag::Terminator);
        OpType type = opcode_to_type(opcode);

        switch (type)
        {

        case OP_TYPE_J: // J-type: JMP
            if (imm == 0)
            {
                MO_ASSERT(rs1 = 0 && target != nullptr, "Invalid jump target");
                mi->add_operand(MOperand::create_basic_block(target));
            }
            else
            {
                mi->add_operand(MOperand::create_imm(imm));
            }
            break;

        case OP_TYPE_B: // B-type: JZ/JNZ
            MO_ASSERT(rs1 != 0 && target != nullptr, "Invalid jump target");
            mi->add_operand(MOperand::create_reg(rs1)); // use
            mi->add_operand(MOperand::create_basic_block(target));
            break;
        default:
            throw std::runtime_error("Unexpected jump instruction type");
        }

        bb->append(std::move(mi));
    }

    void append_call(MachineBasicBlock *bb, const std::string &target)
    {
        auto mi = std::make_unique<MachineInst>(ASIMOV::CALL);
        mi->add_operand(MOperand::create_label(target));
        bb->append(std::move(mi));
    }

    void append_ret(MachineBasicBlock *bb)
    {
        bb->append(std::make_unique<MachineInst>(ASIMOV::RET));
    }

    void setup_simple_loop()
    {
        auto *entry = create_block("entry");
        auto *loop = create_block("loop");
        auto *exit = create_block("exit");

        auto sum = create_vreg(RegClass::GR32, 4, false);
        auto i = create_vreg(RegClass::GR32, 4, false);
        auto tmp1 = create_vreg(RegClass::GR32, 4, false);
        auto tmp2 = create_vreg(RegClass::GR32, 4, false);
        auto tmp3 = create_vreg(RegClass::GR32, 4, false);
        auto const1 = create_vreg(RegClass::GR32, 4, false);

        append_inst(entry, MOVW, i, 0, 0, 0);
        append_inst(entry, MOVW, sum, 0, 0, 0);
        append_inst(entry, MOVW, tmp1, 0, 0, 1);
        append_inst(entry, MOVW, tmp2, 0, 0, 2);
        append_inst(entry, MOVW, tmp3, 0, 0, 3);
        append_inst(entry, MOVW, const1, 0, 0, 1);
        append_jump(entry, JMP, 0, loop);

        append_inst(loop, ADD, sum, i, tmp1);
        append_inst(loop, ADD, sum, sum, tmp2);
        append_inst(loop, ADD, sum, sum, tmp3);
        append_inst(loop, SUB, i, i, const1);
        append_jump(loop, JNZ, i, loop, 0);

        append_inst(exit, ADD, R0, sum, 0, 0);
    }

    void setup_complex_sequence()
    {
        auto *entry = create_block("entry");
        auto *loop_init = create_block("loop_init");
        auto *loop_body = create_block("loop_body");
        auto *exit = create_block("exit");

        // 创建10个虚拟寄存器（GR32）
        std::array<unsigned, 10> vregs;
        for (auto &vreg : vregs)
        {
            vreg = create_vreg(RegClass::GR32, 4, false);
        }

        // 搞一个特殊的寄存器用来存比较结果
        unsigned cmp_reg = create_vreg(RegClass::GR32, 4, false);

        /******************** 入口块 ********************/
        // 初始化所有寄存器（产生交叉使用）
        for (int i = 0; i < 10; ++i)
        {
            // MOVW vreg[i], i*10
            append_inst(entry, MOVW, vregs[i], 0, 0, i * 10);
        }

        // 跳转到循环块
        append_jump(entry, JMP, 0, loop_init);

        /******************** 循环块 ********************/
        // 定义循环计数器（物理寄存器R0）
        unsigned loop_counter = create_vreg(GR32, 4, false);
        unsigned loop_cond = create_vreg(GR32, 4, false);
        append_inst(loop_init, MOVW, loop_counter, 0, 0, 0);
        append_inst(loop_init, MOVW, loop_cond, 0, 0, 10);

        // 复杂的寄存器交叉计算（所有vreg都被使用）
        for (int i = 0; i < 9; ++i)
        {
            // vreg[i] = vreg[i] + vreg[i+1] + loop_counter
            append_inst(loop_body, ADD, vregs[i], vregs[i], vregs[i + 1]);
            append_inst(loop_body, ADD, vregs[i], vregs[i], loop_counter);
        }

        // 最后一个寄存器特殊处理
        append_inst(loop_body, ADD, vregs[9], vregs[9], loop_counter);
        append_inst(loop_body, ADD, vregs[9], vregs[9], vregs[0]);

        // 内存存储压力（同时使用多个寄存器）
        for (int i = 0; i < 5; ++i)
        {
            // 存储寄存器对到内存
            append_inst(loop_body, STORE, 0, vregs[i * 2], R6, i * 8);
            append_inst(loop_body, STORE, 0, vregs[i * 2 + 1], R6, i * 8 + 4);
        }

        // 循环控制（产生寄存器-立即数混合使用）
        append_inst(loop_body, ADD, loop_counter, loop_counter, 0, 1); // loop_counter++
        append_inst(loop_body, CMP, cmp_reg, loop_counter, loop_cond); // 如果 loop_counter != loop_cond 则跳转到 loop
        append_jump(loop_body, JNZ, loop_counter, loop_body, 0);

        /******************** 退出块 ********************/
        // 将所有寄存器相加到R0
        for (int i = 0; i < 10; ++i)
        {
            append_inst(exit, ADD, R0, R0, vregs[i]);
        }

        // 函数返回
        append_ret(exit);

        return;
    }
};

TEST(LSRATest, Test)
{
    MockMachineFunction mf;
    mf.setup_simple_loop();
    mf.build_cfg();
    mf.dump_cfg(std::cout);

    std::stringstream ss_ori;
    mf.export_text(ss_ori);

    std::ofstream cfg_file("cfg.json");
    mf.export_cfg_to_json(cfg_file);
    cfg_file.close();

    std::ofstream live_ranges_file("live_ranges.json");
    mf.live_range_analyzer()->export_to_json(live_ranges_file);
    live_ranges_file.close();

    mf.live_range_analyzer()->export_to_json(std::cout);
    LinearScanRegisterAllocator allocator(mf);
    RegAllocResult result = allocator.allocate_registers();
    MO_DEBUG(result.to_string().c_str());
    EXPECT_TRUE(result.successful);
    auto allocation = allocator.get_vreg_to_preg_map();
    for (auto [vreg, preg] : allocation)
    {
        MO_DEBUG("vreg: %u -> preg: %u", vreg, preg);
    }
    auto tmp_allocation = allocator.get_vreg_to_tmp_preg_map();
    for (auto [vreg, preg] : tmp_allocation)
    {
        MO_DEBUG("vreg: %u -> preg: %u (tmp)", vreg, preg);
    }
    auto spillslots = allocator.get_vreg_to_spill_slot();
    for (auto [vreg, slot] : spillslots)
    {
        MO_DEBUG("vreg: %u -> slot: %d", vreg, slot);
    }

    allocator.apply();
    std::stringstream ss_alloc;
    mf.export_text(ss_alloc);

    MO_DEBUG("Before allocation:\n"
             "%s",
             ss_ori.str().c_str());
    MO_DEBUG("--------------\n"
             "After allocation:\n%s",
             ss_alloc.str().c_str());
}

// TEST(LSRATest, Test)
// {
//     MockMachineFunction mf;
//     mf.setup_complex_sequence();
//     mf.build_cfg();
//     mf.dump_cfg(std::cout);
//     mf.live_range_analyzer()->export_to_gantt_chart(std::cout);
//     LinearScanRegisterAllocator allocator(mf);
//     RegAllocResult result = allocator.allocate_registers();
//     MO_DEBUG(result.to_string().c_str());
//     EXPECT_TRUE(result.successful);
// }
