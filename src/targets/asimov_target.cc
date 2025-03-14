#include "asimov_target.h"

namespace ASIMOV
{
    // 寄存器信息初始化实现
    ASIMOVRegisterInfo::ASIMOVRegisterInfo() : TargetRegisterInfo(Reg::TOTAL_REG)
    {
        MO_DEBUG("Initializing calling conventions for ASIMOV target\n");
        initializeCallingConventions();
        MO_DEBUG("Initializing register classes for ASIMOV target\n");
        initializeRegisterClasses();
        MO_DEBUG("Initializing registers for ASIMOV target\n");
        initializeRegisters();
    }

    void ASIMOVRegisterInfo::initializeRegisters()
    {
        /******************** 通用寄存器初始化 ​********************/
        // 设置默认属性
        for (unsigned i = R0; i <= R7; ++i)
        {
            reg_descs_[i] = {
                .spill_cost = 6, // 默认溢出代价
                // .is_callee_saved = false,
                .is_reserved = false,
                .is_allocatable = true,
                .primary_rc_id = GR32,
                .rc_mask = 0x1 // 位掩码对应GR32
            };
        }

        reg_descs_[R5].is_allocatable = false;
        reg_descs_[R5].is_reserved = false;

        // 标记保留寄存器（不可分配）
        for (auto reg : RESERVED_REGS)
        {
            reg_descs_[reg].is_reserved = true;
            reg_descs_[reg].is_allocatable = false;
            reg_descs_[reg].spill_cost = 0; // 保留寄存器无需溢出
        }

        // 特殊处理栈指针R7
        reg_descs_[R7].spill_cost = 0; // 栈指针不可溢出

        // 配置被调用者保存的寄存器
        for (auto reg : call_conventions_[CallingConv::C].callee_saved_regs)
        {
            auto rc = reg_descs_[reg].primary_rc_id;
            if (rc == GR32)
            {
                reg_descs_[reg].spill_cost = 10; // 更高溢出代价
            }
        }

        /******************** 浮点寄存器初始化 ​********************/
        for (unsigned i = F0; i <= F7; ++i)
        {
            reg_descs_[i] = {
                .spill_cost = 8, // 浮点默认溢出代价
                // .is_callee_saved = false,
                .is_reserved = false,
                .is_allocatable = true,
                .primary_rc_id = FP32,
                .rc_mask = 0x2 // 位掩码对应FP32
            };
        }

        // 配置被调用者保存的浮点寄存器
        for (auto reg : call_conventions_[CallingConv::C].callee_saved_regs)
        {
            auto rc = reg_descs_[reg].primary_rc_id;
            if (rc == FP32)
            {
                reg_descs_[reg].spill_cost = 12; // 更高溢出代价
            }
        }
    }

    void ASIMOVRegisterInfo::initializeRegisterClasses()
    {
        // 创建通用寄存器类 (GR32)
        {
            RegisterClass gr32_class;
            gr32_class.id = GR32;
            gr32_class.name = "GR32";
            gr32_class.copy_cost = 1;
            gr32_class.weight = 1;

            // 使用循环生成寄存器列表
            for (unsigned i = R0; i <= R7; ++i)
            {
                gr32_class.regs.push_back(static_cast<Reg>(i));
            }

            add_register_class(std::move(gr32_class));
        }

        // 创建浮点寄存器类 (FP32)
        {
            RegisterClass fp32_class;
            fp32_class.id = FP32;
            fp32_class.name = "FP32";
            fp32_class.copy_cost = 1;
            fp32_class.weight = 1;

            // 使用循环生成寄存器列表
            for (unsigned i = F0; i <= F7; ++i)
            {
                fp32_class.regs.push_back(static_cast<Reg>(i));
            }

            add_register_class(std::move(fp32_class));
        }
    }

    // 操作码到指令类型映射
    OpType opcode_to_type(ASIMOV::Opcode op)
    {
        switch (op)
        {
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case FADD:
        case FSUB:
        case FMUL:
        case FDIV:
        case CMP:
            return OP_TYPE_R;
        case MOVW:
        case MOVD:
        case LI:
            return OP_TYPE_I;
        case JMP:
            return OP_TYPE_J;
        case JZ:
        case JNZ:
            return OP_TYPE_B;
        case LOAD:
        case STORE:
            return OP_TYPE_M;
        default:
            MO_ERROR("Unknown opcode %s (%u)", ASIMOV::opcode_to_str(op), op);
            throw std::invalid_argument("Unknown opcode");
        }
    }

    void ASIMOVRegisterInfo::initializeCallingConventions()
    {
        // 定义被调用者保存的寄存器 (Callee-saved)
        // 这些寄存器在函数调用前后需要保持不变，如果函数内部修改了这些寄存器，需要负责保存和恢复
        std::vector<unsigned> callee_saved = {
            Reg::R6, // 栈指针通常是被调用者保存的
            Reg::R7  // 示例：R7 也是被调用者保存的
        };

        call_conventions_[CallingConv::C].callee_saved_regs.insert(callee_saved.begin(), callee_saved.end());

        // 定义调用者保存的寄存器 (Caller-saved)
        // 这些寄存器在函数调用前后可能被改变，调用者需要负责保存和恢复
        std::vector<unsigned> caller_saved = {
            Reg::R0, // 返回值寄存器通常是调用者保存的
            Reg::R1,
            Reg::R2,
            Reg::R3,
            Reg::R4,
            Reg::F0,
            Reg::F1,
            Reg::F2,
            Reg::F3,
            Reg::F4,
            Reg::F5,
            Reg::F6,
            Reg::F7};

        // 设置C调用约定的参数传递规则
        call_conventions_[CallingConv::C].arg_passing = {
            .int_regs = {
                Reg::R0,
                Reg::R1,
                Reg::R2,
                Reg::R3},
            .fp_regs = {Reg::F0, Reg::F1, Reg::F2, Reg::F3},
            .stack_align = 4,
            .shadow_space = false,
        };

        call_conventions_[CallingConv::C].temp_regs.insert(Reg::R5); // 临时寄存器

        MO_DEBUG("Initialized calling conventions for ASIMOV target %p\n", &call_conventions_);
    }

    // 操作码到字符串映射
    const char *opcode_to_str(ASIMOV::Opcode op)
    {
        switch (op)
        {
        case ADD:
            return "ADD";
        case SUB:
            return "SUB";
        case MUL:
            return "MUL";
        case DIV:
            return "DIV";
        case FADD:
            return "FADD";
        case FSUB:
            return "FSUB";
        case FMUL:
            return "FMUL";
        case FDIV:
            return "FDIV";
        case LOAD:
            return "LOAD";
        case STORE:
            return "STORE";
        case JMP:
            return "JMP";
        case CMP:
            return "CMP";
        case JZ:
            return "JZ";
        case JNZ:
            return "JNZ";
        case LI:
            return "LI";
        case MOVW:
            return "MOVW";
        case MOVD:
            return "MOVD";
        case NOP:
            return "NOP";
        case HALT:
            return "HALT";
        case RET:
            return "RET";
        case CALL:
            return "CALL";
        default:
            return "UNKNOWN";
        }
    }

    ASIMOVTargetInstInfo::ASIMOVTargetInstInfo() {}

    // 实现接口方法
    const char *ASIMOVTargetInstInfo::opcode_name(unsigned opcode) const
    {
        return opcode_to_str(static_cast<ASIMOV::Opcode>(opcode));
    }

    unsigned ASIMOVTargetInstInfo::get_inst_size(const MachineInst &MI) const
    {
        return 4; // 固定32位指令长度
    }

    bool ASIMOVTargetInstInfo::verify_instruction(const MachineInst &MI, std::string &error_msg) const
    {
        // 实现指令验证逻辑
        // 验证操作数数量和类型
        unsigned opcode = MI.opcode();
        unsigned num_operands = MI.operands().size();

        switch (static_cast<Opcode>(opcode))
        {
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case FADD:
        case FSUB:
        case FMUL:
        case FDIV:
            if (num_operands != 3)
            {
                error_msg = "R-type instruction requires 3 operands";
                return false;
            }
            break;
        case MOVW:
            if (num_operands != 2)
            {
                error_msg = "MOVW instruction requires 2 operands";
                return false;
            }
            break;
        case MOVD:
            if (num_operands != 1)
            {
                error_msg = "MOVD instruction requires 1 operand";
                return false;
            }
        case LOAD:
        case STORE:
            if (num_operands != 2)
            {
                error_msg = "Memory instruction requires 2 operands";
                return false;
            }
            break;
        case JMP:
        case CALL:
            if (num_operands != 1)
            {
                error_msg = "Jump instruction requires 1 operand";
                return false;
            }
            break;
        case JZ:
        case JNZ:
            if (num_operands != 2)
            {
                error_msg = "Conditional jump instruction requires 2 operands";
                return false;
            }
            break;
        case NOP:
        case HALT:
        case RET:
            if (num_operands != 0)
            {
                error_msg = "Instruction requires no operands";
                return false;
            }
            break;
        default:
            error_msg = "Unknown opcode";
            return false;
        }

        // 增强内存访问检查
        if (MI.opcode() == LOAD || MI.opcode() == STORE)
        {
            if (!MI.operands().back().is_mem_ri())
            {
                error_msg = "Memory operand must be [reg + offset] form";
                return false;
            }

            const auto &mem = MI.operands().back().mem_ri();
            if (mem.base_reg == R7 && mem.offset % 4 != 0)
            {
                error_msg = "Stack access requires 4-byte alignment";
                return false;
            }
        }

        // 检查分支目标有效性
        if (MI.has_flag(MIFlag::Branch))
        {
            if (!MI.operands().back().is_basic_block())
            {
                error_msg = "Branch target must be a basic block";
                return false;
            }
        }

        return true;
    }

    uint32_t ASIMOVTargetInstInfo::get_binary_encoding(const MachineInst &MI) const
    {
        unsigned opcode = MI.opcode();

        switch (static_cast<Opcode>(opcode))
        {
        case ADD:
        case SUB:
        case MUL:
        case DIV:
            return encode_R(opcode, MI);
        case FADD:
        case FSUB:
        case FMUL:
        case FDIV:
            return encode_R(opcode, MI);
        case MOVW:
            return encode_I(opcode, MI);
        case LOAD:
        case STORE:
            return encode_M(opcode, MI);
        case JMP:
            return encode_J(opcode, MI);
        case JZ:
        case JNZ:
            return encode_B(opcode, MI);
        case NOP:
            return 0x00000000; // NOP 指令编码
        case HALT:
            return 0xFFFFFFFF; // HALT 指令编码
        case RET:
            return 0x00000001; // RET 指令编码
        case CALL:
            return encode_J(opcode, MI);
        default:
            return 0; // 未知指令
        }
    }

    void ASIMOVTargetInstInfo::expand_pseudo(MachineBasicBlock &mbb,
                                             MachineBasicBlock::iterator mii) const
    {
        MachineInst &mi = **mii;
        auto next_it = std::next(mii);

        switch (mi.opcode())
        {
        case CALL:
        {
            // 展开CALL伪指令到实际指令序列
            // 1. 保存返回地址到栈中
            // 2. 调整栈指针
            // 3. 执行跳转

            // 保存返回地址到栈
            mbb.parent()->frame()->create_fixed_size(nullptr, SIZE_DWORD, ALIGN);

            // STORE R7, [R7]
            auto store = std::make_unique<MachineInst>(STORE);
            store->add_operand(MOperand::create_reg(R7));
            store->add_operand(MOperand::create_mem_ri(R7, 0));
            mbb.insert(mii, std::move(store));

            // SUB R7, R7, 4 (栈向下增长)
            auto sub = std::make_unique<MachineInst>(SUB);
            sub->add_operand(MOperand::create_reg(R7, true));
            sub->add_operand(MOperand::create_reg(R7));
            sub->add_operand(MOperand::create_imm(4));
            mbb.insert(mii, std::move(sub));

            // 获取目标地址
            MachineBasicBlock *target = mi.operands()[0].basic_block();

            // JMP target
            auto jmp = std::make_unique<MachineInst>(JMP);
            jmp->add_operand(MOperand::create_basic_block(target));
            mbb.insert(mii, std::move(jmp));

            mi.erase_from_parent();
            return;
        }
        case RET:
        {
            // 展开RET伪指令
            // 1. 恢复返回地址
            // 2. 调整栈指针
            // 3. 返回跳转

            // ADD R7, R7, 4
            auto add = std::make_unique<MachineInst>(ADD);
            add->add_operand(MOperand::create_reg(R7, true));
            add->add_operand(MOperand::create_reg(R7));
            add->add_operand(MOperand::create_imm(4));
            mbb.insert(mii, std::move(add));

            // LOAD R7, [R7]
            auto load = std::make_unique<MachineInst>(LOAD);
            load->add_operand(MOperand::create_reg(R7, true));
            load->add_operand(MOperand::create_mem_ri(R7, 0));
            mbb.insert(mii, std::move(load));

            // JMP R7
            auto jmp = std::make_unique<MachineInst>(JMP);
            jmp->add_operand(MOperand::create_reg(R7));
            mbb.insert(mii, std::move(jmp));

            mi.erase_from_parent();
            return;
        }
        case LI:
        {
            // 展开MOV到MOVW/MOVD
            if (mi.operands().size() != 2)
                break;

            const auto &src = mi.operands()[1];
            if (src.is_imm())
            {
                int64_t imm = src.imm();
                unsigned new_op = (imm > 0xFFFF || imm < -0x8000) ? MOVD : MOVW;

                auto new_mi = std::make_unique<MachineInst>(new_op);
                new_mi->add_operand(mi.operands()[0]);
                new_mi->add_operand(MOperand::create_imm(imm));
                mbb.insert(mii, std::move(new_mi));
                mi.erase_from_parent();
            }
            return;
        }
        default:
            break;
        }
    }

    bool ASIMOVTargetInstInfo::is_return(const MachineInst &MI) const
    {
        return MI.opcode() == RET;
    }

    bool ASIMOVTargetInstInfo::is_call(const MachineInst &MI) const
    {
        return MI.opcode() == CALL;
    }

    // 操作数类型判断
    bool ASIMOVTargetInstInfo::is_operand_def(unsigned op, unsigned index) const
    {
        OpType type = opcode_to_type(static_cast<ASIMOV::Opcode>(op));
        return (type == OP_TYPE_R && index == 0) ||
               (type == OP_TYPE_I && index == 0);
    }

    bool ASIMOVTargetInstInfo::is_operand_use(unsigned op, unsigned index) const
    {
        OpType type = opcode_to_type(static_cast<ASIMOV::Opcode>(op));
        switch (type)
        {
        case OP_TYPE_R:
            return index == 1 || index == 2;
        case OP_TYPE_B:
            return index == 0;
        case OP_TYPE_M:
            if (static_cast<Opcode>(op) == LOAD)
                return index == 1;
            if (static_cast<Opcode>(op) == STORE)
                return index == 0;
            return false;
        default:
            return false;
        }
    }

    // 编码辅助函数
    uint32_t ASIMOVTargetInstInfo::encode_R(unsigned opcode, const MachineInst &MI) const
    {
        // 假设指令格式为：opcode rd, rs1, rs2
        unsigned rd = MI.operands()[0].reg();
        unsigned rs1 = MI.operands()[1].reg();
        unsigned rs2 = MI.operands()[2].reg();

        return (opcode << 24) | (rd << 16) | (rs1 << 8) | rs2;
    }

    uint32_t ASIMOVTargetInstInfo::encode_I(unsigned opcode, const MachineInst &MI) const
    {
        // 假设指令格式为：opcode rd, imm
        unsigned rd = MI.operands()[0].reg();
        int imm = MI.operands()[1].imm();

        return (opcode << 24) | (rd << 16) | (imm & 0xFFFF); // 假设立即数为16位
    }

    uint32_t ASIMOVTargetInstInfo::encode_M(unsigned opcode, const MachineInst &MI) const
    {
        // 假设指令格式为：opcode rd, offset(rs1)
        unsigned rd = MI.operands()[0].reg();
        int offset = MI.operands()[1].mem_ri().offset;
        unsigned rs1 = MI.operands()[1].mem_ri().base_reg;

        return (opcode << 24) | (rd << 16) | (rs1 << 8) | (offset & 0xFF); // 假设偏移为8位
    }

    uint32_t ASIMOVTargetInstInfo::encode_J(unsigned opcode, const MachineInst &MI) const
    {
        // 假设指令格式为：opcode target
        int target = MI.operands()[0].imm();

        return (opcode << 24) | (target & 0xFFFFFF); // 假设目标地址为24位
    }

    uint32_t ASIMOVTargetInstInfo::encode_B(unsigned opcode, const MachineInst &MI) const
    {
        // 假设指令格式为：opcode rs1, target
        unsigned rs1 = MI.operands()[0].reg();
        int target = MI.operands()[1].imm();

        return (opcode << 24) | (rs1 << 16) | (target & 0xFFFF); // 假设目标地址为16位
    }

    unsigned ASIMOVTargetInstInfo::get_instruction_latency(unsigned opcode) const
    {
        switch (static_cast<Opcode>(opcode))
        {
        case MUL:
        case FMUL:
            return 3; // 乘指令3周期延迟
        case DIV:
        case FDIV:
            return 6; // 除指令6周期延迟
        case LOAD:
        case STORE:
            return 2; // 内存操作2周期
        default:
            return 1; // 其他指令1周期
        }
    }

    void ASIMOVTargetInstInfo::copy_phys_reg(
        MachineBasicBlock &mbb,
        MachineBasicBlock::iterator insert,
        unsigned dest_reg,
        unsigned src_reg) const
    {
        // 检查寄存器类别一致性
        MO_ASSERT((dest_reg < F0 || src_reg < F0) || (dest_reg >= F0 && src_reg >= F0),
                  "Cannot copy between different reg classes");

        MOperand dest = MOperand::create_reg(dest_reg, true);
        MOperand src = MOperand::create_reg(src_reg);

        // 选择适当操作码
        unsigned op = (dest_reg >= F0) ? FADD : MOVW;

        auto mi = std::make_unique<MachineInst>(op);
        mi->add_operand(dest);

        // 浮点拷贝使用 FADD Rd, Rs, F0（假设 F0=0）
        if (op == FADD)
            mi->add_operand(MOperand::create_reg(F0));

        mi->add_operand(src);
        mbb.insert(insert, std::move(mi));
    }

    bool ASIMOVTargetInstInfo::legalize_inst(
        MachineBasicBlock &mbb,
        MachineBasicBlock::iterator mii,
        MachineFunction &mf) const
    {
        MachineInst &mi = **mii;

        // MOV指令立即数超限处理(MOVD为加载32位整数)
        if (mi.opcode() == MOVW && mi.operands().size() >= 2 &&
            mi.operands()[1].is_imm())
        {
            int64_t imm = mi.operands()[1].imm();
            if (imm > 0xFFFF || imm < -0x8000)
            {
                // 替换为MOVD
                auto movd = std::make_unique<MachineInst>(MOVD);
                movd->add_operand(mi.operands()[0]);
                movd->add_operand(MOperand::create_imm(imm));
                mii = mbb.insert(mii, std::move(movd));
                mi.erase_from_parent();
                return true;
            }
        }
        return false;
    }

    MachineBasicBlock::iterator ASIMOVTargetInstInfo::insert_load_from_stack(
        MachineBasicBlock &mbb,
        MachineBasicBlock::iterator insert_point,
        unsigned dest_reg, int frame_index,
        int64_t offset) const
    {
        // 获取总偏移
        MachineFunction &mf = *mbb.parent();
        unsigned base_reg = Reg::R7; // 默认使用栈指针

        int total_offset = mf.frame()->get_frame_index_offset(frame_index) + offset;

        // 生成: LOAD dest, [R7 + offset]
        auto mi = std::make_unique<MachineInst>(LOAD);
        mi->add_operand(MOperand::create_reg(dest_reg, true));
        mi->add_operand(MOperand::create_mem_ri(base_reg, total_offset));

        return mbb.insert(insert_point, std::move(mi));
    }

    MachineBasicBlock::iterator ASIMOVTargetInstInfo::insert_store_to_stack(
        MachineBasicBlock &mbb,
        MachineBasicBlock::iterator insert_point,
        unsigned src_reg, int frame_index,
        int64_t offset) const
    {
        auto mf = mbb.parent();
        int total_offset = mf->frame()->get_frame_index_offset(frame_index) + offset;

        // 生成: STORE src, [R7 + offset]
        auto mi = std::make_unique<MachineInst>(STORE);
        mi->add_operand(MOperand::create_reg(src_reg));
        mi->add_operand(MOperand::create_mem_ri(R7, total_offset));

        return mbb.insert(insert_point, std::move(mi));
    }

    bool ASIMOVTargetInstInfo::is_legal_immediate(int64_t imm, unsigned size) const
    {
        switch (size)
        {
        case 8: // 8位立即数
            return imm >= -128 && imm <= 255;
        case 16: // 16位立即数
            return imm >= -32768 && imm <= 65535;
        case 32:         // 32位立即数
            return true; // 通过MOVD处理
        default:
            return false;
        }
    }

    // ​BranchTargets: 存储分支目标块指针的列表，需在分析前清空。
    // ​FallThrough: 用于存储顺序执行的下一个块，初始化为nullptr。
    bool ASIMOVTargetInstInfo::analyze_branch(
        MachineBasicBlock &mbb,
        MachineInst *terminator,
        std::unordered_set<MachineBasicBlock *> &branch_targets,
        MachineBasicBlock *&fall_through) const
    {
        // 初始化为无分支状态
        branch_targets.clear();
        fall_through = nullptr;

        // 如果没有终止指令，返回false
        if (!terminator || !terminator->has_flag(MIFlag::Terminator))
            return false;

        unsigned opcode = terminator->opcode();
        const auto &operands = terminator->operands();
        MachineFunction &mf = *mbb.parent();

        // 处理不同分支指令类型
        switch (static_cast<ASIMOV::Opcode>(opcode))
        {
        case JMP:
        { // 无条件跳转
            if (operands.size() != 1)
                return false; // 操作数检查
            // 操作数应为基本块标签
            if (operands[0].is_basic_block())
            {
                MachineBasicBlock *target = operands[0].basic_block();
                branch_targets.insert(target);
                // 无条件跳转无fall-through
                fall_through = nullptr;
                return true;
            }
            else
            {
                MO_NOT_IMPLEMENTED();
            }
            break;
        }
        case JZ:
        case JNZ:
        { // 条件跳转
            if (operands.size() != 2)
                return false;
            // 第一个操作数是条件寄存器，第二个是目标块
            if (operands[1].is_basic_block())
            {
                MachineBasicBlock *target = operands[1].basic_block();
                branch_targets.insert(target);
                // Fall-through块是物理顺序的下一个块
                auto next_it = std::next(mf.locate(&mbb));
                fall_through = (next_it != mbb.parent()->basic_blocks().end()) ? next_it->get() : nullptr;
                return true;
            }
            else
            {
                MO_NOT_IMPLEMENTED();
            }
            break;
        }
        case CALL:
        { // 函数调用视为无条件跳转
            if (operands.size() != 1)
                return false;
            if (operands[0].is_basic_block())
            {
                branch_targets.insert(operands[0].basic_block());
                // 函数调用后的下一条指令是fall-through
                fall_through = mbb.next_physical_block();
                return true;
            }
            else
            {
                MO_NOT_IMPLEMENTED();
            }
            // 应该还要处理函数目标，暂时不搞了
            break;
        }
        case RET: // 返回指令无后继
            fall_through = nullptr;
            return false; // 返回false表示无法继续执行
        default:
            // 其他指令不视为分支
            break;
        }

        // 默认处理：非分支指令或解析失败
        // 尝试获取隐式fall-through块
        fall_through = mbb.next_physical_block();
        return false;
    }

} // namespace ASIMOV
