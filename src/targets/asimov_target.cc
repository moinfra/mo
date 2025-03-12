#include "asimov_target.h"

namespace ASIMOV
{
    // 寄存器信息初始化实现
    ASIMOVRegisterInfo::ASIMOVRegisterInfo() : TargetRegisterInfo(Reg::TOTAL_REG)
    {
        initializeRegisters();
        initializeRegisterClasses();
        initializeCallingConventions();
    }

    void ASIMOVRegisterInfo::initializeRegisters()
    {
        // 初始化通用寄存器
        for (unsigned i = R0; i <= R7; ++i)
        {
            reg_descs_[i].primary_rc_id = GR32;
            reg_descs_[i].is_reserved = (i == R6); // R6作为栈指针
        }

        // 初始化浮点寄存器
        for (unsigned i = F0; i <= F7; ++i)
        {
            reg_descs_[i].primary_rc_id = FP32;
        }
    }

    void ASIMOVRegisterInfo::initializeRegisterClasses()
    {
        // 创建寄存器类
        auto &gr32 = register_classes_[GR32];
        gr32 = std::make_unique<RegisterClass>();
        gr32->id = GR32;
        gr32->name = "GR32";
        gr32->copy_cost = 1;
        gr32->weight = 1;
        for (unsigned i = R0; i <= R7; ++i)
        {
            gr32->regs.push_back(i);
        }

        auto &fp32 = register_classes_[FP32];
        fp32 = std::make_unique<RegisterClass>();
        fp32->id = FP32;
        fp32->name = "FP32";
        fp32->copy_cost = 1;
        fp32->weight = 1;
        for (unsigned i = F0; i <= F7; ++i)
        {
            fp32->regs.push_back(i);
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
            return OP_TYPE_R;
        case MOV:
        case MOVD:
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
            throw std::invalid_argument("Unknown opcode");
        }
    }

    void ASIMOVRegisterInfo::initializeCallingConventions()
    {
        // 确保调用约定映射有足够空间
        callee_saved_map_.resize(CallingConv::ID::NUM_CALLING_CONV);
        caller_saved_map_.resize(CallingConv::ID::NUM_CALLING_CONV);

        // 定义被调用者保存的寄存器 (Callee-saved)
        // 这些寄存器在函数调用前后需要保持不变，如果函数内部修改了这些寄存器，需要负责保存和恢复
        std::vector<unsigned> callee_saved = {
            Reg::R5, // 示例：R5 是被调用者保存的
            Reg::R6, // 栈指针通常是被调用者保存的
            Reg::R7  // 示例：R7 也是被调用者保存的
        };

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

        // 设置C调用约定
        callee_saved_map_[CallingConv::C] = callee_saved;
        caller_saved_map_[CallingConv::C] = caller_saved;

        // 参数传递规则
        ArgPassingRule c_rule;

        // 整型参数使用R0-R3
        c_rule.int_regs = {
            Reg::R0,
            Reg::R1,
            Reg::R2,
            Reg::R3};

        // 浮点参数使用F0-F3
        c_rule.fp_regs = {
            Reg::F0,
            Reg::F1,
            Reg::F2,
            Reg::F3};

        // 设置C调用约定的参数传递规则
        arg_rules_[CallingConv::C] = c_rule;
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
        case JZ:
            return "JZ";
        case JNZ:
            return "JNZ";
        case MOV:
            return "MOV";
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
        case MOV:
            if (num_operands != 2)
            {
                error_msg = "MOV instruction requires 2 operands";
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
        case MOV:
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
        // MachineInst &mi = **mii;

        // switch (mi.opcode())
        // {
        // case ASIMOV::CALL:
        // {
        //     // CALL 指令展开

        //     // 1. 获取目标地址
        //     int target = mi.operand(0).imm();

        //     // 2. 计算返回地址 (下一条指令的地址)
        //     unsigned return_address = mi.address() + 4;

        //     // 3. 将栈指针减 4 (为返回地址腾出空间)
        //     auto mi_sub = std::make_unique<MachineInst>(ASIMOV::SUB);
        //     mi_sub->add_operand(MOperand::create_reg(Reg::R6, true)); // R6 = R6 - 4
        //     mi_sub->add_operand(MOperand::create_reg(Reg::R6));
        //     mi_sub->add_operand(MOperand::create_imm(4));
        //     MI = mbb.insert(MI, std::move(mi_sub)); // 插入到当前指令之前
        //     ++MI;                                   // 移动迭代器到新插入的指令之后

        //     // 4. 将返回地址压栈
        //     auto mi_store = std::make_unique<MachineInst>(ASIMOV::STORE);
        //     mi_store->add_operand(MOperand::create_reg(return_address)); // 要保存的返回地址
        //     mi_store->add_operand(MOperand::create_mem_ri(Reg::R6, 0));  // 栈指针指向的位置
        //     MI = mbb.insert(MI, std::move(mi_store));
        //     ++MI;

        //     // 5. 跳转到目标地址
        //     auto mi_jmp = std::make_unique<MachineInst>(ASIMOV::JMP);
        //     mi_jmp->add_operand(MOperand::create_imm(target)); // 目标地址
        //     MI = mbb.insert(MI, std::move(mi_jmp));
        //     ++MI;

        //     // 6. 删除原始的 CALL 指令
        //     mi.erase_from_parent();
        //     break;
        // }
        // case ASIMOV::RET:
        // {
        //     // RET 指令展开

        //     // 1. 从栈中弹出返回地址
        //     auto mi_load = std::make_unique<MachineInst>(ASIMOV::LOAD);
        //     mi_load->add_operand(MOperand::create_reg(Reg::R7, true)); // 将返回地址加载到 R7
        //     mi_load->add_operand(MOperand::create_mem_ri(Reg::R6, 0)); // 从栈指针指向的位置加载
        //     MI = mbb.insert(MI, std::move(mi_load));
        //     ++MI;

        //     // 2. 将栈指针加 4 (恢复栈指针)
        //     auto mi_add = std::make_unique<MachineInst>(ASIMOV::ADD);
        //     mi_add->add_operand(MOperand::create_reg(Reg::R6, true)); // R6 = R6 + 4
        //     mi_add->add_operand(MOperand::create_reg(Reg::R6));
        //     mi_add->add_operand(MOperand::create_imm(4));
        //     MI = mbb.insert(MI, std::move(mi_add));
        //     ++MI;

        //     // 3. 跳转到返回地址
        //     auto mi_jmp = std::make_unique<MachineInst>(ASIMOV::JMP);
        //     mi_jmp->add_operand(MOperand::create_reg(Reg::R7)); // 跳转到 R7 中保存的地址
        //     MI = mbb.insert(MI, std::move(mi_jmp));
        //     ++MI;

        //     // 4. 删除原始的 RET 指令
        //     mi.erase_from_parent();
        //     break;
        // }
        // default:
        //     // 其他指令，不需要展开
        //     break;
        // }
    }

    bool ASIMOVTargetInstInfo::is_return(const MachineInst &MI) const
    {
        return MI.opcode() == RET;
    }

    bool ASIMOVTargetInstInfo::is_call(const MachineInst &MI) const
    {
        return MI.opcode() == CALL;
    }

    bool ASIMOVTargetInstInfo::is_legal_immediate(int64_t imm, unsigned size) const
    {
        // 简化处理，假设所有立即数都是合法的
        return true;
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
        assert((dest_reg < F0 || src_reg < F0) ||
               (dest_reg >= F0 && src_reg >= F0) &&
                   "Cannot copy between different reg classes");

        MOperand dest = MOperand::create_reg(dest_reg, true);
        MOperand src = MOperand::create_reg(src_reg);

        // 选择适当操作码
        unsigned op = (dest_reg >= F0) ? FADD : MOV;

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
        if (mi.opcode() == MOV && mi.operands().size() >= 2 &&
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

    void ASIMOVTargetInstInfo::insert_load_from_stack(
        MachineBasicBlock &mbb,
        MachineBasicBlock::iterator insert_point,
        unsigned dest_reg, int frame_index,
        int64_t offset) const
    {
        // 获取总偏移
        MachineFunction &mf = *mbb.parent();
        unsigned base_reg = Reg::R7; // 默认使用栈指针

        int total_offset = mf.frame()->get_frame_index_offset(frame_index) + offset;

        // 生成: LOAD dest, [R6 + offset]
        auto mi = std::make_unique<MachineInst>(LOAD);
        mi->add_operand(MOperand::create_reg(dest_reg, true));
        mi->add_operand(MOperand::create_mem_ri(base_reg, total_offset));

        mbb.insert(insert_point, std::move(mi));
    }

    void ASIMOVTargetInstInfo::insert_store_to_stack(
        MachineBasicBlock &mbb,
        MachineBasicBlock::iterator insert_point,
        unsigned src_reg, int frame_index,
        int64_t offset) const
    {
        auto mf = mbb.parent();
        int total_offset = mf->frame()->get_frame_index_offset(frame_index) + offset;

        // 生成: STORE src, [R6 + offset]
        auto mi = std::make_unique<MachineInst>(STORE);
        mi->add_operand(MOperand::create_reg(src_reg));
        mi->add_operand(MOperand::create_mem_ri(R6, total_offset));

        mbb.insert(insert_point, std::move(mi));
    }

    bool ASIMOVTargetInstInfo::is_legal_immediate(int64_t imm, unsigned size) const
    {
        switch (size)
        {
        case 16:
            return imm >= -32768 && imm <= 65535; // 16位符号/无符号
        case 32:
            return true; // 32位全支持
        default:
            return false;
        }
    }

} // namespace ASIMOV
