# Phase 1.5: 修复当前架构问题

## 背景
Phase 1完成了基础接口框架，但存在关键问题：
- 指令选择器只创建空基本块
- 汇编发射器只输出占位符
- machine.h中有缺失的函数实现
- 接口间缺乏真正的集成

## Phase 1.5 目标
修复当前架构的基础问题，让现有接口真正工作，为Phase 2打下坚实基础。

---

## Commit 5.1: 修复machine.h缺失实现
**目标**: 补全machine.h中缺失的函数实现
**问题**: 诊断显示多个函数只有声明没有实现

```cpp
// 修复: src/machine.cc
bool MachineInst::is_operand_def(unsigned index) const noexcept {
    // 基于操作码和操作数索引判断是否为定义操作数
    if (index >= ops_.size()) return false;
    return index == 0 && !isCompareOrStore();  // 简化实现
}

unsigned MachineBasicBlock::get_max_instr_position() const {
    return insts_.empty() ? 0 : insts_.size() - 1;
}

void MachineFunction::set_register_allocator(std::unique_ptr<RegisterAllocator> allocator) {
    reg_allocator_ = std::move(allocator);
}

std::unique_ptr<PressureTracker> MachineFunction::compute_pressure() const {
    auto tracker = std::make_unique<PressureTracker>(*tri_);
    // 基础实现：遍历所有指令计算寄存器压力
    return tracker;
}
```
**测试**: 扩展现有测试验证这些函数可调用
- 验证`is_operand_def`基本功能
- 验证压力跟踪器创建

---

## Commit 5.2: 最小化可用的指令选择
**目标**: 让RISCVInstructionSelector生成真实的机器指令
**当前问题**: 只创建空基本块，不处理IR指令

```cpp
// 重构: src/targets/riscv_instruction_selector.cc
bool RISCVInstructionSelector::selectInstructions(const Function& ir_func, 
                                                 MachineFunction& machine_func) {
    // 为每个IR基本块创建对应的机器基本块
    std::map<const BasicBlock*, MachineBasicBlock*> bb_map;
    
    for (const auto& ir_bb : ir_func.basic_blocks()) {
        auto* machine_bb = machine_func.create_block(ir_bb->label());
        bb_map[ir_bb] = machine_bb;
        
        // 处理每条IR指令
        for (const auto& ir_inst : ir_bb->instructions()) {
            selectSingleInstruction(*ir_inst, *machine_bb);
        }
    }
    
    return true;
}

private:
bool selectSingleInstruction(const Instruction& ir_inst, MachineBasicBlock& mbb) {
    switch (ir_inst.opcode()) {
        case Opcode::Add: {
            // IR: %2 = add i32 %0, %1  →  RISC-V: add vreg2, vreg0, vreg1
            auto dst_reg = getVirtualRegForValue(&ir_inst);
            auto src1_reg = getVirtualRegForValue(ir_inst.operand(0));
            auto src2_reg = getVirtualRegForValue(ir_inst.operand(1));
            
            auto add_inst = std::make_unique<MachineInst>(
                static_cast<unsigned>(RISCV::ADD),
                std::vector<MOperand>{
                    MOperand::create_reg(dst_reg, true),   // def
                    MOperand::create_reg(src1_reg, false), // use  
                    MOperand::create_reg(src2_reg, false)  // use
                }
            );
            mbb.append(std::move(add_inst));
            return true;
        }
        
        case Opcode::Return: {
            // IR: ret i32 %0  →  RISC-V: mv a0, %0; ret
            if (ir_inst.num_operands() > 0) {
                auto ret_val_reg = getVirtualRegForValue(ir_inst.operand(0));
                // mv a0, ret_val_reg
                auto mv_inst = std::make_unique<MachineInst>(
                    static_cast<unsigned>(RISCV::ADD),  // mv is add with x0
                    std::vector<MOperand>{
                        MOperand::create_reg(RISCV::A0, true),
                        MOperand::create_reg(ret_val_reg, false),
                        MOperand::create_reg(RISCV::ZERO, false)
                    }
                );
                mbb.append(std::move(mv_inst));
            }
            
            // ret (实际是 jalr x0, ra, 0)
            auto ret_inst = std::make_unique<MachineInst>(
                static_cast<unsigned>(RISCV::JALR),
                std::vector<MOperand>{
                    MOperand::create_reg(RISCV::ZERO, true),
                    MOperand::create_reg(RISCV::RA, false),
                    MOperand::create_imm(0)
                }
            );
            mbb.append(std::move(ret_inst));
            return true;
        }
        
        default:
            // 未支持的指令：插入注释指令  
            auto comment = std::make_unique<MachineInst>(
                static_cast<unsigned>(RISCV::COMMENT),
                std::vector<MOperand>{
                    MOperand::create_external_sym("# unsupported IR opcode: " + 
                                                std::to_string(static_cast<int>(ir_inst.opcode())))
                }
            );
            mbb.append(std::move(comment));
            return false;
    }
}

unsigned getVirtualRegForValue(const Value* ir_value) {
    auto it = ir_value_to_vreg_.find(ir_value);
    if (it != ir_value_to_vreg_.end()) {
        return it->second;
    }
    
    // 创建新的虚拟寄存器
    unsigned reg_class = ir_value->type()->is_float() ? 1 : 0;  // FP vs INT
    unsigned vreg = next_virtual_reg_++;
    ir_value_to_vreg_[ir_value] = vreg;
    return vreg;
}

// 添加成员变量
private:
    std::map<const Value*, unsigned> ir_value_to_vreg_;
    unsigned next_virtual_reg_ = 1000;  // 虚拟寄存器起始号
```
**测试**: `tests/minimal_instruction_selection_test.cc`
- 测试简单add指令的选择: IR add → RISC-V ADD指令
- 测试return指令的选择: IR ret → RISC-V mv + jalr
- 测试虚拟寄存器分配
- 测试未支持指令的处理

---

## Commit 5.3: 最小化可用的汇编生成  
**目标**: 让RISCVAssemblyEmitter输出真实的RISC-V汇编
**当前问题**: 只输出"nop # placeholder"

```cpp
// 重构: src/targets/riscv_assembly_emitter.cc
std::string RISCVAssemblyEmitter::emitInstruction(const MachineInst& inst) {
    auto opcode = static_cast<RISCV::Opcode>(inst.opcode());
    
    switch (opcode) {
        case RISCV::ADD: {
            // add rd, rs1, rs2
            if (inst.operands().size() >= 3) {
                return "add " + formatOperand(inst.operands()[0]) + ", " +
                              formatOperand(inst.operands()[1]) + ", " +
                              formatOperand(inst.operands()[2]);
            }
            break;
        }
        
        case RISCV::ADDI: {
            // addi rd, rs1, imm
            if (inst.operands().size() >= 3) {
                return "addi " + formatOperand(inst.operands()[0]) + ", " +
                               formatOperand(inst.operands()[1]) + ", " +
                               formatOperand(inst.operands()[2]);
            }
            break;
        }
        
        case RISCV::JALR: {
            // jalr rd, rs1, offset
            if (inst.operands().size() >= 3) {
                auto& rd = inst.operands()[0];
                auto& rs1 = inst.operands()[1]; 
                auto& offset = inst.operands()[2];
                
                // 特殊情况：ret is jalr x0, ra, 0
                if (rd.is_reg() && rd.reg() == RISCV::ZERO &&
                    rs1.is_reg() && rs1.reg() == RISCV::RA &&
                    offset.is_imm() && offset.imm() == 0) {
                    return "ret";
                }
                
                return "jalr " + formatOperand(rd) + ", " +
                               formatOperand(rs1) + ", " + 
                               formatOperand(offset);
            }
            break;
        }
        
        case RISCV::COMMENT: {
            // 注释指令
            if (inst.operands().size() >= 1) {
                return formatOperand(inst.operands()[0]);
            }
            break;
        }
        
        default:
            return "# unknown opcode: " + std::to_string(inst.opcode());
    }
    
    return "# malformed instruction";
}

std::string RISCVAssemblyEmitter::formatOperand(const MOperand& operand) const {
    switch (operand.type()) {
        case MOperand::Register:
            return formatRegister(operand.reg());
            
        case MOperand::Immediate:
            return std::to_string(operand.imm());
            
        case MOperand::ExternalSym:
            return operand.symbol();
            
        case MOperand::MemoryRI:
            return std::to_string(operand.offset()) + "(" + 
                   formatRegister(operand.base_reg()) + ")";
                   
        default:
            return "unknown_operand";
    }
}

std::string RISCVAssemblyEmitter::formatRegister(unsigned reg) const {
    // 虚拟寄存器格式化
    if (reg >= 1000) {
        return "vreg" + std::to_string(reg);
    }
    
    // RISC-V物理寄存器名称
    switch (reg) {
        case RISCV::ZERO: return "zero";
        case RISCV::RA:   return "ra";
        case RISCV::SP:   return "sp"; 
        case RISCV::A0:   return "a0";
        case RISCV::A1:   return "a1";
        default:
            return "x" + std::to_string(reg);
    }
}
```
**测试**: `tests/minimal_assembly_emission_test.cc`  
- 测试ADD指令汇编: `add vreg1001, vreg1000, vreg1002`
- 测试return汇编: `mv a0, vreg1000` 然后 `ret`  
- 测试虚拟寄存器格式化
- 测试完整函数汇编输出

---

## Commit 5.4: 端到端集成测试
**目标**: 验证IR→指令选择→汇编生成的完整流水线
**测试**: `tests/end_to_end_basic_test.cc`

```cpp
TEST(EndToEndBasicTest, SimpleAddFunction) {
    // 创建IR: 
    // define i32 @test_add(i32 %a, i32 %b) {
    //   %result = add i32 %a, %b
    //   ret i32 %result
    // }
    
    auto module = createTestModule();
    auto int_type = module->get_integer_type(32);
    auto func_type = module->get_function_type(int_type, {int_type, int_type});
    auto* func = module->create_function("test_add", func_type);
    
    auto* entry = func->create_basic_block("entry");
    auto* add_inst = entry->add_instruction(Opcode::Add, 
                                           {func->arg(0), func->arg(1)});
    entry->add_instruction(Opcode::Return, {add_inst});
    
    // 指令选择
    auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>();
    auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>();
    auto selector = std::make_unique<RISCV::RISCVInstructionSelector>(
        reg_info.get(), inst_info.get());
    
    auto machine_module = std::make_unique<MachineModule>(module.get());
    auto* machine_func = machine_module->create_machine_function(func);
    
    EXPECT_TRUE(selector->selectInstructions(*func, *machine_func));
    
    // 汇编生成
    auto emitter = std::make_unique<RISCV::RISCVAssemblyEmitter>(
        reg_info.get(), inst_info.get());
    
    std::string assembly = emitter->emit(*machine_module);
    
    // 验证输出包含期望的指令
    EXPECT_NE(assembly.find("add vreg"), std::string::npos);
    EXPECT_NE(assembly.find("ret"), std::string::npos);
    EXPECT_NE(assembly.find(".text"), std::string::npos);
}
```

---

## 实施顺序

### 立即开始 Commit 5.1
修复machine.h中的缺失实现，确保编译无错误

### 然后 Commit 5.2  
实现最小化的指令选择，支持add和return指令

### 接着 Commit 5.3
实现基本的汇编生成，输出可读的RISC-V指令

### 最后 Commit 5.4
端到端测试，验证整个流水线工作

完成Phase 1.5后，我们将拥有一个真正能工作的最小RISC-V后端！