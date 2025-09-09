# Mo编译器RISC-V Target重构计划

## 问题分析

### 当前状态
- ✅ Phase 1完成：基础接口框架（TargetBase, InstructionSelector, AssemblyEmitter）
- ✅ RISC-V target骨架，但实现都是占位符
- ✅ 所有测试通过，但功能空洞

### 关键架构问题
1. **指令选择器空洞**: `RISCVInstructionSelector::selectInstructions()` 只创建空基本块
2. **汇编发射器占位符**: 只输出 "nop # placeholder" 而非真实RISC-V指令
3. **机器指令系统不完整**: RISC-V操作码已定义但未连接到指令生成
4. **缺失核心基础设施**: 无指令编码、操作数处理、寄存器分配集成

## 重构方案: Phase 2扩展计划

### Commit 6: RISC-V指令定义与编码系统
**目标**: 建立完整的RISC-V指令定义体系
```cpp
// 新文件: src/targets/riscv_instructions.h, riscv_instructions.cc
enum class RISCVOpcode : unsigned {
    ADD, SUB, MUL, DIV,    // R-type
    ADDI, SLTI, XORI,      // I-type  
    LW, LB, SW, SB,        // Load/Store
    BEQ, BNE, BLT, BGE,    // Branch
    JAL, JALR              // Jump
};

struct RISCVInstrInfo {
    RISCVOpcode opcode;
    InstrFormat format;     // R/I/S/B/U/J
    const char* mnemonic;
    std::vector<OperandType> operands;
    uint32_t encoding_mask;
};

class RISCVInstructionEncoder {
    uint32_t encodeInstruction(RISCVOpcode op, const std::vector<MOperand>& operands);
    std::string formatAssembly(RISCVOpcode op, const std::vector<MOperand>& operands);
};
```
**测试**: `tests/riscv_instruction_test.cc`
- 测试指令编码正确性  
- 测试汇编格式化
- 测试操作数验证

### Commit 7: IR到RISC-V指令映射表
**目标**: 实现IR指令到RISC-V指令的映射
```cpp  
// 新文件: src/targets/riscv_instruction_patterns.h
class RISCVInstructionPatterns {
public:
    struct Pattern {
        Opcode ir_opcode;
        std::vector<RISCVOpcode> riscv_opcodes;
        PatternMatchFn match_fn;
        InstructionGenFn gen_fn;
    };
    
    bool matchPattern(const Instruction& ir_inst, Pattern& matched_pattern);
    std::vector<MachineInst> generateInstructions(const Pattern& pattern, 
                                                  const Instruction& ir_inst);
};

// 映射示例:
// IR: %2 = add i32 %0, %1     → RISC-V: add x2, x0, x1
// IR: %2 = add i32 %0, 42     → RISC-V: addi x2, x0, 42  
// IR: %2 = load i32* %ptr     → RISC-V: lw x2, 0(x_ptr)
```
**测试**: `tests/instruction_pattern_test.cc`
- 测试基本算术运算映射
- 测试立即数模式选择
- 测试load/store地址计算

### Commit 8: 真实的指令选择实现
**目标**: 重写RISCVInstructionSelector实现真正的指令选择
```cpp
// 重构: src/targets/riscv_instruction_selector.cc
bool RISCVInstructionSelector::selectInstructions(const Function& ir_func, 
                                                 MachineFunction& machine_func) {
    for (auto& bb : ir_func.basic_blocks()) {
        auto* machine_bb = machine_func.create_block(bb->label());
        for (auto& inst : bb->instructions()) {
            selectSingleInstruction(*inst, *machine_bb);
        }
    }
    return true;
}

bool selectSingleInstruction(const Instruction& ir_inst, MachineBasicBlock& mbb) {
    Pattern matched_pattern;
    if (patterns_.matchPattern(ir_inst, matched_pattern)) {
        auto machine_insts = patterns_.generateInstructions(matched_pattern, ir_inst);
        for (auto& mi : machine_insts) {
            mbb.append(std::make_unique<MachineInst>(mi));
        }
        return true;
    }
    return false; // 未支持的指令
}
```
**测试**: `tests/real_instruction_selection_test.cc`
- IR: `%2 = add i32 %0, %1` → MachineInst(ADD, [vreg2, vreg0, vreg1])
- 测试复杂表达式的指令选择
- 测试错误处理

### Commit 9: 真实的汇编生成
**目标**: 重写RISCVAssemblyEmitter实现真正的汇编输出
```cpp
// 重构: src/targets/riscv_assembly_emitter.cc  
std::string RISCVAssemblyEmitter::emitInstruction(const MachineInst& inst) {
    auto riscv_opcode = static_cast<RISCVOpcode>(inst.opcode());
    return encoder_.formatAssembly(riscv_opcode, inst.operands());
}

std::string formatOperand(const MOperand& operand) {
    switch (operand.type()) {
        case MOperand::Register:
            return formatRegister(operand.reg());
        case MOperand::Immediate:
            return std::to_string(operand.imm());
        case MOperand::MemoryRI:
            return std::to_string(operand.offset()) + "(" + 
                   formatRegister(operand.base_reg()) + ")";
    }
}
```
**测试**: `tests/real_assembly_emission_test.cc`
- MachineInst(ADD, [x2, x0, x1]) → "add x2, x0, x1"
- MachineInst(LW, [x2, MemRI(x1, 8)]) → "lw x2, 8(x1)"
- 测试完整函数的汇编输出

### Commit 10: 寄存器分配集成
**目标**: 集成虚拟寄存器分配到物理寄存器
```cpp
// 扩展: src/targets/riscv_instruction_selector.cc
unsigned getVirtualRegForValue(const Value* ir_value) {
    if (ir_value_to_vreg_.count(ir_value)) {
        return ir_value_to_vreg_[ir_value];
    }
    
    // 根据类型选择寄存器类
    unsigned reg_class = ir_value->type()->is_float() ? FP32 : GR64;
    unsigned vreg = machine_func_.create_vreg(reg_class, 
                                            getTypeSize(ir_value->type()));
    ir_value_to_vreg_[ir_value] = vreg;
    return vreg;
}
```
**测试**: `tests/register_allocation_integration_test.cc`
- 测试虚拟寄存器生成
- 测试寄存器类选择
- 测试寄存器分配后的汇编输出

### Commit 11: 基本块和控制流
**目标**: 实现分支和跳转指令处理
```cpp
// IR: br i1 %cond, label %true_bb, label %false_bb
// → RISC-V: bne x_cond, x0, true_bb
//          j false_bb

bool selectBranchInstruction(const BranchInst& br_inst, MachineBasicBlock& mbb) {
    if (br_inst.isConditional()) {
        auto cond_reg = getVirtualRegForValue(br_inst.condition());
        auto* true_bb = getMachineBB(br_inst.true_target());
        auto* false_bb = getMachineBB(br_inst.false_target());
        
        // bne cond_reg, x0, true_bb  
        mbb.append(createMachineInst(BNE, {cond_reg, 0, true_bb}));
        // j false_bb (implicit fallthrough or explicit jump)
        if (!isNextBlock(false_bb)) {
            mbb.append(createMachineInst(JAL, {0, false_bb}));
        }
    }
}
```
**测试**: `tests/control_flow_test.cc`
- 测试条件分支生成
- 测试无条件跳转
- 测试函数调用和返回

### Commit 12: 栈管理和调用约定
**目标**: 实现函数序言/尾声和参数传递
```cpp
// 新文件: src/targets/riscv_frame_lowering.h
class RISCVFrameLowering {
    void emitPrologue(MachineFunction& mf, MachineBasicBlock& entry_bb);
    void emitEpilogue(MachineFunction& mf, MachineBasicBlock& exit_bb);
    int allocateStackSlot(MachineFunction& mf, unsigned size, unsigned alignment);
};

// 典型序言:
// addi sp, sp, -frame_size
// sw ra, frame_size-4(sp)
// sw s0, frame_size-8(sp)
```
**测试**: `tests/frame_lowering_test.cc`
- 测试栈帧建立和清理
- 测试参数传递（a0-a7寄存器）
- 测试返回值处理

## 实现优先级

### Phase 2A: 核心指令处理 (Commits 6-9)
最高优先级 - 让基本指令选择和汇编生成工作

### Phase 2B: 系统集成 (Commits 10-12)  
中等优先级 - 集成寄存器分配和控制流

### Phase 3: 完整性和优化 (后续commits)
- 内存操作优化
- 更多RISC-V指令集支持
- 性能优化和错误处理

## 立即行动计划

1. **立即修复machine.h中的缺失函数实现**
2. **开始Commit 6: RISC-V指令定义系统** 
3. **逐步替换所有占位符实现**

这个重构计划将让我们从空洞的接口转向真正可工作的RISC-V编译器后端！