# Mo编译器RISC-V Target实现计划

## 开发方法论：测试驱动的渐进式开发
每个commit都包含：
1. 功能实现
2. 对应的gtest测试用例  
3. 测试通过证明

## Phase 1: 基础架构 (5 commits)

### Commit 1: Target接口基类设计
**目标**: 定义统一的Target接口抽象基类
```cpp
// 新文件: src/targets/target_base.h, src/targets/target_base.cc
class TargetBase {
    virtual MachineModule* generateMachineCode(const Module& ir_module) = 0;
    virtual std::string emitAssembly(const MachineModule& machine_module) = 0;
};
```
**测试**: `tests/target_base_test.cc`
- 测试接口可继承性
- 测试虚函数调用

### Commit 2: RISC-V Target主类框架
**目标**: 实现RISCVTarget类基本结构
```cpp  
// 修改: src/targets/riscv_target.h, src/targets/riscv_target.cc
class RISCVTarget : public TargetBase {
    RISCVTarget(std::unique_ptr<RISCVRegisterInfo> reg_info,
               std::unique_ptr<RISCVTargetInstInfo> inst_info);
    MachineModule* generateMachineCode(const Module& ir_module) override;
    std::string emitAssembly(const MachineModule& machine_module) override;
};
```
**测试**: `tests/riscv_target_test.cc`
- 测试RISCVTarget构造
- 测试基本接口存在性(返回空实现)

### Commit 3: IR到MachineModule转换框架
**目标**: 建立IR到Machine IR的转换管道
```cpp
// 新文件: src/codegen/machine_module.h, machine_module.cc  
class MachineModule {
    std::vector<std::unique_ptr<MachineFunction>> functions;
    void addFunction(std::unique_ptr<MachineFunction> func);
};
```
**测试**: `tests/machine_module_test.cc`
- 测试空MachineModule创建
- 测试添加空MachineFunction
- 测试基本属性访问

### Commit 4: 指令选择接口定义
**目标**: 定义指令选择抽象接口
```cpp
// 新文件: src/codegen/instruction_selector.h
class InstructionSelector {
    virtual void selectInstructions(const Function& ir_func, 
                                  MachineFunction& machine_func) = 0;
};

// 新文件: src/targets/riscv_instruction_selector.h
class RISCVInstructionSelector : public InstructionSelector {
    void selectInstructions(const Function& ir_func,
                          MachineFunction& machine_func) override;
};
```
**测试**: `tests/instruction_selector_test.cc`
- 测试选择器接口基本调用
- 测试RISC-V选择器创建

### Commit 5: 汇编发射器接口
**目标**: 定义汇编输出接口
```cpp  
// 新文件: src/codegen/assembly_emitter.h
class AssemblyEmitter {
    virtual std::string emit(const MachineModule& module) = 0;
};

// 新文件: src/targets/riscv_assembly_emitter.h  
class RISCVAssemblyEmitter : public AssemblyEmitter {
    std::string emit(const MachineModule& module) override;
};
```
**测试**: `tests/assembly_emitter_test.cc`
- 测试空模块的汇编输出
- 测试基本格式正确性

## Phase 2: 核心功能实现 (8 commits)

### Commit 6: 简单IR指令到RISC-V映射
**目标**: 实现基本算术运算的指令选择
```cpp
// 支持的IR指令: add, sub, mul, div (整数)
// 映射到RISC-V: ADD, SUB, MUL, DIV
```
**测试**: `tests/simple_instruction_selection_test.cc`
- IR: `%2 = add i32 %0, %1` → RISC-V: `add x2, x0, x1`
- 测试所有基本算术运算

### Commit 7: 函数框架和调用约定
**目标**: 实现函数入口/出口和基本调用约定
```cpp
// 函数序言: 保存寄存器, 建立栈帧
// 函数尾声: 恢复寄存器, 清理栈帧  
// RISC-V ABI: 参数传递, 返回值处理
```
**测试**: `tests/function_lowering_test.cc`
- 测试空函数生成正确的序言/尾声
- 测试简单参数传递

### Commit 8: 基本块和控制流
**目标**: IR BasicBlock到MachineBasicBlock转换
```cpp
// 支持: br, ret, 条件分支
// 生成: 标签, 跳转指令, 条件分支
```
**测试**: `tests/control_flow_test.cc`  
- 测试无条件跳转
- 测试条件分支(beq, bne等)
- 测试return语句

### Commit 9: 内存操作
**目标**: load/store指令处理
```cpp
// IR: load, store, alloca
// RISC-V: lw, sw, 栈分配
```
**测试**: `tests/memory_operations_test.cc`
- 测试局部变量load/store
- 测试栈空间分配

### Commit 10: 简单汇编输出
**目标**: 生成可读的RISC-V汇编格式
```asm
.text
.globl main
main:
    addi sp, sp, -16
    sw ra, 12(sp)
    # ... 函数体
    lw ra, 12(sp) 
    addi sp, sp, 16
    ret
```
**测试**: `tests/assembly_output_test.cc`
- 测试汇编格式正确性
- 测试标签和指令输出

### Commit 11: 寄存器分配集成
**目标**: 集成现有LRA到代码生成管道
```cpp
// 使用虚拟寄存器生成初始代码
// 调用LRA进行寄存器分配
// 替换虚拟寄存器为物理寄存器
```
**测试**: `tests/register_allocation_test.cc`
- 测试虚拟寄存器生成
- 测试LRA分配结果应用

### Commit 12: 立即数和常量处理
**目标**: 处理常量加载和立即数优化
```cpp
// 小立即数: 直接使用立即数指令
// 大常量: lui + addi组合
// 常量池: 全局常量处理
```
**测试**: `tests/constant_handling_test.cc`
- 测试各种大小常量的处理
- 测试立即数优化

### Commit 13: 编译器集成第一版
**目标**: 更新mo_compiler支持完整管道
```cpp
// mo_compiler流程:  
// Source → AST → IR → MachineCode → Assembly
```
**测试**: `tests/end_to_end_simple_test.cc`
- 测试简单程序完整编译
- 验证生成的汇编可汇编

## Phase 3: 高级特性 (6 commits)

### Commit 14: 复杂表达式和运算符
**目标**: 支持复杂运算和类型转换
```cpp
// 比较运算: <, >, ==, !=
// 逻辑运算: &&, ||, !
// 类型转换: i32→i64, 浮点转换
```
**测试**: `tests/complex_expressions_test.cc`

### Commit 15: 数组和结构体
**目标**: 复合数据类型支持
```cpp
// 数组: 索引计算, 边界检查
// 结构体: 成员访问, 内存布局
```
**测试**: `tests/data_structures_test.cc`

### Commit 16: 函数调用优化
**目标**: 优化函数调用性能
```cpp
// 尾调用优化
// 内联候选识别  
// 调用约定优化
```
**测试**: `tests/call_optimization_test.cc`

### Commit 17: 错误处理和诊断
**目标**: 完善错误报告系统
```cpp
// 代码生成错误诊断
// 源码位置映射
// 详细错误消息
```
**测试**: `tests/error_handling_test.cc`

### Commit 18: 优化通道
**目标**: 基本代码优化
```cpp
// 死代码消除
// 常量传播  
// 简单优化
```
**测试**: `tests/optimization_test.cc`

### Commit 19: 完整编译器功能
**目标**: 生产级编译器特性
```cpp
// 命令行选项解析
// 多文件编译
// 链接器接口
```
**测试**: `tests/compiler_integration_test.cc`

## 测试策略

### 单元测试结构
```
tests/
├── codegen/           # 代码生成组件测试
├── targets/           # Target实现测试  
├── integration/       # 集成测试
└── e2e/              # 端到端测试
```

### 测试数据
```
tests/fixtures/
├── simple_programs/   # 简单测试程序
├── ir_samples/       # 示例IR代码
└── expected_output/  # 期望输出
```

### CI/CD集成
- 每个commit必须通过所有现有测试
- 新功能必须有对应测试覆盖
- 测试覆盖率维持在85%以上

## 验收标准

### 最终目标测试程序
```mo
// examples/fibonacci.mo  
fn fibonacci(n: int) -> int {
    if n <= 1 {
        return n;
    }
    return fibonacci(n-1) + fibonacci(n-2);
}

fn main() -> int {
    let result = fibonacci(10);
    return result;
}
```

### 期望输出
```bash
$ ./bazel-bin/src/mo_compiler examples/fibonacci.mo --emit-asm
=== 解析成功 ===
=== 类型检查成功 === 
=== IR生成成功 ===
=== RISC-V代码生成成功 ===

$ cat fibonacci.s
.text
.globl fibonacci
fibonacci:
    # ... 生成的RISC-V汇编
.globl main  
main:
    # ... 生成的RISC-V汇编
```

## 开发时间线
- **Week 1-2**: Phase 1 (commits 1-5)
- **Week 3-5**: Phase 2 (commits 6-13)  
- **Week 6-8**: Phase 3 (commits 14-19)
- **Week 9**: 测试完善和文档

每个commit预估1-2天开发时间，包含实现和测试。