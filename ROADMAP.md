# Mo编译器RISC-V Target实现路线图

## 当前状态 (2024-09)
- ✅ 前端完整 (Lexer → Parser → Type Checker)
- ✅ 中端完整 (IR Generator → IR)  
- ⚠️ 后端部分完整 (缺少代码生成核心)

## 阶段1: 基础设施完善 (Week 1-2)

### 1.1 Target接口设计 
- [ ] 定义统一的Target基类接口
- [ ] 实现CodeGenerator抽象基类
- [ ] 设计AssemblyEmitter接口

### 1.2 RISC-V Target核心结构
- [ ] 实现RISCVTarget主类
- [ ] 集成现有的RISCVRegisterInfo和RISCVTargetInstInfo
- [ ] 添加RISCVFrameLowering

### 1.3 验证编译管道
- [ ] 测试 Source → AST → IR 流程
- [ ] 验证现有组件集成
- [ ] 修复发现的问题

## 阶段2: 指令选择实现 (Week 3-4)

### 2.1 指令模式定义
- [ ] 定义RISC-V指令选择模式
- [ ] 实现IR指令到RISC-V指令映射
- [ ] 处理复杂寻址模式

### 2.2 指令选择算法
- [ ] 实现基于模式匹配的指令选择
- [ ] 处理函数调用约定
- [ ] 实现栈帧管理

### 2.3 初步代码生成
- [ ] IR BasicBlock → MachineBasicBlock
- [ ] IR Function → MachineFunction  
- [ ] 生成未优化的机器码

## 阶段3: 优化和完善 (Week 5-6)

### 3.1 寄存器分配优化
- [ ] 集成现有LRA算法
- [ ] 优化寄存器分配策略
- [ ] 处理寄存器溢出

### 3.2 汇编生成
- [ ] 实现RISC-V汇编输出器
- [ ] 支持符号和标签
- [ ] 生成标准RISC-V汇编格式

### 3.3 编译器集成
- [ ] 完善mo_compiler二进制程序
- [ ] 添加命令行选项
- [ ] 错误处理和诊断

## 阶段4: 测试和验证 (Week 7-8)

### 4.1 单元测试
- [ ] RISC-V Target组件测试
- [ ] 指令选择测试
- [ ] 寄存器分配测试

### 4.2 集成测试  
- [ ] 简单程序端到端编译
- [ ] 复杂语言特性测试
- [ ] 性能基准测试

### 4.3 文档和示例
- [ ] API文档
- [ ] 使用示例
- [ ] 架构设计文档

## 里程碑目标

### M1 (Week 2): 基础框架完成
- 可以从IR生成空的MachineFunction
- 编译管道基本连通

### M2 (Week 4): 简单代码生成  
- 可以编译简单的算术表达式
- 生成基本的RISC-V指令序列

### M3 (Week 6): 完整功能实现
- 支持函数、控制流、内存操作
- 生成优化的RISC-V汇编

### M4 (Week 8): 生产就绪
- 完整测试覆盖
- 可编译复杂的Mo程序
- 性能可接受

## 技术架构

```
Source Code (.mo)
       ↓
    Lexer → Parser → Type Checker
       ↓
   IR Generator → IR Module  
       ↓
   RISC-V Target:
   - Instruction Selection
   - Register Allocation (LRA)
   - Assembly Emission
       ↓
   RISC-V Assembly (.s)
```

## 实现原则

1. **渐进式开发**: 每阶段都有可测试的输出
2. **接口优先**: 先设计接口，后实现细节  
3. **测试驱动**: 每个组件都有对应测试
4. **不破坏现有**: 保持现有代码的功能性
5. **可扩展性**: 设计支持未来添加其他Target