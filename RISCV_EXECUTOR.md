# 🔧 RISC-V Assembly Executor

一个通用的交互式工具，用于编译和执行任意RISC-V汇编程序。

## ✨ 核心功能

- **📁 文件执行**：执行当前目录或任意路径的.s文件
- **✏️ 在线编程**：交互式创建和测试新程序
- **🐳 Docker集成**：自动设置RISC-V交叉编译环境
- **🖥️ QEMU仿真**：在RISC-V虚拟机中实际执行程序
- **📊 结果分析**：查看编译产物和反汇编代码
- **📖 快速参考**：内置RISC-V指令参考

## 🛠️ 系统要求

- **Python 3.6+**
- **Docker**：确保Docker已安装并运行
- **网络连接**：首次使用时下载Docker镜像

## 🚀 使用方法

### 基本使用

```bash
# 交互式模式
python3 scripts/riscv_executor.py

# 直接执行文件
python3 scripts/riscv_executor.py program.s

# 查看帮助
python3 scripts/riscv_executor.py --help
```

### 交互式菜单

```
🔧 RISC-V Assembly Executor
============================

📋 What would you like to do?
1. Execute a .s file from current directory
2. Execute a .s file from any path  
3. Create and execute a new program
4. Check system requirements
5. Show RISC-V quick reference
0. Exit
```

## 🎯 使用场景

### 1. 执行现有程序

```bash
# 将你的.s文件放在任意目录
# 运行执行器，选择菜单项 1 或 2
# 选择文件并观看执行过程
```

### 2. 创建新程序

```assembly
# 选择菜单项 3，然后输入程序：
.text
.globl main

main:
    # 你的RISC-V代码
    li a0, 123
    ret
END
```

### 3. 命令行直接执行

```bash
python3 scripts/riscv_executor.py my_program.s
```

## 🔍 执行流程

1. **文件选择**：选择要执行的.s文件
2. **预览**：可选查看源代码
3. **编译**：自动设置Docker环境并编译
4. **执行**：在QEMU中运行程序
5. **分析**：查看生成的文件和结果

## 📁 输出文件

执行完成后在`riscv_output/`目录生成：

- **`.o`** - 目标文件
- **`.elf`** - 可执行文件
- **`.bin`** - 原始二进制  
- **`.disasm`** - 反汇编清单

## 🐳 Docker环境

使用`davidburela/riscv-emulator`镜像，包含：
- QEMU RISC-V系统仿真器
- RISC-V GCC交叉编译工具链
- 完整的构建环境

## 📖 RISC-V快速参考

### 寄存器约定
```
a0-a7   : 函数参数/返回值 (a0=第一个参数和返回值)
t0-t6   : 临时寄存器 (调用者保存)
s0-s11  : 保存寄存器 (被调用者保存)
ra      : 返回地址
sp      : 栈指针
zero    : 常量0 (x0)
```

### 基本指令
```assembly
li rd, imm          # 加载立即数
add rd, rs1, rs2    # 寄存器相加
addi rd, rs1, imm   # 立即数相加
lw rd, offset(rs)   # 从内存加载字
sw rs, offset(rd)   # 存储字到内存
jal rd, label       # 跳转并链接(函数调用)
ret                 # 返回
beq rs1, rs2, label # 相等则分支
```

### 程序结构
```assembly
.text               # 代码段
.globl main         # 全局符号
main:               # 标签定义
    # 你的代码
    ret             # 返回
```

## 🎓 学习建议

1. **从简单开始**：先写基本的算术运算
2. **逐步复杂**：添加函数调用和分支
3. **观察输出**：查看反汇编了解编译结果
4. **实验特性**：尝试不同的RISC-V指令

## 🔧 故障排除

### Docker问题
```bash
# 检查Docker状态
docker ps

# 手动拉取镜像
docker pull davidburela/riscv-emulator
```

### 文件权限
```bash
chmod +x scripts/riscv_executor.py
```

### 程序不执行
- 确保程序有`main`标签
- 检查语法错误
- 查看错误输出

## 💡 示例程序

### Hello World (返回42)
```assembly
.text
.globl main

main:
    li a0, 42
    ret
```

### 简单加法
```assembly  
.text
.globl main

main:
    li a0, 15
    li a1, 27
    add a0, a0, a1    # a0 = 15 + 27 = 42
    ret
```

### 函数调用
```assembly
.text
.globl add
.globl main

add:
    add a0, a0, a1
    ret

main:
    li a0, 20
    li a1, 22
    jal ra, add       # 调用add(20, 22)
    ret               # 返回42
```

---

**准备好探索RISC-V汇编了吗？开始执行你的第一个程序！🚀**