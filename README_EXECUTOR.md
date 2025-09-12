# 🔧 RISC-V Assembly Executor

通用的RISC-V汇编程序执行工具，支持编译和运行任意.s文件。

## 🎯 快速开始

```bash
# 1. 交互式模式 - 推荐新手
python3 scripts/riscv_executor.py

# 2. 直接执行模式  
python3 scripts/riscv_executor.py your_program.s

# 3. 查看演示
python3 scripts/demo_executor.py

# 4. 使用启动器
python3 scripts/run_executor.py
```

## ✨ 主要功能

- **📁 文件执行**: 选择并执行任意.s文件
- **✏️ 在线编程**: 交互式创建新程序
- **🐳 Docker集成**: 自动配置RISC-V环境
- **🖥️ QEMU仿真**: 实际运行程序
- **📊 结果分析**: 查看编译产物和反汇编

## 📋 工具对比

| 脚本 | 功能 | 适用场景 |
|------|------|----------|
| `riscv_executor.py` | 完整执行器 | 主要工具，功能最全 |
| `run_executor.py` | 简单启动器 | 快速启动 |
| `demo_executor.py` | 演示和说明 | 了解功能 |
| `riscv_tutorial.py` | ❌ 已废弃 | 包含内置程序 |

## 🛠️ 系统要求

- **Docker**: 必须安装并运行
- **Python 3.6+**
- **网络连接**: 首次下载Docker镜像

## 📝 示例程序

### Hello World
```assembly
# hello.s
.text
.globl main

main:
    li a0, 42    # 返回42
    ret
```

### 简单计算
```assembly
# calc.s  
.text
.globl main

main:
    li a0, 100   # a0 = 100
    li a1, 23    # a1 = 23
    add a0, a0, a1 # a0 = 100 + 23 = 123
    ret          # 返回123
```

### 函数调用
```assembly
# function.s
.text
.globl add
.globl main

add:
    add a0, a0, a1  # a0 = a0 + a1
    ret

main:
    li a0, 15       # 第一个参数
    li a1, 27       # 第二个参数  
    jal ra, add     # 调用add(15, 27)
    ret             # 返回42
```

## 🔍 执行流程

1. **选择文件**: 从当前目录或指定路径
2. **预览代码**: 可选查看源文件内容
3. **Docker设置**: 自动配置RISC-V环境
4. **编译**: 生成可执行文件
5. **仿真**: 在QEMU中运行
6. **分析**: 查看结果和输出文件

## 📁 输出文件

执行后在 `riscv_output/` 目录生成:

- `*.o` - 目标文件
- `*.elf` - 可执行文件  
- `*.bin` - 原始二进制
- `*.disasm` - 反汇编清单

## 🎓 RISC-V基础

### 寄存器
- `a0-a7`: 参数和返回值
- `t0-t6`: 临时寄存器
- `s0-s11`: 保存寄存器
- `ra`: 返回地址
- `sp`: 栈指针

### 指令
- `li rd, imm`: 加载立即数
- `add rd, rs1, rs2`: 寄存器相加
- `jal rd, label`: 函数调用
- `ret`: 返回

## 💡 使用技巧

1. **从简单开始**: 先写基本的li和ret
2. **查看输出**: 使用.disasm了解编译结果
3. **实验指令**: 尝试不同的RISC-V指令
4. **调试**: 通过返回值调试程序逻辑

## 🚨 常见问题

**Docker未运行**
```bash
# macOS/Windows: 启动Docker Desktop
# Linux: sudo systemctl start docker
```

**权限问题**
```bash
chmod +x scripts/riscv_executor.py
```

**程序不执行**
- 确保有`main:`标签
- 检查语法错误
- 确保以`.s`结尾

## 📚 相关文档

- [RISCV_EXECUTOR.md](RISCV_EXECUTOR.md) - 详细使用说明
- [Docker Hub: davidburela/riscv-emulator](https://hub.docker.com/r/davidburela/riscv-emulator)
- [RISC-V指令集手册](https://riscv.org/specifications/)

---

**开始你的RISC-V汇编之旅！🚀**