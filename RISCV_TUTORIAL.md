# 🚀 RISC-V Assembly Interactive Tutorial

这是一个交互式的Python教程，用于学习和演示RISC-V汇编程序的编译和执行过程。

## ✨ 功能特性

- **🎯 交互式学习**：通过菜单系统轻松导航
- **📚 内置示例**：包含3个精心设计的RISC-V程序示例
- **✏️ 自定义编程**：支持在线编写和测试你自己的程序
- **🔍 程序分析**：分析现有的.s汇编文件
- **📖 基础知识**：内置RISC-V汇编基础教学
- **🐳 Docker集成**：使用Docker确保环境一致性
- **🖥️ QEMU仿真**：在QEMU中实际运行程序

## 🛠️ 系统要求

- **Python 3.6+**
- **Docker**：需要Docker Desktop或Docker Engine
- **网络连接**：首次使用时下载Docker镜像

## 🚀 快速开始

### 1. 启动教程

```bash
# 方法1：直接运行
python3 scripts/riscv_tutorial.py

# 方法2：使用启动器
python3 scripts/start_tutorial.py

# 方法3：在终端中直接执行
cd scripts
./riscv_tutorial.py
```

### 2. 跟随菜单操作

教程启动后，你会看到一个交互式菜单：

```
🚀 RISC-V Assembly Interactive Tutorial
========================================

📋 What would you like to do?
1. Run a sample RISC-V program
2. Create and run your own program  
3. Analyze an existing .s file
4. Learn RISC-V basics
5. Check system requirements
0. Exit
```

## 📚 内置示例程序

### 1. **Simple Math (5 + 3)**
- 演示基本的算术运算和函数调用
- 展示RISC-V函数调用约定
- 结果：返回8

### 2. **Fibonacci Calculator**
- 递归计算第7个斐波那契数
- 展示分支、循环和递归
- 结果：返回13

### 3. **Array Sum**
- 计算数组元素的总和
- 展示内存访问和循环结构
- 结果：计算[1,2,3,4,5]的和=15

## 🎓 教学内容

### RISC-V基础知识包括：

- **寄存器**：a0-a7, t0-t6, s0-s11, ra, sp的用途
- **指令**：li, add, addi, lw, sw, jal, ret等基本指令
- **调用约定**：函数参数传递和返回值处理
- **程序结构**：.text, .data段的组织

### 实践技能：

- 编写RISC-V汇编代码
- 使用Docker环境编译程序
- 在QEMU中运行和测试
- 分析程序执行结果

## 🔧 使用示例

### 示例1：运行内置程序

```python
# 选择菜单项 1
# 选择程序 1 (Simple Math)
# 观看编译和执行过程
# 查看结果分析
```

### 示例2：创建自定义程序

```assembly
# 在教程中选择菜单项 2，然后输入：
.text
.globl main

main:
    li a0, 100        # 加载立即数100到a0
    li a1, 23         # 加载立即数23到a1
    add a0, a0, a1    # a0 = a0 + a1 = 123
    ret               # 返回结果
END
```

### 示例3：分析现有文件

```python
# 选择菜单项 3
# 从列表中选择.s文件
# 查看源代码
# 选择是否运行
```

## 🐳 Docker环境详情

教程使用`davidburela/riscv-emulator`Docker镜像，包含：

- **QEMU RISC-V系统仿真器**
- **RISC-V交叉编译工具链**
- **Ubuntu基础系统**

自动安装的工具：
- `gcc-riscv64-linux-gnu`：RISC-V GCC编译器
- `binutils-riscv64-linux-gnu`：汇编器和链接器

## 📁 目录结构

```
mo/
├── scripts/
│   ├── riscv_tutorial.py     # 主教程脚本
│   ├── start_tutorial.py     # 启动器
│   └── run-riscv.sh         # 编译运行脚本
├── examples/
│   └── *.s                  # 示例汇编文件
├── docker/
│   └── output/              # 编译输出目录
└── RISCV_TUTORIAL.md        # 本文档
```

## 🎯 学习路径建议

### 初学者：
1. 选择"Check system requirements"确认环境
2. 选择"Learn RISC-V basics"学习基础知识
3. 运行"Simple Math"示例了解基本操作
4. 尝试创建简单的自定义程序

### 进阶用户：
1. 分析"Fibonacci"和"Array Sum"示例
2. 创建包含循环和分支的复杂程序
3. 实验不同的优化技术
4. 分析生成的机器码

## 🔍 故障排除

### Docker相关问题：
```bash
# 检查Docker是否运行
docker ps

# 手动拉取镜像
docker pull davidburela/riscv-emulator

# 检查磁盘空间
docker system df
```

### 权限问题：
```bash
# 确保脚本有执行权限
chmod +x scripts/riscv_tutorial.py
chmod +x scripts/run-riscv.sh
```

### 网络问题：
- 确保网络连接正常，Docker需要下载镜像
- 在企业网络中可能需要配置代理

## 🤝 贡献和反馈

欢迎提供反馈和改进建议！这个教程是Mo编译器项目的一部分，目标是让RISC-V汇编学习更加有趣和实用。

## 📄 许可证

本教程遵循Mo编译器项目的许可证条款。

---

**Happy RISC-V Programming! 🚀**