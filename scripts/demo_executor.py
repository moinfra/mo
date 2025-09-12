#!/usr/bin/env python3
"""
RISC-V Executor Demo
===================
Demonstrate the capabilities of the RISC-V executor
"""

import sys
from pathlib import Path

def show_demo():
    """Show a demo of the executor capabilities"""
    print("🔧 RISC-V Assembly Executor Demo")
    print("=" * 40)
    
    print("\n📋 This tool allows you to:")
    print("• Execute any RISC-V assembly (.s) file")
    print("• Create programs interactively")
    print("• Use Docker for cross-compilation")
    print("• Run programs in QEMU emulation")
    print("• Analyze compilation results")
    
    print("\n🚀 Usage Examples:")
    print("=" * 20)
    
    print("\n1️⃣ Interactive Mode:")
    print("   python3 scripts/riscv_executor.py")
    print("   # Follow the menu to select and execute files")
    
    print("\n2️⃣ Direct Execution:")
    print("   python3 scripts/riscv_executor.py my_program.s")
    print("   # Directly compile and run a specific file")
    
    print("\n3️⃣ Quick Launcher:")
    print("   python3 scripts/run_executor.py")
    print("   # Alternative launcher script")
    
    print("\n📝 Sample Program Structure:")
    print("=" * 30)
    print("""
# my_program.s
.text
.globl main

main:
    # Your RISC-V code here
    li a0, 123        # Load immediate 123
    li a1, 456        # Load immediate 456  
    add a0, a0, a1    # a0 = a0 + a1 = 579
    ret               # Return result in a0
    """)
    
    print("\n🔧 What happens during execution:")
    print("1. 🐳 Sets up Docker environment")
    print("2. 📦 Installs RISC-V cross-compiler")
    print("3. 🔨 Compiles your .s file to ELF executable")
    print("4. 🚀 Runs the program in QEMU RISC-V emulator")
    print("5. 📊 Shows results and generated files")
    
    print("\n📁 Output files (in riscv_output/):")
    print("• program.o     - Object file")
    print("• program.elf   - Executable file")
    print("• program.bin   - Raw binary")
    print("• program.disasm - Disassembly listing")
    
    print("\n🎯 Prerequisites:")
    print("• Docker installed and running")
    print("• Python 3.6+")
    print("• Internet connection (for first run)")
    
    print("\n📖 Quick RISC-V Reference:")
    print("• a0-a7: Function arguments/return values")
    print("• t0-t6: Temporary registers")
    print("• li rd, imm: Load immediate")
    print("• add rd, rs1, rs2: Add registers")
    print("• ret: Return from function")
    
    print("\n🌟 Ready to try it?")
    print("Create a .s file and run:")
    print("  python3 scripts/riscv_executor.py your_file.s")
    
    print("\n✨ Happy RISC-V programming! 🚀")

def main():
    if len(sys.argv) > 1 and sys.argv[1] in ['--demo', '-d']:
        show_demo()
        return
    
    # Try to launch the actual executor
    try:
        sys.path.insert(0, str(Path(__file__).parent))
        from riscv_executor import RISCVExecutor
        
        show_demo()
        print("\n" + "=" * 50)
        
        launch = input("🚀 Launch the interactive executor now? (y/N): ").lower().strip()
        if launch == 'y':
            print("\nStarting RISC-V Assembly Executor...")
            executor = RISCVExecutor()
            executor.run()
        else:
            print("👋 Come back anytime to try the executor!")
            
    except ImportError:
        show_demo()
        print("\n❌ Could not import executor module")
        print("Make sure you're in the correct directory")
    except Exception as e:
        show_demo()
        print(f"\n❌ Error: {e}")

if __name__ == "__main__":
    main()