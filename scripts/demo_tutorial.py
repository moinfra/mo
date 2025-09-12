#!/usr/bin/env python3
"""
RISC-V Tutorial Demo
===================
Quick demo showing the tutorial capabilities
"""

import os
import sys
from pathlib import Path

# Add the current directory to Python path
sys.path.insert(0, str(Path(__file__).parent))

def demo_tutorial():
    """Run a quick demo of the tutorial features"""
    print("🎪 RISC-V Tutorial Demo")
    print("=" * 30)
    
    print("\n📋 This tutorial provides:")
    print("• Interactive RISC-V assembly programming")
    print("• Docker-based compilation environment")
    print("• QEMU emulation for testing")
    print("• Step-by-step learning materials")
    
    print("\n📚 Built-in Examples:")
    print("1. Simple Math: add(5, 3) = 8")
    print("2. Fibonacci: fib(7) = 13") 
    print("3. Array Sum: sum([1,2,3,4,5]) = 15")
    
    print("\n🛠️ Features:")
    print("• Create custom RISC-V programs")
    print("• Analyze existing .s files")
    print("• Learn RISC-V assembly basics")
    print("• Check system requirements")
    
    print("\n🚀 To start the tutorial:")
    print("python3 scripts/riscv_tutorial.py")
    print("# or")
    print("python3 scripts/start_tutorial.py")
    
    print("\n📖 For detailed documentation:")
    print("cat RISCV_TUTORIAL.md")
    
    print("\n✨ Ready to explore RISC-V assembly? Let's go!")
    
    # Ask if user wants to start the tutorial
    start = input("\n🎯 Start the interactive tutorial now? (y/N): ").lower().strip()
    if start == 'y':
        # Import and run the tutorial
        try:
            from riscv_tutorial import RISCVTutorial
            tutorial = RISCVTutorial()
            tutorial.run()
        except ImportError:
            print("❌ Error: Could not import tutorial module")
            print("Make sure you're running from the correct directory")
        except Exception as e:
            print(f"❌ Error starting tutorial: {e}")
    else:
        print("\n👋 Come back anytime to try the tutorial!")
        print("Happy RISC-V programming! 🚀")

if __name__ == "__main__":
    demo_tutorial()