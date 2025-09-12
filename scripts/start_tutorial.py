#!/usr/bin/env python3
"""
RISC-V Tutorial Launcher
========================
Quick launcher for the RISC-V Assembly Tutorial
"""

import sys
from pathlib import Path

# Add the current directory to Python path
sys.path.insert(0, str(Path(__file__).parent))

# Import and run the tutorial
from riscv_tutorial import RISCVTutorial

def main():
    print("🚀 Starting RISC-V Assembly Interactive Tutorial...")
    print("Make sure Docker is running before proceeding!\n")
    
    tutorial = RISCVTutorial()
    tutorial.run()

if __name__ == "__main__":
    main()