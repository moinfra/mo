#!/usr/bin/env python3
"""
RISC-V Executor Launcher
========================
Simple launcher for the RISC-V Assembly Executor
"""

import sys
from pathlib import Path

# Add the current directory to Python path
sys.path.insert(0, str(Path(__file__).parent))

def main():
    print("🔧 RISC-V Assembly Executor")
    print("Execute any RISC-V assembly (.s) program!")
    print("Make sure Docker is running...\n")
    
    # Import and run the executor
    try:
        from riscv_executor import RISCVExecutor
        executor = RISCVExecutor()
        
        # Handle command line arguments
        if len(sys.argv) > 1:
            if sys.argv[1] == "--help":
                print("Usage:")
                print("  python3 run_executor.py           # Interactive mode")
                print("  python3 run_executor.py file.s    # Direct execution")
                return
            elif sys.argv[1].endswith('.s'):
                # Direct execution mode
                file_path = Path(sys.argv[1])
                if file_path.exists():
                    executor.execute_riscv_file(file_path)
                else:
                    print(f"❌ File not found: {file_path}")
                return
        
        # Interactive mode
        executor.run()
        
    except ImportError:
        print("❌ Error: Could not import executor module")
        print("Make sure you're running from the correct directory")
    except Exception as e:
        print(f"❌ Error starting executor: {e}")

if __name__ == "__main__":
    main()