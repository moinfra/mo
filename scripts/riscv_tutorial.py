#!/usr/bin/env python3
"""
RISC-V Assembly Tutorial - Interactive Demo
==========================================

This interactive tutorial demonstrates how to compile and run RISC-V assembly programs
using Docker and QEMU emulation.

Author: Mo Compiler Project
"""

import os
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Dict, List, Optional


class RISCVTutorial:
    def __init__(self):
        self.docker_image = "davidburela/riscv-emulator"
        self.examples_dir = Path(__file__).parent.parent / "examples"
        self.scripts_dir = Path(__file__).parent
        self.output_dir = Path(__file__).parent.parent / "docker" / "output"
        
        # Ensure output directory exists
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Sample RISC-V programs
        self.sample_programs = {
            "hello_math": {
                "name": "Simple Math (5 + 3)",
                "description": "Demonstrates basic arithmetic and function calls",
                "code": """# Simple Math Example: add(5, 3) = 8
.text
.file "hello_math.s"

.globl add
.globl main

add:
    # Function add(a: i32, b: i32) -> i32
    # Arguments: a0=a, a1=b, Return: a0=a+b
    addi sp, sp, -16
    sw ra, 12(sp)
    add a0, a0, a1      # a0 = a + b
    lw ra, 12(sp)
    addi sp, sp, 16
    ret

main:
    # Function main() -> i32
    addi sp, sp, -16
    sw ra, 12(sp)
    li a0, 5            # First argument = 5
    li a1, 3            # Second argument = 3
    jal ra, add         # Call add(5, 3)
    # Result (8) is now in a0
    lw ra, 12(sp)
    addi sp, sp, 16
    ret
"""
            },
            "fibonacci": {
                "name": "Fibonacci Calculator",
                "description": "Calculates the 7th Fibonacci number recursively",
                "code": """# Fibonacci Example: fib(7) = 13
.text
.file "fibonacci.s"

.globl fib
.globl main

fib:
    # Function fib(n: i32) -> i32
    # Arguments: a0=n, Return: a0=fib(n)
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    
    # Base case: if n <= 1, return n
    li t0, 1
    ble a0, t0, fib_base_case
    
    # Recursive case: fib(n-1) + fib(n-2)
    mv s0, a0           # Save n
    addi a0, a0, -1     # n-1
    jal ra, fib         # fib(n-1)
    mv t1, a0           # Save fib(n-1)
    
    addi a0, s0, -2     # n-2
    jal ra, fib         # fib(n-2)
    add a0, a0, t1      # fib(n-1) + fib(n-2)
    j fib_return

fib_base_case:
    # Return n (already in a0)
    j fib_return

fib_return:
    lw s0, 8(sp)
    lw ra, 12(sp)
    addi sp, sp, 16
    ret

main:
    # Calculate fib(7)
    addi sp, sp, -16
    sw ra, 12(sp)
    li a0, 7            # Calculate 7th Fibonacci number
    jal ra, fib
    # Result (13) is now in a0
    lw ra, 12(sp)
    addi sp, sp, 16
    ret
"""
            },
            "array_sum": {
                "name": "Array Sum",
                "description": "Sums an array of integers using a loop",
                "code": """# Array Sum Example: sum([1,2,3,4,5]) = 15
.text
.file "array_sum.s"

.globl array_sum
.globl main

.section .data
array: .word 1, 2, 3, 4, 5
array_size: .word 5

.section .text
array_sum:
    # Function array_sum(array_ptr, size) -> sum
    # Arguments: a0=array_ptr, a1=size
    # Return: a0=sum
    addi sp, sp, -16
    sw ra, 12(sp)
    
    li t0, 0            # sum = 0
    li t1, 0            # i = 0

loop:
    bge t1, a1, loop_end # if i >= size, exit loop
    
    slli t2, t1, 2      # t2 = i * 4 (word size)
    add t2, a0, t2      # t2 = array_ptr + i*4
    lw t3, 0(t2)        # t3 = array[i]
    add t0, t0, t3      # sum += array[i]
    
    addi t1, t1, 1      # i++
    j loop

loop_end:
    mv a0, t0           # Return sum
    
    lw ra, 12(sp)
    addi sp, sp, 16
    ret

main:
    # Calculate sum of array
    addi sp, sp, -16
    sw ra, 12(sp)
    
    la a0, array        # Load array address
    lw a1, array_size   # Load array size
    jal ra, array_sum
    # Result (15) is now in a0
    
    lw ra, 12(sp)
    addi sp, sp, 16
    ret
"""
            }
        }

    def print_banner(self):
        """Print tutorial banner"""
        print("=" * 60)
        print("🚀 RISC-V Assembly Interactive Tutorial")
        print("=" * 60)
        print("This tutorial demonstrates how to:")
        print("• Compile RISC-V assembly programs")
        print("• Run them in QEMU emulation")
        print("• Analyze the results")
        print("• Understand RISC-V assembly basics")
        print("=" * 60)
        print()

    def print_menu(self):
        """Print main menu"""
        print("\n📋 What would you like to do?")
        print("1. Run a sample RISC-V program")
        print("2. Create and run your own program")
        print("3. Analyze an existing .s file")
        print("4. Learn RISC-V basics")
        print("5. Check system requirements")
        print("0. Exit")
        print("-" * 40)

    def check_docker(self) -> bool:
        """Check if Docker is available and running"""
        try:
            result = subprocess.run(
                ["docker", "--version"],
                capture_output=True,
                text=True,
                check=True
            )
            print(f"✅ Docker found: {result.stdout.strip()}")
            
            # Test if Docker daemon is running
            result = subprocess.run(
                ["docker", "ps"],
                capture_output=True,
                text=True,
                check=True
            )
            print("✅ Docker daemon is running")
            return True
            
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("❌ Docker is not available or not running")
            print("Please install Docker and ensure it's running")
            return False

    def check_requirements(self):
        """Check system requirements"""
        print("\n🔍 Checking System Requirements...")
        print("-" * 40)
        
        docker_ok = self.check_docker()
        
        # Check if we can pull the RISC-V image
        if docker_ok:
            print("\n🐳 Checking RISC-V Docker image...")
            try:
                subprocess.run(
                    ["docker", "image", "inspect", self.docker_image],
                    capture_output=True,
                    check=True
                )
                print(f"✅ {self.docker_image} image is available")
            except subprocess.CalledProcessError:
                print(f"⚠️  {self.docker_image} image not found locally")
                print("Don't worry - it will be downloaded automatically when needed")
        
        print("\n📁 Checking directories...")
        print(f"✅ Examples dir: {self.examples_dir}")
        print(f"✅ Scripts dir: {self.scripts_dir}")
        print(f"✅ Output dir: {self.output_dir}")

    def show_sample_programs(self):
        """Show available sample programs"""
        print("\n📚 Available Sample Programs:")
        print("-" * 40)
        for i, (key, info) in enumerate(self.sample_programs.items(), 1):
            print(f"{i}. {info['name']}")
            print(f"   {info['description']}")
            print()

    def get_program_choice(self) -> Optional[str]:
        """Get user's program choice"""
        self.show_sample_programs()
        
        try:
            choice = int(input("Select a program (1-3): "))
            if 1 <= choice <= len(self.sample_programs):
                return list(self.sample_programs.keys())[choice - 1]
            else:
                print("❌ Invalid choice!")
                return None
        except ValueError:
            print("❌ Please enter a valid number!")
            return None

    def create_temp_file(self, content: str, suffix: str = ".s") -> str:
        """Create a temporary file with given content"""
        with tempfile.NamedTemporaryFile(
            mode='w', 
            suffix=suffix, 
            delete=False,
            dir=self.examples_dir
        ) as f:
            f.write(content)
            return f.name

    def run_riscv_program(self, program_path: str, program_name: str = "program"):
        """Compile and run a RISC-V program using Docker"""
        print(f"\n🔧 Compiling and Running: {program_name}")
        print("=" * 50)
        
        # Build the Docker command
        docker_cmd = [
            "docker", "run", "--rm",
            "-v", f"{self.examples_dir}:/workspace/examples",
            "-v", f"{self.scripts_dir}:/workspace/scripts", 
            "-v", f"{self.output_dir}:/workspace/output",
            self.docker_image,
            "/bin/bash", "-c",
            f"""
            apt-get update -qq &&
            apt-get install -y -qq gcc-riscv64-linux-gnu binutils-riscv64-linux-gnu &&
            echo '✅ RISC-V toolchain installed!' &&
            echo '📝 Compiling {program_name}...' &&
            /workspace/scripts/run-riscv.sh {program_path}
            """
        ]
        
        print("⚙️  Setting up Docker environment...")
        print("📦 Installing RISC-V cross-compilation tools...")
        print("🔨 Compiling program...")
        print()
        
        try:
            # Run the Docker command
            result = subprocess.run(
                docker_cmd,
                capture_output=True,
                text=True,
                timeout=120  # 2 minute timeout
            )
            
            if result.returncode == 0:
                print("✅ Compilation successful!")
                print("\n📄 Output:")
                print("-" * 30)
                # Filter out Docker warnings and apt noise
                lines = result.stdout.split('\n')
                relevant_lines = []
                capture = False
                
                for line in lines:
                    if "Compiling RISC-V assembly" in line:
                        capture = True
                    if capture and not line.startswith("WARNING:") and not line.startswith("debconf:"):
                        relevant_lines.append(line)
                
                print('\n'.join(relevant_lines))
                
                # Try to run in QEMU
                print("\n🚀 Attempting to run in QEMU...")
                self.run_in_qemu(program_name)
                
            else:
                print("❌ Compilation failed!")
                print("\n📄 Error Output:")
                print("-" * 30)
                print(result.stderr)
                
        except subprocess.TimeoutExpired:
            print("❌ Compilation timed out!")
        except Exception as e:
            print(f"❌ Error: {e}")

    def run_in_qemu(self, program_name: str):
        """Run the compiled program in QEMU"""
        elf_path = self.output_dir / f"{program_name}.elf"
        
        if not elf_path.exists():
            print("❌ Executable not found!")
            return
            
        qemu_cmd = [
            "docker", "run", "--rm",
            "-v", f"{self.output_dir}:/workspace/output",
            self.docker_image,
            "timeout", "3s",
            "qemu-system-riscv64",
            "-machine", "virt",
            "-cpu", "rv64", 
            "-m", "128M",
            "-nographic",
            "-bios", "none",
            "-kernel", f"/workspace/output/{program_name}.elf",
            "-serial", "stdio",
            "-monitor", "none"
        ]
        
        try:
            result = subprocess.run(
                qemu_cmd,
                capture_output=True,
                text=True,
                timeout=10
            )
            
            print("✅ QEMU execution completed!")
            if result.stdout:
                print(f"📤 Output: {result.stdout}")
            print(f"🔢 Exit code: {result.returncode}")
            
        except subprocess.TimeoutExpired:
            print("✅ Program executed (timed out as expected)")
        except Exception as e:
            print(f"⚠️  QEMU execution note: {e}")

    def show_riscv_basics(self):
        """Show RISC-V assembly basics"""
        print("\n📖 RISC-V Assembly Basics")
        print("=" * 40)
        print("""
🔹 REGISTERS:
  • a0-a7: Function arguments and return values
  • t0-t6: Temporary registers (caller-saved)
  • s0-s11: Saved registers (callee-saved)
  • ra: Return address
  • sp: Stack pointer

🔹 COMMON INSTRUCTIONS:
  • li rd, imm     : Load immediate
  • add rd, rs1, rs2 : Add registers
  • addi rd, rs1, imm : Add immediate
  • lw rd, offset(rs) : Load word from memory
  • sw rs, offset(rd) : Store word to memory
  • jal rd, label  : Jump and link (function call)
  • ret           : Return from function
  • beq rs1, rs2, label : Branch if equal

🔹 FUNCTION CALLING CONVENTION:
  • Arguments passed in a0, a1, a2, ...
  • Return value in a0
  • Caller saves t0-t6, caller saves s0-s11
  • Use stack for local variables

🔹 PROGRAM STRUCTURE:
  .text section: Contains executable code
  .data section: Contains initialized data
  .globl label: Makes label globally visible
        """)

    def create_custom_program(self):
        """Interactive program creation"""
        print("\n✏️  Create Your Own RISC-V Program")
        print("=" * 40)
        print("Enter your RISC-V assembly code.")
        print("Type 'END' on a new line when finished.")
        print("Example start:")
        print("""
.text
.globl main

main:
    li a0, 42    # Load immediate 42 into a0
    ret          # Return
        """)
        print("\nStart typing your code:")
        
        lines = []
        while True:
            try:
                line = input()
                if line.strip() == "END":
                    break
                lines.append(line)
            except KeyboardInterrupt:
                print("\n❌ Cancelled")
                return
        
        if not lines:
            print("❌ No code entered!")
            return
            
        code = '\n'.join(lines)
        temp_file = self.create_temp_file(code)
        program_name = Path(temp_file).stem
        
        print(f"\n💾 Created temporary file: {temp_file}")
        print("🔧 Compiling your program...")
        
        self.run_riscv_program(f"/workspace/examples/{Path(temp_file).name}", program_name)
        
        # Clean up
        try:
            os.unlink(temp_file)
        except:
            pass

    def analyze_existing_file(self):
        """Analyze an existing .s file"""
        print("\n🔍 Analyze Existing Assembly File")
        print("=" * 40)
        
        # List available .s files
        s_files = list(self.examples_dir.glob("*.s"))
        if not s_files:
            print("❌ No .s files found in examples directory!")
            return
            
        print("Available .s files:")
        for i, file_path in enumerate(s_files, 1):
            print(f"{i}. {file_path.name}")
        
        try:
            choice = int(input(f"\nSelect a file (1-{len(s_files)}): "))
            if 1 <= choice <= len(s_files):
                selected_file = s_files[choice - 1]
                
                # Show file content
                print(f"\n📄 Content of {selected_file.name}:")
                print("-" * 40)
                with open(selected_file, 'r') as f:
                    content = f.read()
                    print(content)
                
                # Ask if they want to run it
                run_it = input("\n🚀 Run this program? (y/N): ").lower().strip()
                if run_it == 'y':
                    self.run_riscv_program(f"/workspace/examples/{selected_file.name}", selected_file.stem)
            else:
                print("❌ Invalid choice!")
        except ValueError:
            print("❌ Please enter a valid number!")

    def run_sample_program(self):
        """Run a sample program"""
        program_key = self.get_program_choice()
        if not program_key:
            return
            
        program_info = self.sample_programs[program_key]
        print(f"\n📖 Selected: {program_info['name']}")
        print(f"📝 {program_info['description']}")
        
        # Show code
        show_code = input("\n👁️  Show source code first? (y/N): ").lower().strip()
        if show_code == 'y':
            print(f"\n📄 Source Code:")
            print("-" * 50)
            print(program_info['code'])
            print("-" * 50)
        
        # Create temp file and run
        temp_file = self.create_temp_file(program_info['code'])
        program_name = program_key
        
        print(f"\n🚀 Running {program_info['name']}...")
        self.run_riscv_program(f"/workspace/examples/{Path(temp_file).name}", program_name)
        
        # Clean up
        try:
            os.unlink(temp_file)
        except:
            pass

    def run(self):
        """Main tutorial loop"""
        self.print_banner()
        
        while True:
            self.print_menu()
            
            try:
                choice = input("Enter your choice (0-5): ").strip()
                
                if choice == "0":
                    print("\n👋 Thanks for using the RISC-V Tutorial!")
                    print("Happy coding! 🚀")
                    break
                elif choice == "1":
                    self.run_sample_program()
                elif choice == "2":
                    self.create_custom_program()
                elif choice == "3":
                    self.analyze_existing_file()
                elif choice == "4":
                    self.show_riscv_basics()
                elif choice == "5":
                    self.check_requirements()
                else:
                    print("❌ Invalid choice! Please enter 0-5.")
                    
            except KeyboardInterrupt:
                print("\n\n👋 Goodbye!")
                break
            except Exception as e:
                print(f"❌ An error occurred: {e}")
                
            input("\n📱 Press Enter to continue...")


def main():
    """Entry point"""
    if len(sys.argv) > 1 and sys.argv[1] == "--help":
        print(__doc__)
        return
        
    tutorial = RISCVTutorial()
    tutorial.run()


if __name__ == "__main__":
    main()