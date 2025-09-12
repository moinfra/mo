#!/usr/bin/env python3
"""
RISC-V Assembly Executor - Interactive Tool
==========================================

A generic interactive tool for compiling and running arbitrary RISC-V assembly programs
using Docker and QEMU emulation.

Author: Mo Compiler Project
"""

import os
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import List, Optional


class RISCVExecutor:
    def __init__(self):
        self.docker_image = "davidburela/riscv-emulator"
        self.work_dir = Path.cwd()
        self.scripts_dir = Path(__file__).parent
        self.output_dir = self.work_dir / "riscv_output"
        
        # Ensure output directory exists
        self.output_dir.mkdir(exist_ok=True)

    def print_banner(self):
        """Print tool banner"""
        print("=" * 60)
        print("🔧 RISC-V Assembly Executor")
        print("=" * 60)
        print("Execute any RISC-V assembly (.s) file with:")
        print("• Docker-based cross-compilation")
        print("• QEMU emulation")
        print("• Automatic toolchain setup")
        print("• Interactive file selection")
        print("=" * 60)
        print()

    def print_menu(self):
        """Print main menu"""
        print("\n📋 What would you like to do?")
        print("1. Execute a .s file from current directory")
        print("2. Execute a .s file from any path")
        print("3. Create and execute a new program")
        print("4. Check system requirements")
        print("5. Show RISC-V quick reference")
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
                print("Will be downloaded automatically when needed")
        
        print(f"\n📁 Working directory: {self.work_dir}")
        print(f"📁 Output directory: {self.output_dir}")

    def find_s_files(self, directory: Path) -> List[Path]:
        """Find all .s files in directory"""
        return list(directory.glob("*.s"))

    def select_file_from_current_dir(self) -> Optional[Path]:
        """Select a .s file from current directory"""
        s_files = self.find_s_files(self.work_dir)
        
        if not s_files:
            print("❌ No .s files found in current directory!")
            print(f"📁 Current directory: {self.work_dir}")
            return None
        
        print(f"\n📁 Found .s files in {self.work_dir}:")
        print("-" * 40)
        for i, file_path in enumerate(s_files, 1):
            # Show file size and modification time
            stat = file_path.stat()
            size = stat.st_size
            mtime = time.strftime("%Y-%m-%d %H:%M", time.localtime(stat.st_mtime))
            print(f"{i}. {file_path.name} ({size} bytes, {mtime})")
        
        try:
            choice = int(input(f"\nSelect a file (1-{len(s_files)}): "))
            if 1 <= choice <= len(s_files):
                return s_files[choice - 1]
            else:
                print("❌ Invalid choice!")
                return None
        except ValueError:
            print("❌ Please enter a valid number!")
            return None

    def select_file_from_path(self) -> Optional[Path]:
        """Select a .s file from any path"""
        while True:
            file_path = input("\n📁 Enter path to .s file (or 'back' to return): ").strip()
            
            if file_path.lower() == 'back':
                return None
                
            path = Path(file_path).expanduser()
            
            if not path.exists():
                print("❌ File does not exist!")
                continue
                
            if not path.is_file():
                print("❌ Path is not a file!")
                continue
                
            if path.suffix.lower() != '.s':
                print("❌ File must have .s extension!")
                continue
                
            return path

    def show_file_preview(self, file_path: Path, max_lines: int = 20):
        """Show preview of the file content"""
        try:
            with open(file_path, 'r') as f:
                lines = f.readlines()
            
            print(f"\n📄 Preview of {file_path.name} ({len(lines)} lines):")
            print("-" * 50)
            
            # Show first max_lines lines
            for i, line in enumerate(lines[:max_lines], 1):
                print(f"{i:3}: {line.rstrip()}")
            
            if len(lines) > max_lines:
                print(f"... ({len(lines) - max_lines} more lines)")
            
            print("-" * 50)
            
        except Exception as e:
            print(f"❌ Could not read file: {e}")

    def execute_riscv_file(self, file_path: Path):
        """Compile and execute a RISC-V assembly file"""
        print(f"\n🔧 Executing: {file_path.name}")
        print("=" * 50)
        
        # Show file preview
        show_preview = input("👁️  Show file preview? (y/N): ").lower().strip() == 'y'
        if show_preview:
            self.show_file_preview(file_path)
        
        # Confirm execution
        execute = input(f"🚀 Execute {file_path.name}? (Y/n): ").lower().strip()
        if execute == 'n':
            print("❌ Execution cancelled")
            return
        
        print(f"\n⚙️  Setting up execution environment...")
        
        # Copy file to a temp location that Docker can access
        temp_name = f"temp_{file_path.stem}_{int(time.time())}.s"
        temp_file = self.output_dir / temp_name
        
        try:
            # Copy the file
            with open(file_path, 'r') as src, open(temp_file, 'w') as dst:
                dst.write(src.read())
            
            # Build Docker command with platform specification
            docker_cmd = [
                "docker", "run", "--rm",
                "--platform", "linux/amd64",  # Explicitly specify platform
                "-v", f"{self.output_dir}:/workspace",
                self.docker_image,
                "/bin/bash", "-c",
                f"""
                apt-get update -qq &&
                apt-get install -y -qq gcc-riscv64-linux-gnu binutils-riscv64-linux-gnu &&
                echo '✅ RISC-V toolchain ready!' &&
                echo '🔨 Compiling {temp_name}...' &&
                cd /workspace &&
                
                # Assemble
                riscv64-linux-gnu-as -march=rv64gc -mabi=lp64d {temp_name} -o {file_path.stem}.o &&
                
                # Create simple startup
                echo '.section .text.init' > start.s &&
                echo '.globl _start' >> start.s &&
                echo '_start:' >> start.s &&
                echo '    li sp, 0x84000000' >> start.s &&
                echo '    jal ra, main' >> start.s &&
                echo '    li t0, 0x100000' >> start.s &&
                echo '    sw a0, 0(t0)' >> start.s &&
                echo '1:  j 1b' >> start.s &&
                
                # Assemble startup
                riscv64-linux-gnu-as -march=rv64gc -mabi=lp64d start.s -o start.o &&
                
                # Create linker script
                cat > linker.ld << 'EOF'
OUTPUT_ARCH(riscv)
ENTRY(_start)
MEMORY {{
    ram (rwx): ORIGIN = 0x80000000, LENGTH = 64M
}}
SECTIONS {{
    .text : {{ KEEP(*(.text.init)) *(.text) }} > ram
    .data : {{ *(.data) }} > ram
    .bss : {{ *(.bss) }} > ram
}}
EOF
                
                # Link
                riscv64-linux-gnu-ld -T linker.ld start.o {file_path.stem}.o -o {file_path.stem}.elf &&
                
                # Create binary  
                riscv64-linux-gnu-objcopy -O binary {file_path.stem}.elf {file_path.stem}.bin &&
                
                # Show info
                echo '✅ Compilation successful!' &&
                echo '📊 File info:' &&
                ls -la {file_path.stem}.* &&
                
                # Disassemble for analysis
                riscv64-linux-gnu-objdump -d {file_path.stem}.elf > {file_path.stem}.disasm &&
                
                echo '🚀 Running in QEMU...' &&
                timeout 3s qemu-system-riscv64 -machine virt -cpu rv64 -m 128M -nographic -bios none -kernel {file_path.stem}.elf -serial stdio -monitor none || echo '✅ QEMU execution completed'
                """
            ]
            
            print("🐳 Starting Docker environment...")
            print("📦 Installing RISC-V toolchain...")
            print("🔨 Compiling program...")
            print("🚀 Running in QEMU...")
            print()
            
            # Execute
            result = subprocess.run(
                docker_cmd,
                text=True,
                timeout=120
            )
            
            if result.returncode == 0:
                print("\n✅ Execution completed successfully!")
            else:
                print(f"\n⚠️  Process finished with return code: {result.returncode}")
            
            # Show generated files
            self.show_output_files(file_path.stem)
            
        except subprocess.TimeoutExpired:
            print("❌ Execution timed out!")
        except Exception as e:
            print(f"❌ Execution failed: {e}")
        finally:
            # Clean up temp file
            if temp_file.exists():
                temp_file.unlink()

    def show_output_files(self, program_name: str):
        """Show information about generated output files"""
        print(f"\n📁 Generated files in {self.output_dir}:")
        print("-" * 40)
        
        extensions = ['.o', '.elf', '.bin', '.disasm']
        for ext in extensions:
            file_path = self.output_dir / f"{program_name}{ext}"
            if file_path.exists():
                size = file_path.stat().st_size
                description = {
                    '.o': 'Object file',
                    '.elf': 'Executable (ELF format)',  
                    '.bin': 'Raw binary',
                    '.disasm': 'Disassembly listing'
                }.get(ext, 'Unknown')
                print(f"  {file_path.name} ({size} bytes) - {description}")

    def create_new_program(self):
        """Interactive program creation"""
        print("\n✏️  Create New RISC-V Program")
        print("=" * 40)
        
        # Get filename
        while True:
            filename = input("📝 Enter filename (without .s extension): ").strip()
            if not filename:
                print("❌ Filename cannot be empty!")
                continue
            if '.' in filename or '/' in filename or '\\' in filename:
                print("❌ Please enter only the base filename!")
                continue
            break
        
        full_path = self.work_dir / f"{filename}.s"
        
        if full_path.exists():
            overwrite = input(f"⚠️  {full_path.name} already exists. Overwrite? (y/N): ").lower().strip()
            if overwrite != 'y':
                print("❌ Creation cancelled")
                return
        
        print(f"\n📝 Enter your RISC-V assembly code for {filename}.s")
        print("Type 'END' on a new line when finished.")
        print("\nExample structure:")
        print("  .text")
        print("  .globl main")
        print("  main:")
        print("      li a0, 42")
        print("      ret")
        print("\nStart typing:")
        
        lines = []
        while True:
            try:
                line = input()
                if line.strip() == "END":
                    break
                lines.append(line)
            except KeyboardInterrupt:
                print("\n❌ Creation cancelled")
                return
        
        if not lines:
            print("❌ No code entered!")
            return
        
        # Write file
        try:
            with open(full_path, 'w') as f:
                f.write('\n'.join(lines) + '\n')
            
            print(f"\n✅ Created {full_path}")
            
            # Execute it
            execute = input("🚀 Execute the new program? (Y/n): ").lower().strip()
            if execute != 'n':
                self.execute_riscv_file(full_path)
                
        except Exception as e:
            print(f"❌ Could not create file: {e}")

    def show_riscv_reference(self):
        """Show RISC-V quick reference"""
        print("\n📖 RISC-V Assembly Quick Reference")
        print("=" * 50)
        print("""
🔹 REGISTERS:
  a0-a7   : Arguments/return values (a0=1st arg & return)
  t0-t6   : Temporaries (caller-saved)
  s0-s11  : Saved registers (callee-saved)  
  ra      : Return address
  sp      : Stack pointer
  zero    : Always zero (x0)

🔹 BASIC INSTRUCTIONS:
  li rd, imm      : Load immediate value
  add rd, rs1, rs2: Add two registers
  addi rd, rs1, imm: Add immediate to register
  sub rd, rs1, rs2: Subtract registers
  mul rd, rs1, rs2: Multiply registers
  
🔹 MEMORY ACCESS:
  lw rd, offset(rs): Load word from memory
  sw rs, offset(rd): Store word to memory
  la rd, label     : Load address of label
  
🔹 CONTROL FLOW:
  jal rd, label   : Jump and link (function call)
  jalr rd, rs, imm: Jump and link register
  ret             : Return (jalr x0, ra, 0)
  beq rs1, rs2, label: Branch if equal
  bne rs1, rs2, label: Branch if not equal
  blt rs1, rs2, label: Branch if less than
  j label         : Unconditional jump
  
🔹 PROGRAM STRUCTURE:
  .text           : Code section
  .data           : Initialized data section  
  .bss            : Uninitialized data section
  .globl label    : Make label globally visible
  label:          : Define a label
        """)

    def run(self):
        """Main executor loop"""
        self.print_banner()
        
        while True:
            self.print_menu()
            
            try:
                choice = input("Enter your choice (0-5): ").strip()
                
                if choice == "0":
                    print("\n👋 Thanks for using RISC-V Executor!")
                    break
                elif choice == "1":
                    file_path = self.select_file_from_current_dir()
                    if file_path:
                        self.execute_riscv_file(file_path)
                elif choice == "2":
                    file_path = self.select_file_from_path()
                    if file_path:
                        self.execute_riscv_file(file_path)
                elif choice == "3":
                    self.create_new_program()
                elif choice == "4":
                    self.check_requirements()
                elif choice == "5":
                    self.show_riscv_reference()
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
    if len(sys.argv) > 1:
        if sys.argv[1] == "--help":
            print(__doc__)
            return
        elif sys.argv[1].endswith('.s'):
            # Direct file execution mode
            file_path = Path(sys.argv[1])
            if file_path.exists():
                executor = RISCVExecutor()
                executor.execute_riscv_file(file_path)
            else:
                print(f"❌ File not found: {file_path}")
            return
    
    # Interactive mode
    executor = RISCVExecutor()
    executor.run()


if __name__ == "__main__":
    main()