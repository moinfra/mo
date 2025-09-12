#!/bin/bash

# Script to compile and run RISC-V assembly programs

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <assembly_file.s>"
    exit 1
fi

ASM_FILE="$1"
BASE_NAME=$(basename "$ASM_FILE" .s)
OUTPUT_DIR="/workspace/output"

echo "Compiling RISC-V assembly: $ASM_FILE"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Compile the assembly file to object file
echo "Step 1: Assembling..."
riscv64-linux-gnu-as -march=rv64gc -mabi=lp64d "$ASM_FILE" -o "$OUTPUT_DIR/${BASE_NAME}.o"

# Create a simple linker script for bare metal execution
cat > "$OUTPUT_DIR/linker.ld" << 'EOF'
OUTPUT_ARCH(riscv)
ENTRY(_start)

MEMORY
{
    ram (rwx): ORIGIN = 0x80000000, LENGTH = 64M
}

SECTIONS
{
    .text : {
        KEEP(*(.text.init))
        *(.text)
    } > ram
    
    .data : {
        *(.data)
    } > ram
    
    .bss : {
        *(.bss)
    } > ram
}
EOF

# Create a startup file that calls main
cat > "$OUTPUT_DIR/start.s" << 'EOF'
.section .text.init
.globl _start
_start:
    # Set up stack pointer (use end of 64MB RAM)
    li sp, 0x84000000
    
    # Call main function
    jal ra, main
    
    # Exit with return value in a0
    # For bare metal, we'll just loop
_exit:
    # QEMU exit: write exit code to 0x100000
    li t0, 0x100000
    sw a0, 0(t0)
    # Infinite loop
1:  j 1b
EOF

# Assemble startup code
echo "Step 2: Assembling startup code..."
riscv64-linux-gnu-as -march=rv64gc -mabi=lp64d "$OUTPUT_DIR/start.s" -o "$OUTPUT_DIR/start.o"

# Link everything together
echo "Step 3: Linking..."
riscv64-linux-gnu-ld -T "$OUTPUT_DIR/linker.ld" "$OUTPUT_DIR/start.o" "$OUTPUT_DIR/${BASE_NAME}.o" -o "$OUTPUT_DIR/${BASE_NAME}.elf"

echo "Step 4: Creating binary..."
riscv64-linux-gnu-objcopy -O binary "$OUTPUT_DIR/${BASE_NAME}.elf" "$OUTPUT_DIR/${BASE_NAME}.bin"

# Display info about the generated executable
echo "Step 5: Analyzing binary..."
riscv64-linux-gnu-objdump -d "$OUTPUT_DIR/${BASE_NAME}.elf" > "$OUTPUT_DIR/${BASE_NAME}.disasm"
riscv64-linux-gnu-readelf -h "$OUTPUT_DIR/${BASE_NAME}.elf" > "$OUTPUT_DIR/${BASE_NAME}.info"

echo "Compilation complete!"
echo "Files generated:"
echo "  - $OUTPUT_DIR/${BASE_NAME}.elf (executable)"
echo "  - $OUTPUT_DIR/${BASE_NAME}.bin (binary)"
echo "  - $OUTPUT_DIR/${BASE_NAME}.disasm (disassembly)"

echo ""
echo "To run in QEMU:"
echo "  qemu-system-riscv64 -machine virt -cpu rv64 -m 128M -nographic -bios none -kernel $OUTPUT_DIR/${BASE_NAME}.elf"

# Optionally run it if QEMU is available and user wants to
read -p "Run in QEMU now? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Running in QEMU (Ctrl+A, X to exit)..."
    echo "Expected output: program should return 8 (5+3)"
    timeout 10s qemu-system-riscv64 \
        -machine virt \
        -cpu rv64 \
        -m 128M \
        -nographic \
        -bios none \
        -kernel "$OUTPUT_DIR/${BASE_NAME}.elf" \
        -serial stdio \
        -monitor none \
        || echo "QEMU execution finished or timed out"
fi