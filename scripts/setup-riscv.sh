#!/bin/bash

# Setup script for RISC-V development environment in Docker

echo "Setting up RISC-V development environment..."

# Create working directories
mkdir -p /workspace/output
mkdir -p /workspace/temp

# Install additional tools if needed
apt-get update
apt-get install -y \
    binutils-riscv64-linux-gnu \
    gdb-multiarch \
    opensbi \
    u-boot-qemu

echo "RISC-V environment setup complete!"
echo "Available tools:"
echo "- QEMU: $(qemu-system-riscv64 --version | head -1)"
echo "- GCC: $(riscv64-linux-gnu-gcc --version | head -1)"
echo "- Assembler: $(riscv64-linux-gnu-as --version | head -1)"

# Download a minimal Linux environment for testing (optional)
if [ ! -f "/workspace/temp/fw_jump.elf" ]; then
    echo "Downloading OpenSBI firmware..."
    cp /usr/lib/riscv64-linux-gnu/opensbi/generic/fw_jump.elf /workspace/temp/ 2>/dev/null || echo "OpenSBI firmware not available, will use built-in"
fi

echo "Setup complete! You can now compile and run RISC-V programs."