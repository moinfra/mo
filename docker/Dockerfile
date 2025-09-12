# RISC-V Development Environment with QEMU
FROM ubuntu:22.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

# Install base dependencies
RUN apt-get update && apt-get install -y \
    git wget curl \
    build-essential ninja-build python3-setuptools \
    libglib2.0-dev libpixman-1-dev \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Install RISC-V cross-compilation toolchain
RUN apt-get update && apt-get install -y \
    gcc-riscv64-linux-gnu \
    qemu-system-riscv64 \
    && rm -rf /var/lib/apt/lists/*

# Create working directory
WORKDIR /workspace

# Copy Mo compiler output
COPY examples/ /workspace/examples/
COPY scripts/ /workspace/scripts/

# Set up entry point
CMD ["/bin/bash"]