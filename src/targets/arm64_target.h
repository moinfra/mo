#pragma once

#include "../machine.h"
#include <memory>

namespace ARM64 {

// ARM64 ABI versions
enum class ABIVersion {
    AAPCS64,    // ARM Architecture Procedure Call Standard (64-bit)
    DARWIN      // Darwin/macOS calling convention
};

//===----------------------------------------------------------------------===//
// ARM64 Register Definitions
//===----------------------------------------------------------------------===//

enum RegisterID : unsigned {
    // General Purpose Registers (64-bit X registers)
    X0 = 0,   X1,   X2,   X3,   X4,   X5,   X6,   X7,
    X8,   X9,   X10,  X11,  X12,  X13,  X14,  X15,
    X16,  X17,  X18,  X19,  X20,  X21,  X22,  X23,
    X24,  X25,  X26,  X27,  X28,  X29,  X30,  XZR,
    
    // 32-bit W registers (lower 32 bits of X registers)
    W0 = 32,  W1,   W2,   W3,   W4,   W5,   W6,   W7,
    W8,   W9,   W10,  W11,  W12,  W13,  W14,  W15,
    W16,  W17,  W18,  W19,  W20,  W21,  W22,  W23,
    W24,  W25,  W26,  W27,  W28,  W29,  W30,  WZR,
    
    // Special registers
    SP = 64,    // Stack Pointer
    FP = X29,   // Frame Pointer (alias for X29)
    LR = X30,   // Link Register (alias for X30)
    
    // SIMD/FP registers
    V0 = 65,  V1,   V2,   V3,   V4,   V5,   V6,   V7,
    V8,   V9,   V10,  V11,  V12,  V13,  V14,  V15,
    V16,  V17,  V18,  V19,  V20,  V21,  V22,  V23,
    V24,  V25,  V26,  V27,  V28,  V29,  V30,  V31
};

//===----------------------------------------------------------------------===//
// ARM64 Instruction Opcodes
//===----------------------------------------------------------------------===//

enum Opcode : unsigned {
    // Arithmetic Instructions
    ADD = 0x1000,    // Add (register)
    ADDS,            // Add and set flags
    SUB,             // Subtract (register)  
    SUBS,            // Subtract and set flags
    MUL,             // Multiply
    UDIV,            // Unsigned divide
    SDIV,            // Signed divide
    
    // Immediate Instructions
    ADD_IMM,         // Add immediate
    SUB_IMM,         // Subtract immediate
    MOV_IMM,         // Move immediate
    MOVZ,            // Move with zero
    MOVK,            // Move with keep
    
    // Logical Instructions
    AND,             // Bitwise AND
    ORR,             // Bitwise OR
    EOR,             // Bitwise XOR
    LSL,             // Logical shift left
    LSR,             // Logical shift right
    ASR,             // Arithmetic shift right
    
    // Memory Instructions
    LDR,             // Load register
    LDRB,            // Load register byte
    LDRH,            // Load register halfword
    STR,             // Store register
    STRB,            // Store register byte
    STRH,            // Store register halfword
    LDP,             // Load pair
    STP,             // Store pair
    
    // Branch Instructions
    B,               // Unconditional branch
    BL,              // Branch with link
    BR,              // Branch to register
    BLR,             // Branch with link to register
    RET,             // Return
    
    // Conditional Branch Instructions
    BEQ,             // Branch if equal
    BNE,             // Branch if not equal
    BLT,             // Branch if less than
    BGE,             // Branch if greater or equal
    BGT,             // Branch if greater than
    BLE,             // Branch if less or equal
    
    // Compare Instructions
    CMP,             // Compare (register)
    CMP_IMM,         // Compare immediate
    
    // System Instructions
    NOP,             // No operation
    COMMENT          // Comment pseudo-instruction
};

//===----------------------------------------------------------------------===//
// ARM64 Target Classes
//===----------------------------------------------------------------------===//

class ARM64RegisterInfo : public TargetRegisterInfo {
private:
    ABIVersion abi_version_;

public:
    explicit ARM64RegisterInfo(ABIVersion abi = ABIVersion::DARWIN);
    
    // Core interface
    const char* getRegisterName(unsigned reg) const;
    std::vector<unsigned> get_allocatable_regs(unsigned reg_class_id) const override;
    std::vector<unsigned> get_allocation_order(unsigned reg_class_id) const override;
    std::vector<unsigned> get_temp_regs(CallingConv::ID cc, unsigned reg_class_id) const override;
    
    // ARM64-specific methods
    ABIVersion getABIVersion() const { return abi_version_; }
    bool isGPR(unsigned reg) const;
    bool isX64Register(unsigned reg) const;
    bool isW32Register(unsigned reg) const;
    bool isSIMDRegister(unsigned reg) const;
    
    // Register class definitions
    enum RegisterClass {
        GPR32 = 0,      // 32-bit general purpose registers (W0-W30)
        GPR64 = 1,      // 64-bit general purpose registers (X0-X30)
        SIMD = 2        // SIMD/FP registers (V0-V31)
    };
};

class ARM64TargetInstInfo : public TargetInstInfo {
private:
    ABIVersion abi_version_;

public:
    explicit ARM64TargetInstInfo(ABIVersion abi = ABIVersion::DARWIN);
    
    // Core interface
    const char* opcode_name(unsigned opcode) const override;
    unsigned get_inst_size(const MachineInst& mi) const override;
    bool verify_instruction(const MachineInst& mi, std::string& error_msg) const override;
    
    // Pure virtual methods from TargetInstInfo (stub implementations)
    uint32_t get_binary_encoding(const MachineInst &mi) const override;
    void expand_pseudo(MachineBasicBlock &mbb, MachineBasicBlock::iterator mi) const override;
    bool is_return(const MachineInst &mi) const override;
    bool is_call(const MachineInst &mi) const override;
    bool is_legal_immediate(int64_t immediate, unsigned operand_size) const override;
    bool is_operand_def(unsigned op, unsigned index) const override;
    bool is_operand_use(unsigned op, unsigned index) const override;
    unsigned get_instruction_latency(unsigned opcode) const override;
    void copy_phys_reg(MachineBasicBlock &mbb, MachineBasicBlock::iterator insert,
                       unsigned dest_reg, unsigned src_reg) const override;
    bool legalize_inst(MachineBasicBlock &mbb, MachineBasicBlock::iterator mii,
                       MachineFunction &mf) const override;
    MachineBasicBlock::iterator insert_load_from_stack(MachineBasicBlock &mbb,
                                                        MachineBasicBlock::iterator insert_point,
                                                        unsigned dest_reg, int frame_index,
                                                        int64_t offset = 0) const override;
    MachineBasicBlock::iterator insert_store_to_stack(MachineBasicBlock &mbb,
                                                       MachineBasicBlock::iterator insert_point,
                                                       unsigned src_reg, int frame_index,
                                                       int64_t offset = 0) const override;
    bool analyze_branch(MachineBasicBlock &mbb, MachineInst *terminator,
                        std::unordered_set<MachineBasicBlock *> &branch_targets,
                        MachineBasicBlock *&fall_through) const override;
    
    // ARM64-specific methods
    ABIVersion getABIVersion() const { return abi_version_; }
};

class ARM64Target {
private:
    std::unique_ptr<ARM64RegisterInfo> reg_info_;
    std::unique_ptr<ARM64TargetInstInfo> inst_info_;
    ABIVersion abi_version_;

public:
    explicit ARM64Target(
        std::unique_ptr<ARM64RegisterInfo> reg_info,
        std::unique_ptr<ARM64TargetInstInfo> inst_info,
        ABIVersion abi = ABIVersion::DARWIN
    );
    
    const ARM64RegisterInfo* getRegisterInfo() const { return reg_info_.get(); }
    const ARM64TargetInstInfo* getInstInfo() const { return inst_info_.get(); }
    ABIVersion getABIVersion() const { return abi_version_; }
    const char* getTargetName() const;
};

} // namespace ARM64