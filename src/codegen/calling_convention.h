#pragma once

#include "../machine.h"
#include "../ir.h"
#include <vector>
#include <memory>

enum class CallingConventionID {
    SystemV,        // System V ABI (Linux x86_64, etc.)
    AAPCS64,        // ARM64 Procedure Call Standard
    Win64,          // Windows x64 calling convention
    RISCV,          // RISC-V calling convention
    Custom          // User-defined calling convention
};

// Represents where an argument or return value should be placed
struct ArgumentLocation {
    enum class Type {
        Register,   // In a register
        Stack,      // On the stack
        Memory      // Indirect (pointer passed)
    };
    
    Type type;
    unsigned reg_id = 0;        // Register ID if type == Register
    int stack_offset = 0;       // Stack offset if type == Stack
    unsigned size = 0;          // Size in bytes
};

// Abstract interface for calling conventions
class CallingConvention {
public:
    virtual ~CallingConvention() = default;
    
    // Get calling convention ID
    virtual CallingConventionID getId() const = 0;
    
    // Analyze argument passing for a function call
    virtual std::vector<ArgumentLocation> getArgumentLocations(
        const std::vector<Type*>& arg_types) const = 0;
    
    // Get return value location
    virtual ArgumentLocation getReturnLocation(Type* return_type) const = 0;
    
    // Get caller-saved registers (may be clobbered by calls)
    virtual std::vector<unsigned> getCallerSavedRegisters() const = 0;
    
    // Get callee-saved registers (must be preserved by callee)
    virtual std::vector<unsigned> getCalleeSavedRegisters() const = 0;
    
    // Get stack alignment requirement
    virtual unsigned getStackAlignment() const = 0;
    
    // Create the appropriate calling convention for a target
    static std::unique_ptr<CallingConvention> create(
        CallingConventionID id, 
        const TargetRegisterInfo* target_reg_info);
};

// Factory for creating calling conventions
class CallingConventionFactory {
public:
    static std::unique_ptr<CallingConvention> createForTarget(
        const std::string& target_name);
    
    static std::unique_ptr<CallingConvention> createById(
        CallingConventionID id,
        const TargetRegisterInfo* target_reg_info);
};