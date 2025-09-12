#include "calling_convention.h"
#include "../targets/arm64_target.h"
#include "../targets/riscv_target.h"

// ARM64 AAPCS64 Calling Convention
class AAPCS64CallingConvention : public CallingConvention {
private:
    const TargetRegisterInfo* target_reg_info_;
    
public:
    explicit AAPCS64CallingConvention(const TargetRegisterInfo* tri) 
        : target_reg_info_(tri) {}
    
    CallingConventionID getId() const override {
        return CallingConventionID::AAPCS64;
    }
    
    std::vector<ArgumentLocation> getArgumentLocations(
        const std::vector<Type*>& arg_types) const override {
        
        std::vector<ArgumentLocation> locations;
        unsigned gpr_count = 0;  // General purpose register count
        unsigned stack_offset = 0;
        
        // ARM64 AAPCS64: First 8 integer args in X0-X7, rest on stack
        for (size_t i = 0; i < arg_types.size(); ++i) {
            ArgumentLocation loc;
            loc.size = 8; // Assume 64-bit for now
            
            if (gpr_count < 8 && isIntegerType(arg_types[i])) {
                loc.type = ArgumentLocation::Type::Register;
                loc.reg_id = ARM64::X0 + gpr_count;
                gpr_count++;
            } else {
                loc.type = ArgumentLocation::Type::Stack;
                loc.stack_offset = stack_offset;
                stack_offset += 8; // Align to 8 bytes
            }
            
            locations.push_back(loc);
        }
        
        return locations;
    }
    
    ArgumentLocation getReturnLocation(Type* return_type) const override {
        ArgumentLocation loc;
        loc.type = ArgumentLocation::Type::Register;
        loc.reg_id = ARM64::X0;  // Return in X0
        loc.size = 8;
        return loc;
    }
    
    std::vector<unsigned> getCallerSavedRegisters() const override {
        // ARM64 caller-saved: X0-X18, X30 (LR)
        return {ARM64::X0, ARM64::X1, ARM64::X2, ARM64::X3, ARM64::X4,
                ARM64::X5, ARM64::X6, ARM64::X7, ARM64::X8, ARM64::X9,
                ARM64::X10, ARM64::X11, ARM64::X12, ARM64::X13, ARM64::X14,
                ARM64::X15, ARM64::X16, ARM64::X17, ARM64::X18, ARM64::X30};
    }
    
    std::vector<unsigned> getCalleeSavedRegisters() const override {
        // ARM64 callee-saved: X19-X29
        return {ARM64::X19, ARM64::X20, ARM64::X21, ARM64::X22, ARM64::X23,
                ARM64::X24, ARM64::X25, ARM64::X26, ARM64::X27, ARM64::X28, ARM64::X29};
    }
    
    unsigned getStackAlignment() const override {
        return 16; // ARM64 requires 16-byte stack alignment
    }

private:
    bool isIntegerType(Type* type) const {
        // Simple check - in real implementation, would be more sophisticated
        return type && !type->is_void();
    }
};

// RISC-V Calling Convention
class RISCVCallingConvention : public CallingConvention {
private:
    const TargetRegisterInfo* target_reg_info_;
    
public:
    explicit RISCVCallingConvention(const TargetRegisterInfo* tri) 
        : target_reg_info_(tri) {}
    
    CallingConventionID getId() const override {
        return CallingConventionID::RISCV;
    }
    
    std::vector<ArgumentLocation> getArgumentLocations(
        const std::vector<Type*>& arg_types) const override {
        
        std::vector<ArgumentLocation> locations;
        unsigned gpr_count = 0;
        unsigned stack_offset = 0;
        
        // RISC-V: First 8 integer args in a0-a7, rest on stack
        for (size_t i = 0; i < arg_types.size(); ++i) {
            ArgumentLocation loc;
            loc.size = 8; // Assume 64-bit
            
            if (gpr_count < 8 && isIntegerType(arg_types[i])) {
                loc.type = ArgumentLocation::Type::Register;
                loc.reg_id = 10 + gpr_count; // a0 = x10, a1 = x11, etc.
                gpr_count++;
            } else {
                loc.type = ArgumentLocation::Type::Stack;
                loc.stack_offset = stack_offset;
                stack_offset += 8;
            }
            
            locations.push_back(loc);
        }
        
        return locations;
    }
    
    ArgumentLocation getReturnLocation(Type* return_type) const override {
        ArgumentLocation loc;
        loc.type = ArgumentLocation::Type::Register;
        loc.reg_id = 10; // a0 register
        loc.size = 8;
        return loc;
    }
    
    std::vector<unsigned> getCallerSavedRegisters() const override {
        return {1, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 28, 29, 30, 31}; // RISC-V caller-saved
    }
    
    std::vector<unsigned> getCalleeSavedRegisters() const override {
        return {2, 8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27}; // RISC-V callee-saved
    }
    
    unsigned getStackAlignment() const override {
        return 16; // RISC-V stack alignment
    }

private:
    bool isIntegerType(Type* type) const {
        return type && !type->is_void();
    }
};

// Factory implementations
std::unique_ptr<CallingConvention> CallingConvention::create(
    CallingConventionID id, 
    const TargetRegisterInfo* target_reg_info) {
    
    switch (id) {
        case CallingConventionID::AAPCS64:
            return std::make_unique<AAPCS64CallingConvention>(target_reg_info);
        case CallingConventionID::RISCV:
            return std::make_unique<RISCVCallingConvention>(target_reg_info);
        default:
            return nullptr;
    }
}

std::unique_ptr<CallingConvention> CallingConventionFactory::createForTarget(
    const std::string& target_name) {
    
    if (target_name == "arm64" || target_name == "aarch64") {
        return CallingConvention::create(CallingConventionID::AAPCS64, nullptr);
    } else if (target_name == "riscv64" || target_name == "riscv") {
        return CallingConvention::create(CallingConventionID::RISCV, nullptr);
    }
    
    return nullptr;
}

std::unique_ptr<CallingConvention> CallingConventionFactory::createById(
    CallingConventionID id,
    const TargetRegisterInfo* target_reg_info) {
    
    return CallingConvention::create(id, target_reg_info);
}