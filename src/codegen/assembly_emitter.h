// assembly_emitter.h - Abstract interface for assembly emission
#pragma once

#include "../machine.h"
#include <memory>
#include <string>
#include <map>
#include <vector>

// Forward declarations
class MachineModule;
class TargetRegisterInfo;
class TargetInstInfo;

namespace codegen {

/// AssemblyEmitter - Abstract base class for target-specific assembly emission
/// 
/// This class defines the interface that all assembly emitters must implement.
/// It converts machine-level code into target-specific assembly text.
class AssemblyEmitter {
public:
    virtual ~AssemblyEmitter() = default;

    /// Emit assembly code for a machine module
    /// @param module The machine module to emit assembly for
    /// @return String containing the complete assembly output
    virtual std::string emit(const MachineModule& module) = 0;
    
    /// Get the target name for this emitter
    /// @return String identifying the target (e.g., "riscv64", "x86_64")
    virtual const char* getTargetName() const = 0;
    
    /// Check if emitter supports a specific feature
    /// @param feature Feature name to check
    /// @return True if feature is supported
    virtual bool supportsFeature(const std::string& feature) const = 0;
    
    /// Get estimated size of assembly output for profiling
    /// @param module The machine module to estimate
    /// @return Estimated size in bytes (arbitrary units)
    virtual size_t estimateOutputSize(const MachineModule& module) const {
        // Default implementation: rough estimate based on function count
        return module.functions().size() * 100;  // 100 bytes per function
    }

protected:
    /// Default constructor for derived classes
    AssemblyEmitter() = default;
    
    /// Non-copyable
    AssemblyEmitter(const AssemblyEmitter&) = delete;
    AssemblyEmitter& operator=(const AssemblyEmitter&) = delete;
    
    /// Moveable
    AssemblyEmitter(AssemblyEmitter&&) = default;
    AssemblyEmitter& operator=(AssemblyEmitter&&) = default;
};

/// AssemblyEmitterRegistry - Registry for available assembly emitters
/// 
/// Provides a registry system for managing different target assembly emitters,
/// similar to the InstructionSelectorRegistry pattern.
class AssemblyEmitterRegistry {
public:
    /// Factory function type for creating assembly emitter instances
    using CreateFunction = std::unique_ptr<AssemblyEmitter>(*)(
        const TargetRegisterInfo* reg_info,
        const TargetInstInfo* inst_info);
    
    /// Register an assembly emitter with the registry
    /// @param target_name Target name (e.g., "riscv32", "riscv64")
    /// @param create_fn Factory function to create emitter instance
    static void registerEmitter(const std::string& target_name, CreateFunction create_fn);
    
    /// Create assembly emitter instance by target name
    /// @param target_name Target name
    /// @param reg_info Target register information
    /// @param inst_info Target instruction information
    /// @return Unique pointer to emitter instance, or nullptr if not found
    static std::unique_ptr<AssemblyEmitter> createEmitter(
        const std::string& target_name,
        const TargetRegisterInfo* reg_info,
        const TargetInstInfo* inst_info);
    
    /// Get list of available target names
    /// @return Vector of registered target names
    static std::vector<std::string> getAvailableTargets();

private:
    static std::map<std::string, CreateFunction>& getRegistry();
};

/// Helper macro for assembly emitter registration
#define REGISTER_ASSEMBLY_EMITTER(target_name, class_name) \
    namespace { \
        std::unique_ptr<codegen::AssemblyEmitter> create##class_name( \
            const TargetRegisterInfo* reg_info, \
            const TargetInstInfo* inst_info) { \
            return std::make_unique<class_name>(reg_info, inst_info); \
        } \
        struct class_name##AssemblyEmitterRegistrar { \
            class_name##AssemblyEmitterRegistrar() { \
                codegen::AssemblyEmitterRegistry::registerEmitter( \
                    target_name, create##class_name); \
            } \
        }; \
        static class_name##AssemblyEmitterRegistrar g_##class_name##_assembly_emitter_registrar; \
    }

} // namespace codegen