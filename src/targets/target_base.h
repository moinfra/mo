// target_base.h - Base class for target-specific code generation
#pragma once

#include <map>
#include <memory>
#include <string>
#include <vector>

// Forward declarations
class Module;
class MachineModule;

namespace codegen {

/// TargetBase - Abstract base class for target-specific code generation
/// 
/// This class defines the interface that all target implementations must provide.
/// It serves as the bridge between target-independent IR and target-specific
/// machine code generation.
class TargetBase {
public:
    virtual ~TargetBase() = default;

    /// Generate machine code from IR module
    /// @param ir_module The input IR module to compile
    /// @return Unique pointer to generated MachineModule, or nullptr on failure
    virtual std::unique_ptr<MachineModule> generateMachineCode(const Module& ir_module) = 0;
    
    /// Emit assembly code from machine module
    /// @param machine_module The input machine module 
    /// @return String containing target-specific assembly code
    virtual std::string emitAssembly(const MachineModule& machine_module) = 0;
    
    /// Get target name for debugging and logging
    /// @return String identifying the target (e.g., "riscv64", "x86_64")
    virtual const char* getTargetName() const = 0;
    
    /// Check if target supports a specific feature
    /// @param feature Feature name to check
    /// @return true if feature is supported
    virtual bool hasFeature(const std::string& feature) const = 0;

protected:
    /// Default constructor for derived classes
    TargetBase() = default;
    
    /// Non-copyable
    TargetBase(const TargetBase&) = delete;
    TargetBase& operator=(const TargetBase&) = delete;
    
    /// Moveable
    TargetBase(TargetBase&&) = default;
    TargetBase& operator=(TargetBase&&) = default;
};

/// Factory function type for creating target instances
using TargetCreateFunction = std::unique_ptr<TargetBase>(*)();

/// Target registry for managing available targets
class TargetRegistry {
public:
    /// Register a target with the registry
    /// @param name Target name (e.g., "riscv32", "riscv64")
    /// @param create_fn Factory function to create target instance
    static void registerTarget(const std::string& name, TargetCreateFunction create_fn);
    
    /// Create target instance by name
    /// @param name Target name
    /// @return Unique pointer to target instance, or nullptr if not found
    static std::unique_ptr<TargetBase> createTarget(const std::string& name);
    
    /// Get list of available target names
    /// @return Vector of registered target names
    static std::vector<std::string> getAvailableTargets();

private:
    static std::map<std::string, TargetCreateFunction>& getRegistry();
};

/// Helper macro for target registration
#define REGISTER_TARGET(name, class_name) \
    namespace { \
        std::unique_ptr<TargetBase> create##class_name() { \
            return std::make_unique<class_name>(); \
        } \
        struct class_name##Registrar { \
            class_name##Registrar() { \
                TargetRegistry::registerTarget(name, create##class_name); \
            } \
        }; \
        static class_name##Registrar g_##class_name##_registrar; \
    }

} // namespace codegen