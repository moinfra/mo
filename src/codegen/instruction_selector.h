// instruction_selector.h - Abstract interface for instruction selection
#pragma once

#include "../ir.h"
#include "../machine.h"
#include <memory>

// Forward declarations
class Function;
class MachineFunction;
class TargetRegisterInfo;
class TargetInstInfo;

namespace codegen {

/// InstructionSelector - Abstract base class for target-specific instruction selection
/// 
/// This class defines the interface that all instruction selectors must implement.
/// It converts IR-level operations into target-specific machine instructions.
class InstructionSelector {
public:
    virtual ~InstructionSelector() = default;

    /// Select instructions for a function
    /// @param ir_func The IR function to convert
    /// @param machine_func The target MachineFunction to populate
    /// @return True if selection succeeded, false on error
    virtual bool selectInstructions(const Function& ir_func, 
                                   MachineFunction& machine_func) = 0;
    
    /// Get the target name for this selector
    /// @return String identifying the target (e.g., "riscv64", "x86_64")
    virtual const char* getTargetName() const = 0;
    
    /// Check if selector supports a specific feature
    /// @param feature Feature name to check
    /// @return True if feature is supported
    virtual bool supportsFeature(const std::string& feature) const = 0;
    
    /// Get estimated cost of instruction selection for profiling
    /// @param ir_func The IR function to estimate
    /// @return Estimated cost (arbitrary units)
    virtual size_t estimateSelectionCost(const Function& ir_func) const {
        // Default implementation: rough estimate based on basic block count
        return ir_func.basic_blocks().size() * 10;  // 10 units per basic block
    }

protected:
    /// Default constructor for derived classes
    InstructionSelector() = default;
    
    /// Non-copyable
    InstructionSelector(const InstructionSelector&) = delete;
    InstructionSelector& operator=(const InstructionSelector&) = delete;
    
    /// Moveable
    InstructionSelector(InstructionSelector&&) = default;
    InstructionSelector& operator=(InstructionSelector&&) = default;
};

/// InstructionSelectorRegistry - Registry for available instruction selectors
/// 
/// Provides a registry system for managing different target instruction selectors,
/// similar to the TargetRegistry pattern.
class InstructionSelectorRegistry {
public:
    /// Factory function type for creating instruction selector instances
    using CreateFunction = std::unique_ptr<InstructionSelector>(*)(
        const TargetRegisterInfo* reg_info,
        const TargetInstInfo* inst_info);
    
    /// Register an instruction selector with the registry
    /// @param target_name Target name (e.g., "riscv32", "riscv64")
    /// @param create_fn Factory function to create selector instance
    static void registerSelector(const std::string& target_name, CreateFunction create_fn);
    
    /// Create instruction selector instance by target name
    /// @param target_name Target name
    /// @param reg_info Target register information
    /// @param inst_info Target instruction information
    /// @return Unique pointer to selector instance, or nullptr if not found
    static std::unique_ptr<InstructionSelector> createSelector(
        const std::string& target_name,
        const TargetRegisterInfo* reg_info,
        const TargetInstInfo* inst_info);
    
    /// Get list of available target names
    /// @return Vector of registered target names
    static std::vector<std::string> getAvailableTargets();

private:
    static std::map<std::string, CreateFunction>& getRegistry();
};

/// Helper macro for instruction selector registration
#define REGISTER_INSTRUCTION_SELECTOR(target_name, class_name) \
    namespace { \
        std::unique_ptr<codegen::InstructionSelector> create##class_name( \
            const TargetRegisterInfo* reg_info, \
            const TargetInstInfo* inst_info) { \
            return std::make_unique<class_name>(reg_info, inst_info); \
        } \
        struct class_name##InstrSelectorRegistrar { \
            class_name##InstrSelectorRegistrar() { \
                codegen::InstructionSelectorRegistry::registerSelector( \
                    target_name, create##class_name); \
            } \
        }; \
        static class_name##InstrSelectorRegistrar g_##class_name##_instr_selector_registrar; \
    }

} // namespace codegen