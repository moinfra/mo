// machine_module.h - Code generation utilities for MachineModule
#pragma once

#include "../machine.h"
#include "../ir.h"
#include <memory>
#include <vector>
#include <string>

namespace codegen {

/// CodeGenModule - Wrapper around MachineModule for code generation
/// 
/// This class provides a high-level interface for converting IR to machine code,
/// building on top of the existing MachineModule infrastructure.
class CodeGenModule {
private:
    std::unique_ptr<MachineModule> machine_module_;
    Module* ir_module_;
    
public:
    /// Constructor creates a new CodeGenModule for the given IR module
    explicit CodeGenModule(Module* ir_module);
    
    /// Destructor
    ~CodeGenModule() = default;
    
    /// Non-copyable
    CodeGenModule(const CodeGenModule&) = delete;
    CodeGenModule& operator=(const CodeGenModule&) = delete;
    
    /// Moveable
    CodeGenModule(CodeGenModule&&) = default;
    CodeGenModule& operator=(CodeGenModule&&) = default;
    
    /// Add a function to the machine module
    /// @param ir_function The IR function to convert
    /// @return Pointer to the created MachineFunction
    MachineFunction* addFunction(Function* ir_function);
    
    /// Set target-specific information
    /// @param reg_info Target register information
    /// @param inst_info Target instruction information  
    void setTargetInfo(const TargetRegisterInfo* reg_info,
                      const TargetInstInfo* inst_info);
    
    /// Get the underlying MachineModule
    /// @return Pointer to the MachineModule (ownership retained)
    MachineModule* getMachineModule() const { return machine_module_.get(); }
    
    /// Release ownership of the MachineModule
    /// @return Unique pointer to the MachineModule (ownership transferred)
    std::unique_ptr<MachineModule> releaseMachineModule();
    
    /// Get the IR module this CodeGenModule is based on
    /// @return Pointer to the IR Module
    Module* getIRModule() const { return ir_module_; }
    
    /// Get number of machine functions
    /// @return Number of functions in the machine module
    size_t getFunctionCount() const { return function_count_; }
    
    /// Check if module is empty
    /// @return True if no functions have been added
    bool empty() const { return function_count_ == 0; }
    
    /// Get target register info
    /// @return Pointer to target register info, or nullptr if not set
    const TargetRegisterInfo* getTargetRegisterInfo() const;
    
    /// Get target instruction info
    /// @return Pointer to target instruction info, or nullptr if not set  
    const TargetInstInfo* getTargetInstInfo() const;

private:
    size_t function_count_ = 0;
};

/// MachineModuleBuilder - Factory class for creating CodeGenModules
/// 
/// Provides convenient factory methods for creating CodeGenModules
/// with various configurations.
class MachineModuleBuilder {
public:
    /// Create a basic CodeGenModule
    /// @param ir_module IR module to convert
    /// @return Unique pointer to CodeGenModule
    static std::unique_ptr<CodeGenModule> create(Module* ir_module);
    
    /// Create a CodeGenModule with target information
    /// @param ir_module IR module to convert
    /// @param reg_info Target register information
    /// @param inst_info Target instruction information
    /// @return Unique pointer to CodeGenModule
    static std::unique_ptr<CodeGenModule> createWithTarget(
        Module* ir_module,
        const TargetRegisterInfo* reg_info,
        const TargetInstInfo* inst_info);
};

} // namespace codegen