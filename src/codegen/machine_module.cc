// machine_module.cc - Implementation of code generation utilities
#include "machine_module.h"

#include <cassert>

namespace codegen {

//===----------------------------------------------------------------------===//
// CodeGenModule Implementation
//===----------------------------------------------------------------------===//

CodeGenModule::CodeGenModule(Module* ir_module) 
    : ir_module_(ir_module)
    , function_count_(0)
{
    assert(ir_module && "IR module cannot be null");
    machine_module_ = std::make_unique<MachineModule>(ir_module);
}

MachineFunction* CodeGenModule::addFunction(Function* ir_function) {
    assert(ir_function && "IR function cannot be null");
    assert(machine_module_ && "MachineModule not initialized");
    
    // Create machine function using the underlying MachineModule
    auto* machine_func = machine_module_->create_machine_function(ir_function);
    
    if (machine_func) {
        function_count_++;
    }
    
    return machine_func;
}

void CodeGenModule::setTargetInfo(const TargetRegisterInfo* reg_info,
                                 const TargetInstInfo* inst_info) {
    assert(machine_module_ && "MachineModule not initialized");
    machine_module_->set_target_info(reg_info, inst_info);
}

std::unique_ptr<MachineModule> CodeGenModule::releaseMachineModule() {
    return std::move(machine_module_);
}

const TargetRegisterInfo* CodeGenModule::getTargetRegisterInfo() const {
    return machine_module_ ? machine_module_->target_reg_info() : nullptr;
}

const TargetInstInfo* CodeGenModule::getTargetInstInfo() const {
    return machine_module_ ? machine_module_->target_inst_info() : nullptr;
}

//===----------------------------------------------------------------------===//
// MachineModuleBuilder Implementation  
//===----------------------------------------------------------------------===//

std::unique_ptr<CodeGenModule> MachineModuleBuilder::create(Module* ir_module) {
    return std::make_unique<CodeGenModule>(ir_module);
}

std::unique_ptr<CodeGenModule> MachineModuleBuilder::createWithTarget(
    Module* ir_module,
    const TargetRegisterInfo* reg_info,
    const TargetInstInfo* inst_info) {
    
    auto codegen_module = std::make_unique<CodeGenModule>(ir_module);
    codegen_module->setTargetInfo(reg_info, inst_info);
    return codegen_module;
}

} // namespace codegen