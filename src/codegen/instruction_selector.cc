// instruction_selector.cc - Implementation of instruction selection interfaces
#include "instruction_selector.h"

#include <map>
#include <vector>
#include <string>

namespace codegen {

//===----------------------------------------------------------------------===//
// InstructionSelectorRegistry Implementation
//===----------------------------------------------------------------------===//

std::map<std::string, InstructionSelectorRegistry::CreateFunction>& 
InstructionSelectorRegistry::getRegistry() {
    static std::map<std::string, CreateFunction> registry;
    return registry;
}

void InstructionSelectorRegistry::registerSelector(const std::string& target_name, 
                                                  CreateFunction create_fn) {
    if (create_fn != nullptr) {
        getRegistry()[target_name] = create_fn;
    }
}

std::unique_ptr<InstructionSelector> InstructionSelectorRegistry::createSelector(
    const std::string& target_name,
    const TargetRegisterInfo* reg_info,
    const TargetInstInfo* inst_info) {
    
    auto& registry = getRegistry();
    auto it = registry.find(target_name);
    if (it != registry.end() && it->second != nullptr) {
        return it->second(reg_info, inst_info);
    }
    return nullptr;
}

std::vector<std::string> InstructionSelectorRegistry::getAvailableTargets() {
    std::vector<std::string> targets;
    auto& registry = getRegistry();
    
    targets.reserve(registry.size());
    for (const auto& pair : registry) {
        targets.push_back(pair.first);
    }
    
    return targets;
}

} // namespace codegen