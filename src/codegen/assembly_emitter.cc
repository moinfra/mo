// assembly_emitter.cc - Implementation of assembly emission interfaces
#include "assembly_emitter.h"

#include <map>
#include <vector>
#include <string>

namespace codegen {

//===----------------------------------------------------------------------===//
// AssemblyEmitterRegistry Implementation
//===----------------------------------------------------------------------===//

std::map<std::string, AssemblyEmitterRegistry::CreateFunction>& 
AssemblyEmitterRegistry::getRegistry() {
    static std::map<std::string, CreateFunction> registry;
    return registry;
}

void AssemblyEmitterRegistry::registerEmitter(const std::string& target_name, 
                                              CreateFunction create_fn) {
    if (create_fn != nullptr) {
        getRegistry()[target_name] = create_fn;
    }
}

std::unique_ptr<AssemblyEmitter> AssemblyEmitterRegistry::createEmitter(
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

std::vector<std::string> AssemblyEmitterRegistry::getAvailableTargets() {
    std::vector<std::string> targets;
    auto& registry = getRegistry();
    
    targets.reserve(registry.size());
    for (const auto& pair : registry) {
        targets.push_back(pair.first);
    }
    
    return targets;
}

} // namespace codegen