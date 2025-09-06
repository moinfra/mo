// target_base.cc - Implementation of target base classes
#include "target_base.h"

#include <map>
#include <vector>
#include <string>

namespace codegen {

// Static registry storage
std::map<std::string, TargetCreateFunction>& TargetRegistry::getRegistry() {
    static std::map<std::string, TargetCreateFunction> registry;
    return registry;
}

void TargetRegistry::registerTarget(const std::string& name, TargetCreateFunction create_fn) {
    if (create_fn != nullptr) {
        getRegistry()[name] = create_fn;
    }
}

std::unique_ptr<TargetBase> TargetRegistry::createTarget(const std::string& name) {
    auto& registry = getRegistry();
    auto it = registry.find(name);
    if (it != registry.end() && it->second != nullptr) {
        return it->second();
    }
    return nullptr;
}

std::vector<std::string> TargetRegistry::getAvailableTargets() {
    std::vector<std::string> targets;
    auto& registry = getRegistry();
    
    targets.reserve(registry.size());
    for (const auto& pair : registry) {
        targets.push_back(pair.first);
    }
    
    return targets;
}

} // namespace codegen