// ir_scope.h -- Scope for the intermediate representation
#pragma once

#include "ir.h"
#include <unordered_map>

class Scope
{
public:
    Scope(Scope *parent) : parent_(parent) {
        if (parent_) {
            depth_ = parent_->depth_ + 1;
        }
    }

    Value* resolve_variable(const std::string &name) const;
    Type* resolve_type(const std::string &name) const;
    bool insert_variable(std::string name, Value* value);
    bool insert_type(std::string name, Type* type);

    Scope *parent() const { return parent_; }
    size_t depth() const { return depth_; }
    bool is_global() const { return depth_ == 0; }

private:
    Scope *parent_ = nullptr;
    size_t depth_ = 0;
    std::unordered_map<std::string, Value*> variables_;
    std::unordered_map<std::string, Type*> types_;
};
