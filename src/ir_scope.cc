#include "ir_scope.h"
#include "utils.h"

// Find a variable in the current or parent scopes
Value *Scope::resolve_variable(const std::string &name) const
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot find empty name");
    }

    auto var_it = variables_.find(name);
    if (var_it != variables_.end())
    {
        return var_it->second;
    }

    if (parent_)
    {
        return parent_->resolve_variable(name);
    }

    return nullptr;
}

// Insert a variable into the current scope
bool Scope::insert_variable(std::string name, Value *type)
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot insert empty name");
    }
    if (!type)
    {
        throw std::invalid_argument("Type cannot be null");
    }

    if (auto it = variables_.find(name); it != variables_.end())
    {
        MO_WARN("Variable already exists: %s", name.c_str());
        return false;
    }

    if (auto it = types_.find(name); it != types_.end())
    {
        MO_WARN("Type already exists: %s", name.c_str());
        return false;
    }

    variables_[name] = std::move(type);
    return true;
}

bool Scope::insert_type(std::string name, Type *type)
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot insert empty name");
    }
    if (!type)
    {
        MO_WARN("Inserting null type: %s", name.c_str());
    }

    if (auto it = variables_.find(name); it != variables_.end())
    {
        MO_WARN("Variable already exists: %s", name.c_str());
        return false;
    }

    if (auto it = types_.find(name); it != types_.end())
    {
        if (it->second != nullptr)
        {
            MO_WARN("Type already exists: %s", name.c_str());
            return false;
        }
        else
        {
            MO_WARN("Type already exists (but with null pointer): %s", name.c_str());
        }
    }

    types_[name] = std::move(type);
    return true;
}

// Resolve a type in the current or parent scopes
Type *Scope::resolve_type(const std::string &name) const
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot resolve empty name");
    }

    auto type_it = types_.find(name);
    if (type_it != types_.end())
    {
        return type_it->second;
    }

    if (parent_)
    {
        return parent_->resolve_type(name);
    }

    return nullptr;
}
