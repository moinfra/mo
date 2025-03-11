#include "ast_scope.h"
#include "mo_debug.h"

using namespace ast;

// Find a variable in the current or parent scopes
TypePtr Scope::resolve_variable(const std::string &name) const
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot find empty name");
    }

    auto var_it = variables_.find(name);
    if (var_it != variables_.end())
    {
        return var_it->second->clone();
    }

    if (parent_)
    {
        return parent_->resolve_variable(name);
    }

    return nullptr;
}

// Insert a variable into the current scope
bool Scope::insert_variable(std::string name, TypePtr type)
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot insert empty name");
    }
    if (!type)
    {
        MO_WARN("Type be null");
    }

    if (variables_.find(name) != variables_.end() || types_.find(name) != types_.end())
    {
        MO_WARN("Variable or type already exists: %s", name.c_str());
        return false; // Variable or type already exists
    }

    variables_[name] = std::move(type);
    return true;
}

bool Scope::insert_type(std::string name, TypePtr type)
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot insert empty name");
    }
    if (!type)
    {
        MO_WARN("Type be null");
    }

    if (variables_.find(name) != variables_.end())
    {
        MO_WARN("Variable already exists: %s", name.c_str());
        return false; // Variable or type already exists
    }

    types_[name] = std::move(type);
    return true;
}

// Resolve a type in the current or parent scopes
TypePtr Scope::resolve_type(const std::string &name) const
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot resolve empty name");
    }

    auto type_it = types_.find(name);
    if (type_it != types_.end())
    {
        return type_it->second->clone();
    }

    if (parent_)
    {
        return parent_->resolve_type(name);
    }

    return nullptr;
}
