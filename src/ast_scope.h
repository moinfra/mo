// ast_scope.h -- AST symbol table for variables and types
#pragma once

#include "ast.h"
#include <unordered_map>

namespace ast
{
    class Scope
    {
    public:
        Scope(Scope *parent) : parent_(parent) {}

        ast::TypePtr resolve_variable(const std::string &name) const;
        ast::TypePtr resolve_type(const std::string &name) const;
        bool insert_variable(std::string name, ast::TypePtr type);
        bool insert_type(std::string name, ast::TypePtr type);

        Scope *parent() const { return parent_; }

    private:
        Scope *parent_ = nullptr;
        std::unordered_map<std::string, ast::TypePtr> variables_;
        std::unordered_map<std::string, ast::TypePtr> types_;
    };

}
