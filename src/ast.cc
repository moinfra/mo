//===----------------------------------------------------------------------===//
//                             Headers and Namespaces
//===----------------------------------------------------------------------===//

#include "ast.h"
#include <algorithm>

using namespace ast;
//===----------------------------------------------------------------------===//
//                         StructLiteralExpr Implementation
//===----------------------------------------------------------------------===//

void StructLiteralExpr::add_member(std::string name, ExprPtr expr)
{
    size_t index = members.size();
    members.emplace_back(std::move(name), std::move(expr));
    member_map[members.back().first] = index;
}

Expr *StructLiteralExpr::get_member(const std::string &name) const
{
    auto it = member_map.find(name);
    if (it != member_map.end() && it->second < members.size())
    {
        return members[it->second].second.get();
    }
    return nullptr;
}

//===----------------------------------------------------------------------===//
//                          VarDeclStmt Implementation
//===----------------------------------------------------------------------===//

void VarDeclStmt::swap(VarDeclStmt &a, VarDeclStmt &b) noexcept
{
    using std::swap;
    swap(a.is_const, b.is_const);
    swap(a.name, b.name);
    swap(a.type, b.type);
    swap(a.init_expr, b.init_expr);
}

VarDeclStmt &VarDeclStmt::operator=(VarDeclStmt other) noexcept
{
    VarDeclStmt::swap(*this, other);
    return *this;
}

VarDeclStmt::VarDeclStmt(VarDeclStmt &&other) noexcept
    : is_const(std::move(other.is_const)),
      name(std::move(other.name)),
      type(std::move(other.type)),
      init_expr(std::move(other.init_expr)) {}

//===----------------------------------------------------------------------===//
//                          StructDecl Implementation
//===----------------------------------------------------------------------===//

StructDecl::StructDecl(std::string name, std::vector<TypedField> fields)
    : name(std::move(name)), fields(std::move(fields))
{
    this->fields.reserve(this->fields.size());
    // Build the field map for quick lookup
    for (size_t i = 0; i < this->fields.size(); ++i)
    {
        field_map[this->fields[i].name] = i;
    }
}

// Add a field to the struct
void StructDecl::add_field(TypedField field)
{
    fields.emplace_back(std::move(field));
    field_map[fields.back().name] = fields.size() - 1;
}

// Get a field by name
const TypedField *StructDecl::get_field(std::string_view name) const
{
    auto it = field_map.find(name);
    if (it != field_map.end() && it->second < fields.size())
    {
        return &fields[it->second];
    }
    return nullptr;
}

// Add a method to the struct
void StructDecl::add_method(FunctionDecl method)
{
    methods.emplace_back(std::move(method));
    method_map[methods.back().name] = methods.size() - 1;
}

// Get a method by name
FunctionDecl *StructDecl::get_method(std::string_view name) const
{
    auto it = method_map.find(name);
    if (it != method_map.end() && it->second < methods.size())
    {
        return const_cast<FunctionDecl *>(&methods[it->second]);
    }
    return nullptr;
}

//===----------------------------------------------------------------------===//
//                          FunctionDecl Implementation
//===----------------------------------------------------------------------===//

void FunctionDecl::add_param(const std::string &name, std::unique_ptr<Type> type)
{
    params.emplace_back(name, std::move(type));
}

Type &FunctionDecl::get_param_type(const std::string &name)
{
    for (const auto &p : params)
    {
        if (p.name == name)
        {
            if (!p.type)
            {
                throw std::runtime_error("parameter type is null");
            }
            return *p.type;
        }
    }
    throw std::runtime_error("parameter not found: " + name);
}

TypePtr FunctionDecl::type() const
{
    std::vector<TypePtr> param_types;

    for (const auto &param : params)
    {
        param_types.push_back(param.type->clone());
    }

    return Type::create_function(return_type->clone(), std::move(param_types));
}

FunctionDecl FunctionDecl::create_main_function()
{
    FunctionDecl main_function;
    main_function.name = "main";
    main_function.add_param("argc", Type::create_int());

    // auto char_type = Type::get_char_type();
    auto char_type = Type::create_int(8);
    auto argv_type = Type::create_pointer(Type::create_pointer(std::move(char_type)));
    main_function.add_param("argv", std::move(argv_type));

    main_function.return_type = Type::create_int();

    return main_function;
}

//===----------------------------------------------------------------------===//
//                          GlobalDecl Implementation
//===----------------------------------------------------------------------===//

GlobalDecl::GlobalDecl(VarDeclStmt &&varDeclStmt, bool exported)
    : VarDeclStmt(std::move(varDeclStmt)), is_exported(exported) {}

//===----------------------------------------------------------------------===//
//                          TypeAliasDecl Implementation
//===----------------------------------------------------------------------===//

TypeAliasDecl::TypeAliasDecl(const TypeAliasDecl &other)
    : name(other.name), type(other.type ? other.type->clone() : nullptr) {}

TypeAliasDecl::TypeAliasDecl(TypeAliasDecl &&other) noexcept
    : name(), type(nullptr)
{
    TypeAliasDecl::swap(*this, other);
}

TypeAliasDecl &TypeAliasDecl::operator=(TypeAliasDecl other) noexcept
{
    TypeAliasDecl::swap(*this, other);
    return *this;
}

void TypeAliasDecl::swap(TypeAliasDecl &a, TypeAliasDecl &b) noexcept
{
    using std::swap;
    swap(a.name, b.name);
    swap(a.type, b.type);
}
