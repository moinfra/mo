//===----------------------------------------------------------------------===//
//                             Headers and Namespaces
//===----------------------------------------------------------------------===//

#include "ast.h"
#include <algorithm>

using namespace ast;

//===----------------------------------------------------------------------===//
//                             Type Class Implementation
//===----------------------------------------------------------------------===//

bool Type::operator!=(const Type &other) const
{
    return !(*this == other);
}

bool Type::operator==(const Type &other) const
{
    if (kind != other.kind || is_const != other.is_const)
        return false;

    switch (kind)
    {
    case Kind::Basic:
        return name == other.name && basic_kind == other.basic_kind;
    case Kind::Pointer:
        return pointee && other.pointee && (*pointee == *other.pointee);
    case Kind::Array:
        return element_type && other.element_type &&
               (*element_type == *other.element_type) &&
               array_size == other.array_size;
    case Kind::Function:
        if (params.size() != other.params.size())
            return false;
        for (size_t i = 0; i < params.size(); ++i)
            if (!params[i] || !other.params[i] || !(*params[i] == *other.params[i]))
                return false;
        return return_type && other.return_type && (*return_type == *other.return_type);
    case Kind::Struct:
        return members == other.members;
    default:
        return false;
    }
}

Type &Type::operator=(Type &&other) noexcept
{
    if (this != &other)
    {
        kind = std::exchange(other.kind, Type::Kind::Unknown);
        name = std::move(other.name);
        is_const = std::exchange(other.is_const, false);
        basic_kind = std::exchange(other.basic_kind, Type::BasicKind::Int);
        pointee = std::move(other.pointee);
        element_type = std::move(other.element_type);
        array_size = std::exchange(other.array_size, 0);
        params = std::move(other.params);
        return_type = std::move(other.return_type);
        members = std::move(other.members);
    }
    return *this;
}

Type &Type::operator=(const Type &other)
{
    if (this != &other)
    {
        Type temp(other);  // deepcopy
        swap(*this, temp); // swap resource
    }
    return *this;
}

// for copy constructor
Type::Type(const Type &other) : kind(other.kind),
                                name(other.name),
                                is_const(other.is_const),
                                basic_kind(other.basic_kind),
                                array_size(other.array_size)
{
    if (other.pointee)
    {
        pointee = std::make_unique<Type>(*other.pointee);
    }
    if (other.element_type)
    {
        element_type = std::make_unique<Type>(*other.element_type);
    }
    if (other.return_type)
    {
        return_type = std::make_unique<Type>(*other.return_type);
    }

    for (const auto &param : other.params)
    {
        params.push_back(std::make_unique<Type>(*param));
    }

    for (const auto &member : other.members)
    {
        members[member.first] = std::make_unique<Type>(*member.second);
    }
}

void Type::swap(Type &a, Type &b) noexcept
{
    using std::swap;
    swap(a.kind, b.kind);
    swap(a.name, b.name);
    swap(a.is_const, b.is_const);
    swap(a.basic_kind, b.basic_kind);
    swap(a.pointee, b.pointee);
    swap(a.element_type, b.element_type);
    swap(a.array_size, b.array_size);
    swap(a.params, b.params);
    swap(a.return_type, b.return_type);
    swap(a.members, b.members);
}

std::unique_ptr<Type> Type::clone() const
{
    auto result = std::make_unique<Type>();
    *result = *this;
    return result;
}

Type Type::get_void_type()
{
    Type result;
    result.kind = Kind::Void;
    return result;
}

Type Type::get_int_type()
{
    Type result;
    result.kind = Kind::Basic;
    result.basic_kind = BasicKind::Int;
    return result;
}

Type Type::get_float_type()
{
    Type result;
    result.kind = Kind::Basic;
    result.basic_kind = BasicKind::Float;
    return result;
}

Type Type::get_string_type()
{
    Type result;
    result.kind = Kind::Basic;
    result.basic_kind = BasicKind::String;
    return result;
}

Type Type::get_struct_type(
    const std::string &name,
    std::vector<ast::TypedField> fields)
{
    Type result;
    result.kind = Kind::Struct;
    result.name = name;
    for (const auto &field : fields)
    {
        result.members[field.name] = field.type->clone();
    }
    return result;
}

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
//                          TypedField Implementation
//===----------------------------------------------------------------------===//

TypedField::TypedField(TypePtr type, std::string name)
    : type(std::move(type)), name(std::move(name)) {}

TypedField::TypedField(const TypedField &other)
    : type(other.type ? other.type->clone() : nullptr), name(other.name) {}

TypedField &TypedField::operator=(TypedField other) noexcept
{
    TypedField::swap(*this, other);
    return *this;
}

void TypedField::swap(TypedField &a, TypedField &b) noexcept
{
    using std::swap;
    swap(a.name, b.name);
    swap(a.type, b.type);
}

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
void StructDecl::add_field(TypePtr type, std::string name)
{
    fields.emplace_back(std::move(type), std::move(name));
    field_map[fields.back().name] = fields.size() - 1;
}

void StructDecl::add_field(TypedField field)
{
    fields.emplace_back(std::move(field.type), std::move(field.name));
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

//===----------------------------------------------------------------------===//
//                          FunctionDecl Implementation
//===----------------------------------------------------------------------===//

void FunctionDecl::add_param(const std::string &name, std::unique_ptr<Type> type)
{
    params.emplace_back(std::move(type), name);
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
