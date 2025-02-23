// src/ast.h
#pragma once
#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <utility>
#include "lexer.h"

struct Type
{
    enum class Kind
    {
        Unknown,
        Basic,
        Pointer,
        Array,
        Function,
        Struct
    };

    Kind kind = Kind::Basic;
    std::string name;
    bool is_const = false;

    // for pointer type
    std::unique_ptr<Type> pointee;

    // for array type
    std::unique_ptr<Type> element_type;
    int array_size = -1;

    // for function type
    std::vector<std::unique_ptr<Type>> params;
    std::unique_ptr<Type> return_type;

    // for struct type
    std::unordered_map<std::string, std::unique_ptr<Type>> members;

    Type() : kind(Kind::Unknown) {}

    // for type comparison
    bool operator==(const Type &other) const;

    Type &operator=(Type &&other) noexcept
    {
        if (this != &other)
        {
            kind = std::exchange(other.kind, Type::Kind::Unknown);
            name = std::move(other.name);
            is_const = std::exchange(other.is_const, false);
            pointee = std::move(other.pointee);
            element_type = std::move(other.element_type);
            array_size = std::exchange(other.array_size, 0);
            params = std::move(other.params);
            return_type = std::move(other.return_type);
            members = std::move(other.members);
        }
        return *this;
    }

    Type &operator=(const Type &other)
    {
        if (this != &other)
        {
            Type temp(other);  // deepcopy
            swap(*this, temp); // swap resource
        }
        return *this;
    }

    // for copy constructor
    Type(const Type &other) : kind(other.kind),
                              name(other.name),
                              is_const(other.is_const),
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

    void swap(Type &a, Type &b) noexcept
    {
        using std::swap;
        swap(a.kind, b.kind);
        swap(a.name, b.name);
        swap(a.is_const, b.is_const);
        swap(a.pointee, b.pointee);
        swap(a.element_type, b.element_type);
        swap(a.array_size, b.array_size);
        swap(a.params, b.params);
        swap(a.return_type, b.return_type);
        swap(a.members, b.members);
    }

    std::unique_ptr<Type> clone() const
    {
        auto result = std::make_unique<Type>();
        *result = *this;
        return result;
    }
};

struct Expr
{
    enum class Category
    {
        LValue,
        RValue
    };

    virtual ~Expr() = default;
    Category exprCategory;
};

using ExprPtr = std::unique_ptr<Expr>;
using TypePtr = std::unique_ptr<Type>;

struct VariableExpr : Expr
{
    std::string name;

    VariableExpr(std::string name) : name(std::move(name)) {}
};

struct IntegerLiteralExpr : Expr
{
    int value;

    IntegerLiteralExpr(int value) : value(value) {}
};

struct FloatLiteralExpr : Expr
{
    float value;

    FloatLiteralExpr(float value) : value(value) {}
};

struct StringLiteralExpr : Expr
{
    std::string value;

    StringLiteralExpr(std::string value) : value(std::move(value)) {}
};

struct BinaryExpr : Expr
{
    TokenType op;
    std::unique_ptr<Expr> left;
    std::unique_ptr<Expr> right;

    BinaryExpr(TokenType op, ExprPtr left, ExprPtr right)
        : op(op), left(std::move(left)), right(std::move(right)) {}
};

struct UnaryExpr : Expr
{
    TokenType op;
    std::unique_ptr<Expr> operand;

    UnaryExpr(TokenType op, ExprPtr operand)
        : op(op), operand(std::move(operand)) {}
};

struct CallExpr : Expr
{
    ExprPtr callee;
    std::vector<ExprPtr> args;
};

struct MemberAccessExpr : Expr
{
    std::unique_ptr<Expr> object;
    std::string member;
    bool isPointerAccess;
    TokenType accessor;
};

struct CastExpr : Expr
{
    std::unique_ptr<Type> target_type;
    ExprPtr expr;
};

struct SizeofExpr : Expr
{
    std::unique_ptr<Type> target_type;
};

struct InitListExpr : Expr
{
    std::vector<ExprPtr> members;
};

// 补充在Expr派生类中
struct FunctionPointerExpr : Expr
{
    std::vector<std::unique_ptr<Type>> param_types;
    std::unique_ptr<Type> return_type;

    FunctionPointerExpr() = default;
};

struct StructLiteralExpr : Expr
{
    std::string struct_name; // Name of the struct (nullable)
    std::vector<std::pair<std::string, ExprPtr>> members;
    std::unordered_map<std::string, size_t> member_map;

    void add_member(std::string name, ExprPtr expr)
    {
        size_t index = members.size();
        members.emplace_back(std::move(name), std::move(expr));
        member_map[members.back().first] = index;
    }

    Expr *get_member(const std::string &name) const
    {
        auto it = member_map.find(name);
        if (it != member_map.end() && it->second < members.size())
        {
            return members[it->second].second.get();
        }
        return nullptr;
    }
};

struct Statement
{
    virtual ~Statement() = default;
};
using StmtPtr = std::unique_ptr<Statement>;

struct BlockStmt : Statement
{
    std::vector<StmtPtr> statements;
};

struct VarDeclStmt : Statement
{
    bool is_const;
    std::string name;
    std::unique_ptr<Type> type;
    std::unique_ptr<Expr> init_expr;

    VarDeclStmt() : is_const(false) {}
    // 新增 swap 函数
    friend void swap(VarDeclStmt &a, VarDeclStmt &b) noexcept
    {
        using std::swap;
        swap(a.is_const, b.is_const);
        swap(a.name, b.name);
        swap(a.type, b.type);
        swap(a.init_expr, b.init_expr);
    }

    // 简化后的拷贝赋值（利用 copy-and-swap）
    VarDeclStmt &operator=(VarDeclStmt other) noexcept
    {
        swap(*this, other);
        return *this;
    }

    VarDeclStmt(VarDeclStmt &&other) noexcept = default;

    VarDeclStmt(const VarDeclStmt &other)
        : is_const(other.is_const),
          name(other.name),
          type(other.type ? std::make_unique<Type>(*other.type) : nullptr),
          init_expr(other.init_expr ? std::make_unique<Expr>(*other.init_expr) : nullptr) {}
};

struct ReturnStmt : Statement
{
    ExprPtr value;

    ReturnStmt(ExprPtr value) : value(std::move(value)) {}
};

struct IfStmt : Statement
{
    ExprPtr condition;
    StmtPtr then_branch;
    StmtPtr else_branch;

    IfStmt(ExprPtr condition, StmtPtr then_branch, StmtPtr else_branch)
        : condition(std::move(condition)),
          then_branch(std::move(then_branch)),
          else_branch(std::move(else_branch)) {}
};

struct TypedField
{
    TypePtr type;
    std::string name;

    TypedField(TypePtr type, std::string name)
        : type(std::move(type)), name(std::move(name)) {}

    TypedField(const TypedField &other)
        : type(other.type ? other.type->clone() : nullptr), name(other.name) {}

    TypedField(TypedField &&other) noexcept = default;

    TypedField &operator=(TypedField other) noexcept
    {
        swap(*this, other);
        return *this;
    }

    friend void swap(TypedField &a, TypedField &b) noexcept
    {
        using std::swap;
        swap(a.name, b.name);
        swap(a.type, b.type);
    }
};

class StructDecl : public Statement
{
public:
    std::string name;
    std::vector<TypedField> fields;
    std::unordered_map<std::string_view, size_t> field_map; // Map for quick field lookup

    StructDecl() = default;

    StructDecl(std::string name, std::vector<TypedField> fields)
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
    void add_field(TypePtr type, std::string name)
    {
        fields.emplace_back(std::move(type), std::move(name));
        field_map[fields.back().name] = fields.size() - 1;
    }

    void add_field(TypedField field)
    {
        fields.emplace_back(std::move(field.type), std::move(field.name));
        field_map[fields.back().name] = fields.size() - 1;
    }

    // Get a field by name
    const TypedField *get_field(std::string_view name) const
    {
        auto it = field_map.find(name);
        if (it != field_map.end() && it->second < fields.size())
        {
            return &fields[it->second];
        }
        return nullptr;
    }
};

// 增强函数声明
struct FunctionDecl
{
    std::string name;
    std::unique_ptr<Type> return_type;
    std::vector<TypedField> params;
    std::vector<StmtPtr> body;

    // 方法特有属性
    bool is_method = false;
    std::unique_ptr<Type> receiver_type; // 方法的接收者类型

    void add_param(const std::string &name, std::unique_ptr<Type> type)
    {
        params.emplace_back(name, std::move(type));
    }

    Type &get_param_type(const std::string &name)
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
};

struct StructDecl;
struct ImplBlock;
struct FunctionDecl;
struct VarDeclStmt;

struct Program
{
    std::vector<std::unique_ptr<StructDecl>> structs;
    std::vector<std::unique_ptr<ImplBlock>> impl_blocks;
    std::vector<std::unique_ptr<FunctionDecl>> functions;
    std::vector<std::unique_ptr<VarDeclStmt>> globals;
};

struct ImplBlock
{
    std::unique_ptr<Type> target_type;
    std::vector<std::unique_ptr<FunctionDecl>> methods;
};

struct GlobalDecl : VarDeclStmt
{
    bool is_exported = false;

    GlobalDecl() : VarDeclStmt() {};

    GlobalDecl(const VarDeclStmt &varDeclStmt)
        : VarDeclStmt(varDeclStmt), is_exported(false) {}

    GlobalDecl(VarDeclStmt &&varDeclStmt, bool exported = false)
        : VarDeclStmt(std::move(varDeclStmt)), is_exported(exported) {}
};
