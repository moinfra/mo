// ast.h - Abstract Syntax Tree of Mo language

#pragma once

#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <utility>

#include "lexer.h"
#include "ast_type.h"

namespace ast
{

    struct Expr;
    struct Statement;
    struct TypeAliasDecl;
    struct GlobalDecl;
    struct StructDecl;
    struct ImplBlock;
    struct FunctionDecl;
    struct VarDeclStmt;
    struct Type;
    struct TypedField;

    using ExprPtr = std::unique_ptr<Expr>;
    using TypePtr = std::unique_ptr<Type>;
    using StmtPtr = std::unique_ptr<Statement>;

    // Program Structure
    struct Program
    {
        std::vector<std::unique_ptr<TypeAliasDecl>> aliases;
        std::vector<std::unique_ptr<StructDecl>> structs;
        std::vector<std::unique_ptr<ImplBlock>> impl_blocks;
        std::vector<std::unique_ptr<FunctionDecl>> functions;
        std::vector<std::unique_ptr<GlobalDecl>> globals;
    };

    // Expressions
    struct Expr
    {
        enum class Category
        {
            Unknown,
            LValue,
            RValue
        };

        virtual ~Expr() = default;

        bool is_lvalue() const { return expr_category == Category::LValue; }
        bool is_rvalue() const { return expr_category == Category::RValue; }

        // -- Filled by the type checker --
        Category expr_category = Category::Unknown;
        TypePtr type = nullptr; // inferred type of the expression
        virtual std::string name() const { return "Expr"; }
    };

    struct VariableExpr : Expr
    {
        std::string identifier;
        explicit VariableExpr(std::string name) : identifier(std::move(name)) {}
        std::string name() const override { return "VariableExpr"; }
    };

    struct LiteralExpr : Expr
    {
        virtual ~LiteralExpr() = default;
        std::string name() const override { return "LiteralExpr"; }
    };

    struct IntegerLiteralExpr : LiteralExpr
    {
        // TODO: support bitwidth and sign
        int value;
        IntegerLiteralExpr(int value) : value(value) {}
        std::string name() const override { return "IntegerLiteralExpr"; }
    };

    struct BooleanLiteralExpr : LiteralExpr
    {
        bool value;
        BooleanLiteralExpr(bool value) : value(value) {}
        std::string name() const override { return "BooleanLiteralExpr"; }
    };

    struct FloatLiteralExpr : LiteralExpr
    {
        float value;
        FloatLiteralExpr(float value) : value(value) {}
        std::string name() const override { return "FloatLiteralExpr"; }
    };

    struct StringLiteralExpr : LiteralExpr
    {
        std::string value;
        StringLiteralExpr(std::string value) : value(std::move(value)) {}
        std::string name() const override { return "StringLiteralExpr"; }
    };

    struct StructLiteralExpr : LiteralExpr
    {
        std::string struct_name; // Name of the struct (nullable)
        std::vector<std::pair<std::string, ExprPtr>> members;
        std::unordered_map<std::string, size_t> member_map;

        void add_member(std::string name, ExprPtr expr);
        Expr *get_member(const std::string &name) const;

        explicit StructLiteralExpr(std::string struct_name) : struct_name(std::move(struct_name)) {}
        StructLiteralExpr(std::string struct_name, std::vector<std::pair<std::string, ExprPtr>> members)
            : struct_name(std::move(struct_name)), members(std::move(members))
        {
            for (size_t i = 0; i < members.size(); ++i)
            {
                member_map[members[i].first] = i;
            }
        }
        std::string name() const override { return "StructLiteralExpr"; }
    };

    struct BinaryExpr : Expr
    {
        TokenType op;
        ExprPtr left;
        ExprPtr right;

        BinaryExpr(TokenType op, ExprPtr left, ExprPtr right)
            : op(op), left(std::move(left)), right(std::move(right)) {}
        std::string name() const override { return "BinaryExpr"; }
    };

    struct UnaryExpr : Expr
    {
        TokenType op;
        ExprPtr operand;

        UnaryExpr(TokenType op, ExprPtr operand)
            : op(op), operand(std::move(operand)) {}
        std::string name() const override { return "UnaryExpr"; }
    };

    struct CallExpr : Expr
    {
        ExprPtr callee;
        std::vector<ExprPtr> args;

        CallExpr() = default;
        CallExpr(ExprPtr callee, std::vector<ExprPtr> args)
            : callee(std::move(callee)), args(std::move(args)) {}
        CallExpr(std::string callee_name, std::vector<ExprPtr> args)
            : callee(std::make_unique<VariableExpr>(callee_name)), args(std::move(args)) {}
        std::string name() const override { return "CallExpr"; }
    };

    struct MemberAccessExpr : Expr
    {
        ExprPtr object;
        std::string member;
        TokenType accessor;

        bool is_call = false; // obj.member(...) or obj.method(...)
        bool is_method_call = false;
        std::vector<ExprPtr> args;

        FunctionDecl *resolved_func = nullptr;
        bool is_indirect_call = false;

        MemberAccessExpr() = default;
        MemberAccessExpr(ExprPtr object, std::string member, TokenType accessor)
            : object(std::move(object)), member(std::move(member)), accessor(accessor) {}
        std::string name() const override { return "MemberAccessExpr"; }
    };

    struct ArrayAccessExpr : Expr
    {
        ExprPtr array;
        ExprPtr index;

        ArrayAccessExpr() = default;
        ArrayAccessExpr(ExprPtr array, ExprPtr index)
            : array(std::move(array)), index(std::move(index)) {}
        std::string name() const override { return "ArrayAccessExpr"; }
    };

    struct CastExpr : Expr
    {
        TypePtr target_type;
        ExprPtr expr;

        CastExpr(TypePtr target_type, ExprPtr expr)
            : target_type(std::move(target_type)), expr(std::move(expr)) {}
        std::string name() const override { return "CastExpr"; }
    };

    struct SizeofExpr : Expr
    {
        enum class Kind
        {
            Type, // sizeof(type)
            Expr  // sizeof(expr)
        } kind;

        union
        {
            TypePtr target_type;
            ExprPtr target_expr;
        };

        // Default constructor (deleted)
        SizeofExpr() = delete;

        // Constructor for Kind::Type
        explicit SizeofExpr(TypePtr type)
            : kind(Kind::Type)
        {
            new (&target_type) TypePtr(std::move(type));
        }

        // Constructor for Kind::Expr
        explicit SizeofExpr(ExprPtr expr)
            : kind(Kind::Expr)
        {
            new (&target_expr) ExprPtr(std::move(expr));
        }

        // Destructor
        ~SizeofExpr()
        {
            if (kind == Kind::Type)
            {
                target_type.~TypePtr();
            }
            else if (kind == Kind::Expr)
            {
                target_expr.~ExprPtr();
            }
        }

        // Copy constructor (deleted)
        SizeofExpr(const SizeofExpr &) = delete;

        // Copy assignment operator (deleted)
        SizeofExpr &operator=(const SizeofExpr &) = delete;

        // Move constructor
        SizeofExpr(SizeofExpr &&other) noexcept
            : kind(other.kind)
        {
            if (kind == Kind::Type)
            {
                new (&target_type) TypePtr(std::move(other.target_type));
            }
            else if (kind == Kind::Expr)
            {
                new (&target_expr) ExprPtr(std::move(other.target_expr));
            }
        }

        // Move assignment operator
        SizeofExpr &operator=(SizeofExpr &&other) noexcept
        {
            if (this != &other)
            {
                this->~SizeofExpr();

                kind = other.kind;
                if (kind == Kind::Type)
                {
                    new (&target_type) TypePtr(std::move(other.target_type));
                }
                else if (kind == Kind::Expr)
                {
                    new (&target_expr) ExprPtr(std::move(other.target_expr));
                }
            }
            return *this;
        }
        std::string name() const override { return "SizeofExpr"; }
    };

    struct TupleExpr : Expr
    {
        std::vector<ExprPtr> elements;

        explicit TupleExpr(std::vector<ExprPtr> elements) : elements(std::move(elements)) {}
        std::string name() const override { return "TupleExpr"; }
    };

    struct AddressOfExpr : Expr
    {
        ExprPtr operand;
        explicit AddressOfExpr(ExprPtr operand) : operand(std::move(operand)) {}
        std::string name() const override { return "AddressOfExpr"; }
    };

    struct DerefExpr : Expr
    {
        ExprPtr operand;
        explicit DerefExpr(ExprPtr operand) : operand(std::move(operand)) {}
        std::string name() const override { return "DerefExpr"; }
    };

    struct InitListExpr : Expr
    {
        std::vector<ExprPtr> members;

        explicit InitListExpr(std::vector<ExprPtr> members) : members(std::move(members)) {}
        std::string name() const override { return "InitListExpr"; }
    };

    struct FunctionPointerExpr : Expr
    {
        std::vector<TypePtr> param_types;
        TypePtr return_type;

        FunctionPointerExpr(std::vector<TypePtr> param_types, TypePtr return_type)
            : param_types(std::move(param_types)), return_type(std::move(return_type)) {}
        FunctionPointerExpr() = default;
        std::string name() const override { return "FunctionPointerExpr"; }
    };
    // Statements
    struct Statement
    {
        virtual ~Statement() = default;
    };

    struct ExprStmt : Statement
    {
        ExprPtr expr;

        explicit ExprStmt(ExprPtr expr) : expr(std::move(expr)) {}
    };

    struct BlockStmt : Statement
    {
        std::vector<StmtPtr> statements;

        explicit BlockStmt() = default;
        explicit BlockStmt(std::vector<StmtPtr> statements) : statements(std::move(statements)) {}
    };

    struct VarDeclStmt : Statement
    {
        bool is_const;
        std::string name;
        TypePtr type;
        ExprPtr init_expr;

        VarDeclStmt() : is_const(false) {}
        VarDeclStmt(bool is_const, std::string name, TypePtr type, ExprPtr init_expr)
            : is_const(is_const),
              name(std::move(name)),
              type(std::move(type)),
              init_expr(std::move(init_expr)) {}

        static void swap(VarDeclStmt &a, VarDeclStmt &b) noexcept;
        VarDeclStmt &operator=(VarDeclStmt other) noexcept;
        VarDeclStmt(VarDeclStmt &&other) noexcept;
        VarDeclStmt(const VarDeclStmt &other) = delete;
    };

    struct ReturnStmt : Statement
    {
        ExprPtr value;
        explicit ReturnStmt(ExprPtr value) : value(std::move(value)) {}
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

    struct WhileStmt : Statement
    {
        ExprPtr condition;
        StmtPtr body;

        WhileStmt(ExprPtr condition, StmtPtr body)
            : condition(std::move(condition)), body(std::move(body)) {}
    };

    struct BreakStmt : Statement
    {
    };

    struct ContinueStmt : Statement
    {
    };

    class StructDecl : public Statement
    {
    public:
        std::string name;
        std::vector<TypedField> fields;
        std::vector<FunctionDecl> methods;
        std::unordered_map<std::string, size_t> field_map;  // Map for quick field lookup
        std::unordered_map<std::string, size_t> method_map; // Map for quick method lookup
        mutable TypePtr cached_type_;

        StructDecl() = default;
        StructDecl(std::string name, std::vector<TypedField> fields);

        // Add a field to the struct
        void add_field(TypedField field);

        // Get a field by name
        const TypedField *get_field(std::string name) const;

        // Add a method to the struct
        void add_method(FunctionDecl method);

        // Get a method by name
        FunctionDecl *get_method(std::string name) const;

        // Get the type of the struct
        Type* type() const;
    };

    struct FunctionDecl
    {
        std::string name;
        TypePtr return_type = nullptr;
        std::vector<TypedField> params;
        std::vector<StmtPtr> body;

        // method
        TypePtr receiver_type = nullptr;
        bool is_method = false;
        bool is_static = false;

        void add_param(const std::string &name, TypePtr type);
        Type &get_param_type(const std::string &name);
        TypePtr type() const;

        static FunctionDecl create_main_function();

        FunctionDecl() = default;
        explicit FunctionDecl(std::string name, TypePtr return_type, std::vector<TypedField> params, std::vector<StmtPtr> body)
            : name(std::move(name)),
              return_type(std::move(return_type)),
              params(std::move(params)),
              body(std::move(body)) {}

        explicit FunctionDecl(std::string name, TypePtr return_type, std::vector<TypedField> params, std::vector<StmtPtr> body, TypePtr receiver_type, bool is_static)
            : name(std::move(name)),
              return_type(std::move(return_type)),
              params(std::move(params)),
              body(std::move(body)),
              receiver_type(std::move(receiver_type)),
              is_static(is_static) {}
    };

    struct ImplBlock
    {
        TypePtr target_type;
        std::vector<std::unique_ptr<FunctionDecl>> methods;
    };

    struct GlobalDecl : VarDeclStmt
    {
        bool is_exported = false;

        GlobalDecl() : VarDeclStmt() {};
        GlobalDecl(VarDeclStmt &&varDeclStmt, bool exported = false);
    };

    struct TypeAliasDecl
    {
        std::string name;
        TypePtr type;

        TypeAliasDecl() = default;
        TypeAliasDecl(std::string name, TypePtr type) : name(std::move(name)), type(std::move(type)) {}
        TypeAliasDecl(const TypeAliasDecl &other);
        TypeAliasDecl(TypeAliasDecl &&other) noexcept;
        TypeAliasDecl &operator=(TypeAliasDecl other) noexcept;

        static void swap(TypeAliasDecl &a, TypeAliasDecl &b) noexcept;
    };
};
