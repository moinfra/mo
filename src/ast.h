#pragma once

#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <utility>

#include "lexer.h"
namespace ast
{
    // Forwards declarations
    struct Expr;
    struct Statement;
    struct StructDecl;
    struct ImplBlock;
    struct FunctionDecl;
    struct VarDeclStmt;
    struct Type;

    using ExprPtr = std::unique_ptr<Expr>;
    using TypePtr = std::unique_ptr<Type>;
    using StmtPtr = std::unique_ptr<Statement>;

    // Program Structure
    struct Program
    {
        std::vector<std::unique_ptr<StructDecl>> structs;
        std::vector<std::unique_ptr<ImplBlock>> impl_blocks;
        std::vector<std::unique_ptr<FunctionDecl>> functions;
        std::vector<std::unique_ptr<VarDeclStmt>> globals;
    };

    // Type Definition
    struct Type
    {
        enum class Kind
        {
            Unknown,
            Void,
            Basic,    // Int/Float/String
            Pointer,  // T*
            Array,    // T[N]
            Function, // fn() -> T
            Struct,   // struct { ... }
            Alias     // 类型别名（如 typedef）
        };
        enum class BasicKind
        {
            Int,
            Float,
            String
        };

        Kind kind = Kind::Unknown;
        std::string name;
        bool is_const = false;
        BasicKind basic_kind; // 基本类型的种类

        // for pointer type
        TypePtr pointee;

        // for array type
        TypePtr element_type;
        int array_size = -1;

        // for function type
        std::vector<TypePtr> params;
        TypePtr return_type;

        // for struct type
        std::unordered_map<std::string, TypePtr> members;

        Type() : kind(Kind::Unknown) {}

        // for type comparison
        bool operator==(const Type &other) const;
        bool operator!=(const Type &other) const;

        Type &operator=(Type &&other) noexcept;
        Type &operator=(const Type &other);

        // for copy constructor
        Type(const Type &other);

        void swap(Type &a, Type &b) noexcept;

        TypePtr clone() const;

        static Type get_void_type();
        static Type get_int_type();
        static Type get_float_type();
    };

    // Expressions
    struct Expr
    {
        enum class Category
        {
            LValue,
            RValue
        };

        virtual ~Expr() = default;
        Category expr_category = Category::RValue;
    };

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
        ExprPtr left;
        ExprPtr right;

        BinaryExpr(TokenType op, ExprPtr left, ExprPtr right)
            : op(op), left(std::move(left)), right(std::move(right)) {}
    };

    struct UnaryExpr : Expr
    {
        TokenType op;
        ExprPtr operand;

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
        ExprPtr object;
        std::string member;
        TokenType accessor;
    };

    struct ArrayAccessExpr : Expr
    {
        ExprPtr array;
        ExprPtr index;
    };

    struct CastExpr : Expr
    {
        TypePtr target_type;
        ExprPtr expr;
    };

    struct SizeofExpr : Expr
    {
        TypePtr target_type;
    };

    struct AddressOfExpr : Expr
    {
        ExprPtr operand;
        AddressOfExpr(ExprPtr operand) : operand(std::move(operand)) {}
    };

    struct DerefExpr : Expr
    {
        ExprPtr operand;
        DerefExpr(ExprPtr operand) : operand(std::move(operand)) {}
    };

    struct InitListExpr : Expr
    {
        std::vector<ExprPtr> members;
    };

    struct FunctionPointerExpr : Expr
    {
        std::vector<TypePtr> param_types;
        TypePtr return_type;

        FunctionPointerExpr() = default;
    };

    struct StructLiteralExpr : Expr
    {
        std::string struct_name; // Name of the struct (nullable)
        std::vector<std::pair<std::string, ExprPtr>> members;
        std::unordered_map<std::string, size_t> member_map;

        void add_member(std::string name, ExprPtr expr);
        Expr *get_member(const std::string &name) const;
    };

    // Statements
    struct Statement
    {
        virtual ~Statement() = default;
    };

    struct ExprStmt : Statement
    {
        ExprPtr expr;

        ExprStmt(ExprPtr expr) : expr(std::move(expr)) {}
    };

    struct BlockStmt : Statement
    {
        std::vector<StmtPtr> statements;
    };

    struct VarDeclStmt : Statement
    {
        bool is_const;
        std::string name;
        TypePtr type;
        ExprPtr init_expr;

        VarDeclStmt() : is_const(false) {}

        friend void swap(VarDeclStmt &a, VarDeclStmt &b) noexcept;
        VarDeclStmt &operator=(VarDeclStmt other) noexcept;
        VarDeclStmt(VarDeclStmt &&other) noexcept = default;
        VarDeclStmt(const VarDeclStmt &other);
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

    struct TypedField
    {
        TypePtr type;
        std::string name;

        TypedField(TypePtr type, std::string name);
        TypedField(const TypedField &other);
        TypedField(TypedField &&other) noexcept = default;
        TypedField &operator=(TypedField other) noexcept;

        friend void swap(TypedField &a, TypedField &b) noexcept;
    };

    class StructDecl : public Statement
    {
    public:
        std::string name;
        std::vector<TypedField> fields;
        std::unordered_map<std::string_view, size_t> field_map; // Map for quick field lookup

        StructDecl() = default;
        StructDecl(std::string name, std::vector<TypedField> fields);

        // Add a field to the struct
        void add_field(TypePtr type, std::string name);
        void add_field(TypedField field);

        // Get a field by name
        const TypedField *get_field(std::string_view name) const;
    };

    struct FunctionDecl
    {
        std::string name;
        TypePtr return_type = nullptr;
        std::vector<TypedField> params;
        std::vector<StmtPtr> body;

        bool is_method = false;
        TypePtr receiver_type = nullptr;
        ;

        void add_param(const std::string &name, TypePtr type);
        Type &get_param_type(const std::string &name);
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
        GlobalDecl(const VarDeclStmt &varDeclStmt);
        GlobalDecl(VarDeclStmt &&varDeclStmt, bool exported = false);
    };
};
