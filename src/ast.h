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
            Alias
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
        BasicKind basic_kind;

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

        static void swap(Type &a, Type &b) noexcept;

        TypePtr clone() const;

        bool is_aggregate() const
        {
            return kind == Kind::Struct || kind == Kind::Array;
        }

        static Type get_void_type();
        static Type get_int_type();
        static Type get_float_type();
        static Type get_string_type();
        static Type get_struct_type(const std::string &name, std::vector<ast::TypedField> fields);
        static Type get_pointer_type(std::unique_ptr<ast::Type> pointee);
        static Type get_array_type(std::unique_ptr<Type> element_type, int size);
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
    };

    struct VariableExpr : Expr
    {
        std::string name;
        VariableExpr(std::string name) : name(std::move(name)) {}
    };

    struct LiteralExpr : Expr
    {
        virtual ~LiteralExpr() = default;
    };

    struct IntegerLiteralExpr : LiteralExpr
    {
        int value;
        IntegerLiteralExpr(int value) : value(value) {}
    };

    struct FloatLiteralExpr : LiteralExpr
    {
        float value;
        FloatLiteralExpr(float value) : value(value) {}
    };

    struct StringLiteralExpr : LiteralExpr
    {
        std::string value;
        StringLiteralExpr(std::string value) : value(std::move(value)) {}
    };

    struct StructLiteralExpr : LiteralExpr
    {
        std::string struct_name; // Name of the struct (nullable)
        std::vector<std::pair<std::string, ExprPtr>> members;
        std::unordered_map<std::string, size_t> member_map;

        void add_member(std::string name, ExprPtr expr);
        Expr *get_member(const std::string &name) const;
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

        CallExpr() = default;
        CallExpr(ExprPtr callee, std::vector<ExprPtr> args)
            : callee(std::move(callee)), args(std::move(args)) {}
        CallExpr(std::string callee_name, std::vector<ExprPtr> args)
            : callee(std::make_unique<VariableExpr>(callee_name)), args(std::move(args)) {}
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
    };

    struct ArrayAccessExpr : Expr
    {
        ExprPtr array;
        ExprPtr index;

        ArrayAccessExpr() = default;
        ArrayAccessExpr(ExprPtr array, ExprPtr index)
            : array(std::move(array)), index(std::move(index)) {}
    };

    struct CastExpr : Expr
    {
        TypePtr target_type;
        ExprPtr expr;
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

        SizeofExpr(Kind k) : kind(k)
        {
            if (kind == Kind::Type)
            {
                new (&target_type) TypePtr();
            }
            else if (kind == Kind::Expr)
            {
                new (&target_expr) ExprPtr();
            }
        }

        SizeofExpr(TypePtr type) : kind(Kind::Type)
        {
            new (&target_type) TypePtr(std::move(type));
        }

        SizeofExpr(ExprPtr expr) : kind(Kind::Expr)
        {
            new (&target_expr) ExprPtr(std::move(expr));
        }

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

        SizeofExpr(const SizeofExpr &) = delete;
        SizeofExpr &operator=(const SizeofExpr &) = delete;

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

        static void swap(VarDeclStmt &a, VarDeclStmt &b) noexcept;
        VarDeclStmt &operator=(VarDeclStmt other) noexcept;
        VarDeclStmt(VarDeclStmt &&other) noexcept;
        VarDeclStmt(const VarDeclStmt &other) = delete;
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

        static void swap(TypedField &a, TypedField &b) noexcept;
    };

    class StructDecl : public Statement
    {
    public:
        std::string name;
        std::vector<TypedField> fields;
        std::vector<FunctionDecl> methods;
        std::unordered_map<std::string_view, size_t> field_map; // Map for quick field lookup
        std::unordered_map<std::string_view, size_t> method_map; // Map for quick method lookup

        StructDecl() = default;
        StructDecl(std::string name, std::vector<TypedField> fields);

        // Add a field to the struct
        void add_field(TypePtr type, std::string name);
        void add_field(TypedField field);

        // Get a field by name
        const TypedField *get_field(std::string_view name) const;

        // Add a method to the struct
        void add_method(FunctionDecl method);

        // Get a method by name
        FunctionDecl *get_method(std::string_view name) const;

        // Get the type of the struct
        TypePtr type() const
        {
            return std::make_unique<Type>(Type::get_struct_type(name, fields));
        }
    };

    struct FunctionDecl
    {
        std::string name;
        TypePtr return_type = nullptr;
        std::vector<TypedField> params;
        std::vector<StmtPtr> body;

        // method
        bool is_method = false;
        bool is_static = false;
        TypePtr receiver_type = nullptr;

        void add_param(const std::string &name, TypePtr type);
        Type &get_param_type(const std::string &name);
        TypePtr type() const;

        static FunctionDecl create_main_function();
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
