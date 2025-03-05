#pragma once

#include <vector>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <string>

#include "ast.h"
#include "ast_scope.h"

class TypeChecker
{
public:
    struct TypeCheckResult
    {
        bool ok;
        std::vector<std::string> errors;
    };

    explicit TypeChecker(ast::Program *program = nullptr);
    void set_program(ast::Program *program) { program_ = program; }
    TypeCheckResult check();

protected:
    ast::Program *program_;
    mutable std::vector<std::string> errors_;
    std::unique_ptr<ast::Scope> global_scope_;
    ast::Scope *current_scope_ = nullptr;
    ast::Type *current_return_type_ = nullptr;
    int loop_depth_ = 0; // Track nested loop depth

    void push_scope();
    void pop_scope();
    template <typename... Args>
    void add_error(const Args &...args) const
    {
        std::stringstream ss;
        (ss << ... << args); // Fold expression to handle multiple arguments
        errors_.push_back(ss.str());
    }

    // Type system helpers
    bool types_equal(const ast::Type &t1, const ast::Type &t2) const;
    bool is_convertible(const ast::Type &from, const ast::Type &to) const;
    bool verify_assignable(const ast::Expr &target, const ast::Expr &value);
    ast::TypePtr resolve_alias(const ast::Type &type) const;
    ast::StructDecl *find_struct(const std::string &name) const;
    bool is_valid_lvalue(ast::Expr &expr);
    bool is_valid_cond_type(const ast::Type &type);

    // AST visitors
    void check_expr(ast::Expr &expr);
    void check_expr_safe(ast::Expr &expr);
    void check_stmt(ast::Statement &stmt);

    void visit(ast::FunctionDecl &decl);
    void visit(ast::StructDecl &decl);
    void visit(ast::ImplBlock &impl);
    void visit(ast::GlobalDecl &decl);
    void visit(ast::TypeAliasDecl &decl);
    // Expression visitors
    void visit(ast::VariableExpr &expr);
    void visit(ast::IntegerLiteralExpr &expr);
    void visit(ast::FloatLiteralExpr &expr);
    void visit(ast::StringLiteralExpr &expr);
    void visit(ast::StructLiteralExpr &expr);
    void visit(ast::FunctionPointerExpr &expr);
    void visit(ast::BinaryExpr &expr);
    void visit(ast::UnaryExpr &expr);
    void visit(ast::CallExpr &expr);
    void visit(ast::MemberAccessExpr &expr);
    void visit(ast::ArrayAccessExpr &expr);
    void visit(ast::InitListExpr &expr);
    void visit(ast::TupleExpr &expr);
    void visit(ast::SizeofExpr &expr);
    void visit(ast::AddressOfExpr &expr);
    void visit(ast::DerefExpr &expr);
    void visit(ast::CastExpr &expr);

    // Statement visitors
    void visit(ast::ExprStmt &stmt);
    void visit(ast::VarDeclStmt &stmt);
    void visit(ast::ReturnStmt &stmt);
    void visit(ast::BlockStmt &stmt);
    void visit(ast::IfStmt &stmt);
    void visit(ast::WhileStmt &stmt);
    void visit(ast::BreakStmt &stmt);
    void visit(ast::ContinueStmt &stmt);
    template <typename T>
    void visit_base(T &node)
    {
        if constexpr (std::is_base_of_v<ast::Expr, T>)
            check_expr(node);
        else if constexpr (std::is_base_of_v<ast::Statement, T>)
            check_stmt(node);
    }
};

class TypeCheckAbort : public std::exception
{
    const char *what() const noexcept override
    {
        return "Type checking aborted";
    }
};
