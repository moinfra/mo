#pragma once

#include <string>
#include "ast.h"

class ASTPrinter {
public:
    std::string print(const ast::Program &program);
    std::string print(const ast::Expr &expr);
    std::string print(const ast::Statement &stmt);
    std::string print(const ast::Type &type);
    std::string print(const ast::StructDecl &struct_decl);
    std::string print(const ast::FunctionDecl &func_decl);
    std::string print(const ast::ImplBlock &impl_block);
    std::string print(const ast::VarDeclStmt &var_decl);
    std::string print(const ast::GlobalDecl &global_decl);

private:
    // Expression visitors
    std::string print(const ast::VariableExpr &expr);
    std::string print(const ast::IntegerLiteralExpr &expr);
    std::string print(const ast::FloatLiteralExpr &expr);
    std::string print(const ast::StringLiteralExpr &expr);
    std::string print(const ast::BinaryExpr &expr);
    std::string print(const ast::UnaryExpr &expr);
    std::string print(const ast::CallExpr &expr);
    std::string print(const ast::MemberAccessExpr &expr);
    std::string print(const ast::CastExpr &expr);
    std::string print(const ast::SizeofExpr &expr);
    std::string print(const ast::AddressOfExpr &expr);
    std::string print(const ast::DerefExpr &expr);
    std::string print(const ast::InitListExpr &expr);
    std::string print(const ast::FunctionPointerExpr &expr);
    std::string print(const ast::StructLiteralExpr &expr);

    // Statement visitors
    std::string print(const ast::BlockStmt &stmt);
    std::string print(const ast::ReturnStmt &stmt);
    std::string print(const ast::IfStmt &stmt);
    std::string print(const ast::WhileStmt &stmt);
    std::string print(const ast::BreakStmt &stmt);
    std::string print(const ast::ContinueStmt &stmt);
    std::string print(const ast::ExprStmt &stmt);

    // Helpers
    std::string indent() const;
    void enter_scope();
    void leave_scope();
    std::string escape_string(const std::string &str) const;
    std::string print_type(const ast::Type &type);
    std::string print_expr(const ast::Expr &expr);
    std::string print_stmt(const ast::Statement &stmt);
    int current_level = 0;
    int indent_size = 2;
};
