#pragma once

#include <string>
#include "ast.h"

class ASTPrinter
{
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
    std::string visit(const ast::VariableExpr &expr);
    std::string visit(const ast::IntegerLiteralExpr &expr);
    std::string visit(const ast::FloatLiteralExpr &expr);
    std::string visit(const ast::StringLiteralExpr &expr);
    std::string visit(const ast::BinaryExpr &expr);
    std::string visit(const ast::UnaryExpr &expr);
    std::string visit(const ast::CallExpr &expr);
    std::string visit(const ast::MemberAccessExpr &expr);
    std::string visit(const ast::CastExpr &expr);
    std::string visit(const ast::SizeofExpr &expr);
    std::string visit(const ast::AddressOfExpr &expr);
    std::string visit(const ast::DerefExpr &expr);
    std::string visit(const ast::InitListExpr &expr);
    std::string visit(const ast::FunctionPointerExpr &expr);
    std::string visit(const ast::StructLiteralExpr &expr);

    std::string visit(const ast::BlockStmt &stmt);
    std::string visit(const ast::ReturnStmt &stmt);
    std::string visit(const ast::IfStmt &stmt);
    std::string visit(const ast::WhileStmt &stmt);
    std::string visit(const ast::BreakStmt &stmt);
    std::string visit(const ast::ContinueStmt &stmt);
    std::string visit(const ast::ExprStmt &stmt);
    std::string indent() const;
    void enter_scope();
    void leave_scope();

    int current_indent = 0;
};
