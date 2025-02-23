#pragma once

#include <string>
#include "ast.h"

class ASTPrinter
{
public:
    std::string print(const Program &program);
    std::string print(const Expr &expr);
    std::string print(const Statement &stmt);
    std::string print(const Type &type);
    std::string print(const StructDecl &struct_decl);
    std::string print(const FunctionDecl &func_decl);
    std::string print(const ImplBlock &impl_block);
    std::string print(const VarDeclStmt &var_decl);
    std::string print(const GlobalDecl &global_decl);

private:
    std::string visit(const VariableExpr &expr);
    std::string visit(const IntegerLiteralExpr &expr);
    std::string visit(const FloatLiteralExpr &expr);
    std::string visit(const StringLiteralExpr &expr);
    std::string visit(const BinaryExpr &expr);
    std::string visit(const UnaryExpr &expr);
    std::string visit(const CallExpr &expr);
    std::string visit(const MemberAccessExpr &expr);
    std::string visit(const CastExpr &expr);
    std::string visit(const SizeofExpr &expr);
    std::string visit(const InitListExpr &expr);
    std::string visit(const FunctionPointerExpr &expr);
    std::string visit(const StructLiteralExpr &expr);

    std::string visit(const BlockStmt &stmt);
    std::string visit(const ReturnStmt &stmt);
    std::string visit(const IfStmt &stmt);
    std::string visit(const WhileStmt &stmt);
    std::string visit(const BreakStmt &stmt);
    std::string visit(const ContinueStmt &stmt);
    std::string visit(const ExprStmt &stmt);
    std::string indent() const;
    void enter_scope();
    void leave_scope();

    int current_indent = 0;
};
