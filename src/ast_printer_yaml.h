#pragma once

#include <string>
#include "ast.h"

class ASTPrinter {
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
    // Expression visitors
    std::string print(const VariableExpr &expr);
    std::string print(const IntegerLiteralExpr &expr);
    std::string print(const FloatLiteralExpr &expr);
    std::string print(const StringLiteralExpr &expr);
    std::string print(const BinaryExpr &expr);
    std::string print(const UnaryExpr &expr);
    std::string print(const CallExpr &expr);
    std::string print(const MemberAccessExpr &expr);
    std::string print(const CastExpr &expr);
    std::string print(const SizeofExpr &expr);
    std::string print(const AddressOfExpr &expr);
    std::string print(const InitListExpr &expr);
    std::string print(const FunctionPointerExpr &expr);
    std::string print(const StructLiteralExpr &expr);

    // Statement visitors
    std::string print(const BlockStmt &stmt);
    std::string print(const ReturnStmt &stmt);
    std::string print(const IfStmt &stmt);
    std::string print(const WhileStmt &stmt);
    std::string print(const BreakStmt &stmt);
    std::string print(const ContinueStmt &stmt);
    std::string print(const ExprStmt &stmt);

    // Helpers
    std::string indent() const;
    void enter_scope();
    void leave_scope();
    std::string escape_string(const std::string &str) const;
    std::string print_type(const Type &type);
    std::string print_expr(const Expr &expr);
    std::string print_stmt(const Statement &stmt);
    int current_level = 0;
    int indent_size = 2;
};
