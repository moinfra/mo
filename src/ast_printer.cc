#include "ast_printer.h"
#include <sstream>
#include <algorithm>

using namespace std;

string ASTPrinter::print(const Program &program)
{
    string result;
    for (const auto &s : program.structs)
    {
        result += print(*s) + "\n\n";
    }
    for (const auto &impl : program.impl_blocks)
    {
        result += print(*impl) + "\n\n";
    }
    for (const auto &func : program.functions)
    {
        result += print(*func) + "\n\n";
    }
    for (const auto &global : program.globals)
    {
        result += print(*global) + "\n";
    }
    return result;
}

string ASTPrinter::print(const Expr &expr)
{
    if (auto p = dynamic_cast<const VariableExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const IntegerLiteralExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const FloatLiteralExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const StringLiteralExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const BinaryExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const UnaryExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const CallExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const MemberAccessExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const CastExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const SizeofExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const InitListExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const FunctionPointerExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const StructLiteralExpr *>(&expr))
        return visit(*p);
    return "unknown_expr";
}

string ASTPrinter::print(const Statement &stmt)
{
    if (auto p = dynamic_cast<const BlockStmt *>(&stmt))
        return visit(*p);
    if (auto p = dynamic_cast<const VarDeclStmt *>(&stmt))
        return print(*p);
    if (auto p = dynamic_cast<const GlobalDecl *>(&stmt))
        return print(*p);
    if (auto p = dynamic_cast<const ReturnStmt *>(&stmt))
        return visit(*p);
    if (auto p = dynamic_cast<const IfStmt *>(&stmt))
        return visit(*p);
    if (auto p = dynamic_cast<const WhileStmt *>(&stmt))
        return visit(*p);
    if (auto p = dynamic_cast<const BreakStmt *>(&stmt))
        return visit(*p);
    if (auto p = dynamic_cast<const ContinueStmt *>(&stmt))
        return visit(*p);
    if (auto p = dynamic_cast<const StructDecl *>(&stmt))
        return print(*p);
    if (auto p = dynamic_cast<const ExprStmt *>(&stmt))
        return visit(*p);
    return "unknown_statement";
}

string ASTPrinter::print(const Type &type)
{
    string result;
    if (type.is_const)
    {
        result += "const ";
    }
    switch (type.kind)
    {
    case Type::Kind::Basic:
        result += type.name;
        break;
    case Type::Kind::Pointer:
        result += "*" + print(*type.pointee);
        break;
    case Type::Kind::Array:
        result += "array<" + print(*type.element_type) + ", " + to_string(type.array_size) + ">";
        break;
    case Type::Kind::Function:
    {
        result += "func(";
        for (size_t i = 0; i < type.params.size(); ++i)
        {
            result += print(*type.params[i]);
            if (i != type.params.size() - 1)
            {
                result += ", ";
            }
        }
        result += ") -> " + print(*type.return_type);
        break;
    }
    case Type::Kind::Struct:
        result += "struct " + type.name;
        break;
    case Type::Kind::Alias:
        result += type.name;
        break;
    case Type::Kind::Void:
        result += "void";
        break;
    default:
        result += "unknown_type";
        break;
    }
    return result;
}

string ASTPrinter::print(const StructDecl &struct_decl)
{
    string result = "struct " + struct_decl.name + " {\n";
    enter_scope();
    for (const auto &field : struct_decl.fields)
    {
        result += indent() + print(*field.type) + " " + field.name + ";\n";
    }
    leave_scope();
    result += indent() + "}";
    return result;
}

string ASTPrinter::print(const FunctionDecl &func_decl)
{
    string result = (func_decl.is_method ? "method " : "fn ") + func_decl.name + "(";
    for (size_t i = 0; i < func_decl.params.size(); ++i)
    {
        const auto &param = func_decl.params[i];
        result += param.name + ": " + print(*param.type);
        if (i != func_decl.params.size() - 1)
        {
            result += ", ";
        }
    }
    result += ") -> " + print(*func_decl.return_type) + " {\n";
    enter_scope();
    for (const auto &stmt : func_decl.body)
    {
        result += indent() + print(*stmt) + "\n";
    }
    leave_scope();
    result += indent() + "}";
    return result;
}

string ASTPrinter::print(const ImplBlock &impl_block)
{
    string result = "impl " + print(*impl_block.target_type) + " {\n";
    enter_scope();
    for (const auto &method : impl_block.methods)
    {
        result += indent() + print(*method) + "\n";
    }
    leave_scope();
    result += indent() + "}";
    return result;
}

string ASTPrinter::print(const VarDeclStmt &var_decl)
{
    string result = (var_decl.is_const ? "const " : "let ") + var_decl.name;
    if (var_decl.type)
    {
        result += ": " + print(*var_decl.type);
    }
    if (var_decl.init_expr)
    {
        result += " = " + print(*var_decl.init_expr);
    }
    result += ";";
    return result;
}

string ASTPrinter::print(const GlobalDecl &global_decl)
{
    string result = (global_decl.is_exported ? "export " : "") + print(static_cast<const VarDeclStmt &>(global_decl));
    return result;
}

// Expressions
string ASTPrinter::visit(const VariableExpr &expr)
{
    return expr.name;
}

string ASTPrinter::visit(const IntegerLiteralExpr &expr)
{
    return to_string(expr.value);
}

string ASTPrinter::visit(const FloatLiteralExpr &expr)
{
    ostringstream oss;
    oss << expr.value;
    return oss.str();
}

string ASTPrinter::visit(const StringLiteralExpr &expr)
{
    return "\"" + expr.value + "\"";
}

string ASTPrinter::visit(const BinaryExpr &expr)
{
    return "(" + print(*expr.left) + " " + token_type_to_string(expr.op) + " " + print(*expr.right) + ")";
}

string ASTPrinter::visit(const UnaryExpr &expr)
{
    return "(" + token_type_to_string(expr.op) + print(*expr.operand) + ")";
}

string ASTPrinter::visit(const CallExpr &expr)
{
    string result = print(*expr.callee) + "(";
    for (size_t i = 0; i < expr.args.size(); ++i)
    {
        result += print(*expr.args[i]);
        if (i != expr.args.size() - 1)
        {
            result += ", ";
        }
    }
    result += ")";
    return result;
}

string ASTPrinter::visit(const MemberAccessExpr &expr)
{
    return print(*expr.object) + token_type_to_string(expr.accessor) + expr.member;
}

string ASTPrinter::visit(const CastExpr &expr)
{
    return "cast<" + print(*expr.target_type) + ">(" + print(*expr.expr) + ")";
}

string ASTPrinter::visit(const SizeofExpr &expr)
{
    return "sizeof(" + print(*expr.target_type) + ")";
}

string ASTPrinter::visit(const InitListExpr &expr)
{
    string result = "{";
    for (size_t i = 0; i < expr.members.size(); ++i)
    {
        result += print(*expr.members[i]);
        if (i != expr.members.size() - 1)
        {
            result += ", ";
        }
    }
    result += "}";
    return result;
}

string ASTPrinter::visit(const FunctionPointerExpr &expr)
{
    string result = "funcptr(";
    for (size_t i = 0; i < expr.param_types.size(); ++i)
    {
        result += print(*expr.param_types[i]);
        if (i != expr.param_types.size() - 1)
        {
            result += ", ";
        }
    }
    result += ") -> " + print(*expr.return_type);
    return result;
}

string ASTPrinter::visit(const StructLiteralExpr &expr)
{
    string struct_name = expr.struct_name.empty() ? "anonymous" : expr.struct_name;
    string result = struct_name + " {";
    for (size_t i = 0; i < expr.members.size(); ++i)
    {
        const auto &member = expr.members[i];
        result += "." + member.first + " = " + print(*member.second);
        if (i != expr.members.size() - 1)
        {
            result += ", ";
        }
    }
    result += "}";
    return result;
}

// Statements
string ASTPrinter::visit(const BlockStmt &stmt)
{
    string result = "{\n";
    enter_scope();
    for (const auto &s : stmt.statements)
    {
        result += indent() + print(*s) + "\n";
    }
    leave_scope();
    result += indent() + "}";
    return result;
}

string ASTPrinter::visit(const ReturnStmt &stmt)
{
    return stmt.value ? "return " + print(*stmt.value) + ";" : "return;";
}

string ASTPrinter::visit(const IfStmt &stmt)
{
    string result = "if (" + print(*stmt.condition) + ") " + print(*stmt.then_branch);
    if (stmt.else_branch)
    {
        result += " else " + print(*stmt.else_branch);
    }
    return result;
}

string ASTPrinter::visit(const WhileStmt &stmt)
{
    return "while (" + print(*stmt.condition) + ") " + print(*stmt.body);
}

string ASTPrinter::visit(const BreakStmt &stmt)
{
    return "break;";
}

string ASTPrinter::visit(const ContinueStmt &stmt)
{
    return "continue;";
}

string ASTPrinter::visit(const ExprStmt &stmt)
{
    return print(*stmt.expr) + ";";
}

string ASTPrinter::indent() const
{
    return string(current_indent, ' ');
}

void ASTPrinter::enter_scope()
{
    current_indent += 2;
}

void ASTPrinter::leave_scope()
{
    current_indent = max(0, current_indent - 2);
}
