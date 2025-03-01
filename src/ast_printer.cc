#include "ast_printer.h"
#include <sstream>
#include <algorithm>

using namespace std;
using namespace ast;

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
    if (auto p = dynamic_cast<const ArrayAccessExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const CastExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const SizeofExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const AddressOfExpr *>(&expr))
        return visit(*p);
    if (auto p = dynamic_cast<const DerefExpr *>(&expr))
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
    if (auto p = dynamic_cast<const TypeAliasDecl *>(&stmt))
        return print(*p);
    if (auto p = dynamic_cast<const ExprStmt *>(&stmt))
        return visit(*p);
    return "unknown_statement";
}

std::string ASTPrinter::print(const Type& type) {
    if (const QualifiedType* q = type.as_qualified()) {
        std::string qual;
        if (static_cast<uint8_t>(q->qualifiers() & Qualifier::Const)) qual += "const ";
        if (static_cast<uint8_t>(q->qualifiers() & Qualifier::Volatile)) qual += "volatile ";
        return qual + print(q->base_type());
    }

    if (const PointerType* p = type.as_pointer()) {
        return "*" + print(p->pointee());
    }

    if (const ArrayType* a = type.as_array()) {
        std::string size = a->size() >= 0 ? std::to_string(a->size()) : "";
        return print(a->element_type()) + "[" + size + "]";
    }

    if (const FunctionType* f = type.as_function()) {
        std::string params;
        for (size_t i = 0; i < f->params().size(); ++i) {
            params += print(*f->params()[i]);
            if (i != f->params().size() - 1) params += ", ";
        }
        return "func(" + params + ") -> " + print(f->return_type());
    }

    if (const StructType* s = type.as_struct()) {
        std::string members;
        for (size_t i = 0; i < s->member_count(); ++i) {
            const auto& m = s->get_member(i);
            members += m.name + ": " + print(*m.type);
            if (i != s->member_count() - 1) members += "; ";
        }
        return "struct " + s->name() + " { " + members + " }";
    }

    if (const AliasType* a = type.as_alias()) {
        return a->name();
    }

    if (const IntType* i = type.as_int()) {
        auto bw = i->bit_width();
        return "i" + std::to_string(bw);
    }
    if (const FloatType* f = type.as_float()) {
        auto bw = static_cast<uint8_t>(f->precision());
        return "f" + std::to_string(bw); 
    }
    if (const BoolType* b = type.as_bool()) {
        return "bool";
    }
    if (const StringType* s = type.as_string()) {
        return "string";
    }

    if (type.kind() == Type::Kind::Void) {
        return "void";
    }

    return "unknown_type";
}
string ASTPrinter::print(const TypeAliasDecl &type_alias_decl)
{
    string result = "type " + type_alias_decl.name + " = " + print(*type_alias_decl.type) + ";";
    return result;
}


string ASTPrinter::print(const StructDecl &struct_decl)
{
    string result = "struct " + struct_decl.name + " {\n";
    enter_scope();
    for (const auto &field : struct_decl.fields)
    {
        result += indent() + field.name + ": " + print(*field.type) + ",\n";
    }
    leave_scope();
    result += indent() + "}";
    return result;
}

string ASTPrinter::print(const FunctionDecl &func_decl)
{
    string result = "fn " + func_decl.name + "(";
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
    string result = "(" + print(*expr.callee) + "(";
    for (size_t i = 0; i < expr.args.size(); ++i)
    {
        result += print(*expr.args[i]);
        if (i != expr.args.size() - 1)
        {
            result += ", ";
        }
    }
    result += "))";
    return result;
}

string ASTPrinter::visit(const MemberAccessExpr &expr)
{
    return print(*expr.object) + token_type_to_string(expr.accessor) + expr.member;
}

string ASTPrinter::visit(const ArrayAccessExpr &expr)
{
    return print(*expr.array) + "[" + print(*expr.index) + "]";
}

string ASTPrinter::visit(const CastExpr &expr)
{
    return "cast<" + print(*expr.target_type) + ">(" + print(*expr.expr) + ")";
}

string ASTPrinter::visit(const SizeofExpr &expr)
{
    if (expr.kind == SizeofExpr::Kind::Type)
    {
        return "sizeof(" + print(*expr.target_type) + ")";
    }
    else if (expr.kind == SizeofExpr::Kind::Expr)
    {
        return "sizeof(" + print(*expr.target_expr) + ")";
    }
    else
    {
        abort();
    }
}

string ASTPrinter::visit(const AddressOfExpr &expr)
{
    return "&(" + print(*expr.operand) + ")";
}

string ASTPrinter::visit(const DerefExpr &expr)
{
    return "*(" + print(*expr.operand) + ")";
}

string ASTPrinter::visit(const InitListExpr &expr)
{
    string result = "[";
    for (size_t i = 0; i < expr.members.size(); ++i)
    {
        result += print(*expr.members[i]);
        if (i != expr.members.size() - 1)
        {
            result += ", ";
        }
    }
    result += "]";
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
        result += " " + member.first + ": " + print(*member.second);
        if (i != expr.members.size() - 1)
        {
            result += ", ";
        }
    }
    result += " }";
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
