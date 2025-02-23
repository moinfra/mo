#include "ast_printer_json.h"

#include <sstream>
#include <algorithm>
#include <string>
#include <vector>

std::string ASTPrinter::print(const Program &program)
{
    std::vector<std::string> structs;
    for (const auto &s : program.structs)
    {
        enter_scope();
        structs.push_back(print(*s));
        leave_scope();
    }

    std::vector<std::string> impl_blocks;
    for (const auto &impl : program.impl_blocks)
    {
        enter_scope();
        impl_blocks.push_back(print(*impl));
        leave_scope();
    }

    std::vector<std::string> functions;
    for (const auto &func : program.functions)
    {
        enter_scope();
        functions.push_back(print(*func));
        leave_scope();
    }

    std::vector<std::string> globals;
    for (const auto &global : program.globals)
    {
        enter_scope();
        globals.push_back(print(*global));
        leave_scope();
    }

    std::vector<std::string> program_parts;
    program_parts.push_back(json_pair("structs", json_array(structs, current_indent + 1)));
    program_parts.push_back(json_pair("impl_blocks", json_array(impl_blocks, current_indent + 1)));
    program_parts.push_back(json_pair("functions", json_array(functions, current_indent + 1)));
    program_parts.push_back(json_pair("globals", json_array(globals, current_indent + 1)));

    std::string result = "{\n";
    enter_scope();
    for (size_t i = 0; i < program_parts.size(); ++i)
    {
        result += indent_current() + program_parts[i];
        if (i != program_parts.size() - 1)
        {
            result += ",";
        }
        result += "\n";
    }
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::print(const Expr &expr)
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
    return "\"unknown_expr\"";
}

std::string ASTPrinter::print(const Statement &stmt)
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
    return "\"unknown_statement\"";
}

std::string ASTPrinter::print(const Type &type)
{
    std::vector<std::string> type_parts;
    type_parts.push_back(json_pair("is_const", type.is_const ? "\"true\"" : "\"false\""));

    std::string kind_str;
    switch (type.kind)
    {
    case Type::Kind::Basic:
        kind_str = "\"Basic\"";
        type_parts.push_back(json_pair("name", "\"" + escape_json_string(type.name) + "\""));
        break;
    case Type::Kind::Pointer:
        kind_str = "\"Pointer\"";
        type_parts.push_back(json_pair("pointee", print(*type.pointee)));
        break;
    case Type::Kind::Array:
        kind_str = "\"Array\"";
        type_parts.push_back(json_pair("element_type", print(*type.element_type)));
        type_parts.push_back(json_pair("array_size", std::to_string(type.array_size)));
        break;
    case Type::Kind::Function:
    {
        kind_str = "\"Function\"";
        std::vector<std::string> params;
        for (const auto &param : type.params)
        {
            params.push_back(print(*param));
        }
        type_parts.push_back(json_pair("params", json_array(params, current_indent + 1)));
        type_parts.push_back(json_pair("return_type", print(*type.return_type)));
        break;
    }
    case Type::Kind::Struct:
        kind_str = "\"Struct\"";
        type_parts.push_back(json_pair("name", "\"" + escape_json_string(type.name) + "\""));
        break;
    case Type::Kind::Alias:
        kind_str = "\"Alias\"";
        break;
    case Type::Kind::Void:
        kind_str = "\"Void\"";
        break;
    default:
        kind_str = "\"unknown_type\"";
        break;
    }
    type_parts.push_back(json_pair("kind", kind_str));

    std::string result = "{\n";
    enter_scope();
    for (size_t i = 0; i < type_parts.size(); ++i)
    {
        result += indent(current_indent + 1) + type_parts[i];
        if (i != type_parts.size() - 1)
        {
            result += ",";
        }
        result += "\n";
    }
    result += indent_current() + "}";
    leave_scope();
    return result;
}

std::string ASTPrinter::print(const StructDecl &struct_decl)
{
    std::vector<std::string> fields;
    enter_scope();
    for (const auto &field : struct_decl.fields)
    {
        std::vector<std::string> field_parts;
        field_parts.push_back(json_pair("name", "\"" + escape_json_string(field.name) + "\""));
        field_parts.push_back(json_pair("type", print(*field.type)));

        std::string field_json = "{\n";
        enter_scope();
        for (size_t i = 0; i < field_parts.size(); ++i)
        {
            field_json += indent_current() + field_parts[i];
            if (i != field_parts.size() - 1)
            {
                field_json += ",";
            }
            field_json += "\n";
        }
        leave_scope();
        field_json += indent_current() + "}";
        fields.push_back(field_json);
    }
    leave_scope();

    std::vector<std::string> struct_parts;
    struct_parts.push_back(json_pair("name", "\"" + escape_json_string(struct_decl.name) + "\""));
    struct_parts.push_back(json_pair("fields", json_array(fields, current_indent + 1)));

    std::string result = "{\n";
    enter_scope();
    for (size_t i = 0; i < struct_parts.size(); ++i)
    {
        result += indent_current() + struct_parts[i];
        if (i != struct_parts.size() - 1)
        {
            result += ",";
        }
        result += "\n";
    }
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::print(const FunctionDecl &func_decl)
{
    std::vector<std::string> params;
    for (const auto &param : func_decl.params)
    {
        std::vector<std::string> param_parts;
        param_parts.push_back(json_pair("name", "\"" + escape_json_string(param.name) + "\""));
        param_parts.push_back(json_pair("type", print(*param.type)));

        std::string param_json = "{\n";
        enter_scope();
        for (size_t i = 0; i < param_parts.size(); ++i)
        {
            param_json += indent_current() + param_parts[i];
            if (i != param_parts.size() - 1)
            {
                param_json += ",";
            }
            param_json += "\n";
        }
        leave_scope();
        param_json += indent_current() + "}";
        params.push_back(param_json);
    }

    std::vector<std::string> body;
    enter_scope();
    for (const auto &stmt : func_decl.body)
    {
        body.push_back(print(*stmt));
    }
    leave_scope();
    
    std::vector<std::string> func_parts;
    func_parts.push_back(json_pair("is_method", func_decl.is_method ? "\"true\"" : "\"false\""));
    func_parts.push_back(json_pair("name", "\"" + escape_json_string(func_decl.name) + "\""));
    func_parts.push_back(json_pair("params", json_array(params, current_indent + 1)));
    func_parts.push_back(json_pair("return_type", print(*func_decl.return_type)));
    func_parts.push_back(json_pair("body", json_array(body, current_indent + 1)));

    std::string result = "{\n";
    enter_scope();
    for (size_t i = 0; i < func_parts.size(); ++i)
    {
        result += indent_current() + func_parts[i];
        if (i != func_parts.size() - 1)
        {
            result += ",";
        }
        result += "\n";
    }
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::print(const ImplBlock &impl_block)
{
    std::vector<std::string> methods;
    enter_scope();
    for (const auto &method : impl_block.methods)
    {
        methods.push_back(print(*method));
    }
    leave_scope();

    std::vector<std::string> impl_parts;
    impl_parts.push_back(json_pair("target_type", print(*impl_block.target_type)));
    impl_parts.push_back(json_pair("methods", json_array(methods, current_indent + 1)));

    std::string result = "{\n";
    enter_scope();
    for (size_t i = 0; i < impl_parts.size(); ++i)
    {
        result += indent_current() + impl_parts[i];
        if (i != impl_parts.size() - 1)
        {
            result += ",";
        }
        result += "\n";
    }
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::print(const VarDeclStmt &var_decl)
{
    std::vector<std::string> var_parts;
    var_parts.push_back(json_pair("is_const", var_decl.is_const ? "\"true\"" : "\"false\""));
    var_parts.push_back(json_pair("name", "\"" + escape_json_string(var_decl.name) + "\""));
    if (var_decl.type)
    {
        var_parts.push_back(json_pair("type", print(*var_decl.type)));
    }
    if (var_decl.init_expr)
    {
        var_parts.push_back(json_pair("init_expr", print(*var_decl.init_expr)));
    }

    std::string result = "{\n";
    enter_scope();
    for (size_t i = 0; i < var_parts.size(); ++i)
    {
        result += indent_current() + var_parts[i];
        if (i != var_parts.size() - 1)
        {
            result += ",";
        }
        result += "\n";
    }
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::print(const GlobalDecl &global_decl)
{
    std::vector<std::string> global_parts;
    global_parts.push_back(json_pair("is_exported", global_decl.is_exported ? "\"true\"" : "\"false\""));
    std::string var_decl_json = print(static_cast<const VarDeclStmt &>(global_decl));
    global_parts.push_back(json_pair("var_decl", var_decl_json));

    std::string result = "{\n";
    enter_scope();
    for (size_t i = 0; i < global_parts.size(); ++i)
    {
        result += indent_current() + global_parts[i];
        if (i != global_parts.size() - 1)
        {
            result += ",";
        }
        result += "\n";
    }
    leave_scope();
    result += indent_current() + "}";
    return result;
}

// Expressions
std::string ASTPrinter::visit(const VariableExpr &expr)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"VariableExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("name", "\"" + escape_json_string(expr.name) + "\"", current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const IntegerLiteralExpr &expr)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"IntegerLiteralExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("value", std::to_string(expr.value), current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const FloatLiteralExpr &expr)
{
    std::ostringstream oss;
    oss << expr.value;
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"FloatLiteralExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("value", "\"" + escape_json_string(oss.str()) + "\"", current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const StringLiteralExpr &expr)
{
    std::string result = "{\n";
    enter_scope();
    enter_scope();
    result += indent_current() + json_pair("type", "\"StringLiteralExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("value", "\"" + escape_json_string(expr.value) + "\"", current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    leave_scope();
    return result;
}

std::string ASTPrinter::visit(const BinaryExpr &expr)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"BinaryExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("op", "\"" + escape_json_string(token_type_to_string(expr.op)) + "\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("left", print(*expr.left), current_indent + 1) + ",\n";
    result += indent_current() + json_pair("right", print(*expr.right), current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const UnaryExpr &expr)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"UnaryExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("op", "\"" + escape_json_string(token_type_to_string(expr.op)) + "\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("operand", print(*expr.operand), current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const CallExpr &expr)
{
    std::vector<std::string> args;
    for (const auto &arg : expr.args)
    {
        args.push_back(print(*arg));
    }

    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"CallExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("callee", print(*expr.callee), current_indent + 1) + ",\n";
    result += indent_current() + json_pair("args", json_array(args, current_indent)) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const MemberAccessExpr &expr)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"MemberAccessExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("object", print(*expr.object), current_indent + 1) + ",\n";
    result += indent_current() + json_pair("accessor", "\"" + escape_json_string(token_type_to_string(expr.accessor)) + "\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("member", "\"" + escape_json_string(expr.member) + "\"", current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const CastExpr &expr)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"CastExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("target_type", print(*expr.target_type), current_indent + 1) + ",\n";
    result += indent_current() + json_pair("expr", print(*expr.expr), current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const SizeofExpr &expr)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"SizeofExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("target_type", print(*expr.target_type), current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const InitListExpr &expr)
{
    std::vector<std::string> members;
    for (const auto &member : expr.members)
    {
        members.push_back(print(*member));
    }

    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"InitListExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("members", json_array(members, current_indent + 1)) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const FunctionPointerExpr &expr)
{
    std::vector<std::string> param_types;
    for (const auto &param_type : expr.param_types)
    {
        param_types.push_back(print(*param_type));
    }

    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"FunctionPointerExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("param_types", json_array(param_types, current_indent + 1)) + ",\n";
    result += indent_current() + json_pair("return_type", print(*expr.return_type), current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const StructLiteralExpr &expr)
{
    std::vector<std::string> members;
    for (const auto &member : expr.members)
    {
        std::vector<std::string> member_parts;
        member_parts.push_back(json_pair("name", "\"" + escape_json_string(member.first) + "\""));
        member_parts.push_back(json_pair("value", print(*member.second)));

        enter_scope();
        std::string member_json = "{\n";
        for (size_t i = 0; i < member_parts.size(); ++i)
        {
            member_json += indent_current() + member_parts[i];
            if (i != member_parts.size() - 1)
            {
                member_json += ",";
            }
            member_json += "\n";
        }
        leave_scope();
        member_json += indent_current() + "}";
        members.push_back(member_json);
    }

    std::string struct_name = expr.struct_name.empty() ? "anonymous" : expr.struct_name;
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"StructLiteralExpr\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("struct_name", "\"" + escape_json_string(struct_name) + "\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("members", json_array(members, current_indent + 1)) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

// Statements
std::string ASTPrinter::visit(const BlockStmt &stmt)
{
    std::vector<std::string> statements;
    enter_scope();
    for (const auto &s : stmt.statements)
    {
        statements.push_back(print(*s));
    }
    leave_scope();

    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"BlockStmt\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("statements", json_array(statements, current_indent + 1)) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const ReturnStmt &stmt)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"ReturnStmt\"", current_indent + 1) + ",\n";
    if (stmt.value)
    {
        result += indent_current() + json_pair("value", print(*stmt.value), current_indent + 1) + "\n";
    }
    else
    {
        result += indent_current() + json_pair("value", "null", current_indent + 1) + "\n";
    }
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const IfStmt &stmt)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"IfStmt\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("condition", print(*stmt.condition), current_indent + 1) + ",\n";
    result += indent_current() + json_pair("then_branch", print(*stmt.then_branch), current_indent + 1) + ",\n";
    if (stmt.else_branch)
    {
        result += indent_current() + json_pair("else_branch", print(*stmt.else_branch), current_indent + 1) + "\n";
    }
    else
    {
        result += indent_current() + json_pair("else_branch", "null", current_indent + 1) + "\n";
    }
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const WhileStmt &stmt)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"WhileStmt\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("condition", print(*stmt.condition), current_indent + 1) + ",\n";
    result += indent_current() + json_pair("body", print(*stmt.body), current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const BreakStmt &stmt)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"BreakStmt\"", current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const ContinueStmt &stmt)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"ContinueStmt\"", current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::visit(const ExprStmt &stmt)
{
    std::string result = "{\n";
    enter_scope();
    result += indent_current() + json_pair("type", "\"ExprStmt\"", current_indent + 1) + ",\n";
    result += indent_current() + json_pair("expr", print(*stmt.expr), current_indent + 1) + "\n";
    leave_scope();
    result += indent_current() + "}";
    return result;
}

std::string ASTPrinter::indent(int level) const
{
    return std::string(level * 2, ' ');
}

void ASTPrinter::enter_scope()
{
    current_indent += 1;
}

void ASTPrinter::leave_scope()
{
    current_indent = std::max(0, current_indent - 1);
}
