#include "ast_printer_yaml.h"
#include <sstream>
#include <iomanip>
#include <cassert>

using namespace std;
using namespace ast;

string ASTPrinter::escape_string(const string &str) const
{
    ostringstream oss;
    for (char c : str)
    {
        switch (c)
        {
        case '"':
            oss << "\\\"";
            break;
        case '\\':
            oss << "\\\\";
            break;
        case '\b':
            oss << "\\b";
            break;
        case '\f':
            oss << "\\f";
            break;
        case '\n':
            oss << "\\n";
            break;
        case '\r':
            oss << "\\r";
            break;
        case '\t':
            oss << "\\t";
            break;
        default:
            if (c >= 0x00 && c <= 0x1f)
            {
                oss << "\\u" << hex << setw(4) << setfill('0') << static_cast<int>(c);
            }
            else
            {
                oss << c;
            }
        }
    }
    return oss.str();
}

string ASTPrinter::indent() const
{
    return string(current_level * indent_size, ' ');
}

void ASTPrinter::enter_scope() { current_level++; }
void ASTPrinter::leave_scope() { current_level--; }

string ASTPrinter::print(const Program &program)
{
    ostringstream oss;
    oss << "program:\n";
    enter_scope();

    if (!program.structs.empty())
    {
        oss << indent() << "structs:\n";
        enter_scope();
        for (const auto &s : program.structs)
        {
            oss << indent() << "- \n";
            enter_scope();
            oss << print(*s);
            leave_scope();
        }
        leave_scope();
    }

    if (!program.impl_blocks.empty())
    {
        oss << indent() << "impl_blocks:\n";
        enter_scope();
        for (const auto &i : program.impl_blocks)
        {
            oss << indent() << "- \n";
            enter_scope();
            oss << print(*i);
            leave_scope();
        }
        leave_scope();
    }

    if (!program.functions.empty())
    {
        oss << indent() << "functions:\n";
        enter_scope();
        for (const auto &f : program.functions)
        {
            oss << indent() << "- \n";
            enter_scope();
            oss << print(*f);
            leave_scope();
        }
        leave_scope();
    }

    if (!program.globals.empty())
    {
        oss << indent() << "globals:\n";
        enter_scope();
        for (const auto &g : program.globals)
        {
            oss << indent() << "- \n";
            enter_scope();
            oss << print(*g);
            leave_scope();
        }
        leave_scope();
    }

    leave_scope();
    return oss.str();
}

std::string ASTPrinter::print_type(const Type &type)
{
    std::ostringstream oss;
    enter_scope();
    oss << "\n";
    oss << indent() << "kind: ";
    switch (type.kind())
    {
    case Type::Kind::Placeholder:
        oss << "Placeholder";
        break;
    case Type::Kind::Void:
        oss << "Void";
        break;
    case Type::Kind::Int:
    {
        auto &int_type = static_cast<const IntType &>(type);
        oss << "Int\n";
        oss << indent() << "bit_width: " << int_type.bit_width();
        break;
    }
    case Type::Kind::Float:
    {
        auto &float_type = static_cast<const FloatType &>(type);
        oss << "Float\n";
        oss << indent() << "precision: ";
        switch (float_type.precision())
        {
        case FloatType::Precision::Half:
            oss << "Half";
            break;
        case FloatType::Precision::Single:
            oss << "Single";
            break;
        case FloatType::Precision::Double:
            oss << "Double";
            break;
        case FloatType::Precision::Quad:
            oss << "Quad";
            break;
        }
        break;
    }
    case Type::Kind::Bool:
        oss << "Bool";
        break;
    case Type::Kind::String:
        oss << "String";
        break;
    case Type::Kind::Pointer:
    {
        auto &pointer_type = static_cast<const PointerType &>(type);
        oss << "Pointer\n";
        oss << indent() << "pointee:";
        enter_scope();
        oss << print_type(pointer_type.pointee());
        leave_scope();
        break;
    }
    case Type::Kind::Array:
    {
        auto &array_type = static_cast<const ArrayType &>(type);
        oss << "Array\n";
        oss << indent() << "element_type:";
        enter_scope();
        oss << print_type(array_type.element_type()) << "\n";
        leave_scope();
        oss << indent() << "size: " << array_type.size();
        break;
    }
    case Type::Kind::Function:
    {
        auto &function_type = static_cast<const FunctionType &>(type);
        oss << "Function\n";
        oss << indent() << "params:";
        enter_scope();
        for (const auto &p : function_type.params())
        {
            oss << indent() << "- \n";
            enter_scope();
            oss << print_type(*p);
            leave_scope();
        }
        leave_scope();
        oss << indent() << "return_type:";
        enter_scope();
        oss << print_type(function_type.return_type());
        leave_scope();
        break;
    }
    case Type::Kind::Struct:
    {
        auto &struct_type = static_cast<const StructType &>(type);
        oss << "Struct\n";
        oss << indent() << "name: \"" << escape_string(struct_type.name()) << "\"";
        break;
    }
    case Type::Kind::Alias:
    {
        auto &alias_type = static_cast<const AliasType &>(type);
        oss << "Alias\n";
        oss << indent() << "name: \"" << escape_string(alias_type.name()) << "\"";
        break;
    }
    case Type::Kind::Qualified:
    {
        auto &qualified_type = static_cast<const QualifiedType &>(type);
        oss << "Qualified\n";
        oss << indent() << "qualifiers: ";
        if ((qualified_type.qualifiers() & Qualifier::Const) != Qualifier(0))
            oss << "Const ";
        if ((qualified_type.qualifiers() & Qualifier::Volatile) != Qualifier(0))
            oss << "Volatile ";
        if ((qualified_type.qualifiers() & Qualifier::Restrict) != Qualifier(0))
            oss << "Restrict ";
        oss << "\n";
        oss << indent() << "base_type:";
        enter_scope();
        oss << print_type(qualified_type.base_type());
        leave_scope();
        break;
    }
    }
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const Expr &expr)
{
    if (auto e = dynamic_cast<const VariableExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const IntegerLiteralExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const FloatLiteralExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const StringLiteralExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const BinaryExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const UnaryExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const CallExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const MemberAccessExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const ArrayAccessExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const CastExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const SizeofExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const InitListExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const FunctionPointerExpr *>(&expr))
        return print(*e);
    if (auto e = dynamic_cast<const StructLiteralExpr *>(&expr))
        return print(*e);
    return indent() + "unknown_expr\n";
}

string ASTPrinter::print(const VariableExpr &expr)
{
    ostringstream oss;
    oss << indent() << "variable_expr:\n";
    enter_scope();
    oss << indent() << "name: \"" << escape_string(expr.name) << "\"\n";
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const BinaryExpr &expr)
{
    ostringstream oss;
    oss << indent() << "binary_expr:\n";
    enter_scope();
    oss << indent() << "operator: " << token_type_to_string(expr.op) << "\n";
    oss << indent() << "left:\n";
    enter_scope();
    oss << print(*expr.left);
    leave_scope();
    oss << indent() << "right:\n";
    enter_scope();
    oss << print(*expr.right);
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const CallExpr &expr)
{
    ostringstream oss;
    oss << indent() << "call_expr:\n";
    enter_scope();
    oss << indent() << "callee:\n";
    enter_scope();
    oss << print(*expr.callee);
    leave_scope();
    oss << indent() << "arguments:\n";
    enter_scope();
    for (const auto &arg : expr.args)
    {
        oss << print(*arg);
    }
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const MemberAccessExpr &expr)
{
    ostringstream oss;
    oss << indent() << "member_access_expr:\n";
    enter_scope();
    oss << indent() << "object:\n";
    enter_scope();
    oss << print(*expr.object);
    leave_scope();
    oss << indent() << "member: \"" << escape_string(expr.member) << "\"\n";
    oss << indent() << "accessor: " << token_type_to_string(expr.accessor) << "\n";
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const ArrayAccessExpr &expr)
{
    ostringstream oss;
    oss << indent() << "array_access_expr:\n";
    enter_scope();
    oss << indent() << "array:\n";
    enter_scope();
    oss << print(*expr.array);
    leave_scope();
    oss << indent() << "index:\n";
    enter_scope();
    oss << print(*expr.index);
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const TypeAliasDecl &type_alias_decl)
{
    ostringstream oss;
    oss << indent() << "type_alias_decl:\n";
    enter_scope();
    oss << indent() << "name: \"" << escape_string(type_alias_decl.name) << "\"\n";
    oss << indent() << "type: " << print_type(*type_alias_decl.type) << "\n";
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const StructDecl &struct_decl)
{
    ostringstream oss;
    oss << indent() << "struct_decl:\n";
    enter_scope();
    oss << indent() << "name: \"" << escape_string(struct_decl.name) << "\"\n";
    oss << indent() << "fields:\n";
    enter_scope();
    for (const auto &field : struct_decl.fields)
    {
        oss << indent() << "- field:\n";
        enter_scope();
        oss << indent() << "name: \"" << escape_string(field.name) << "\"\n";
        oss << indent() << "type: " << print_type(*field.type) << "\n";
        leave_scope();
    }
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const FunctionDecl &func_decl)
{
    ostringstream oss;
    oss << indent() << "function_decl:\n";
    enter_scope();
    oss << indent() << "name: \"" << escape_string(func_decl.name) << "\"\n";
    oss << indent() << "return_type: " << print_type(*func_decl.return_type) << "\n";

    oss << indent() << "parameters:\n";
    enter_scope();
    for (const auto &param : func_decl.params)
    {
        oss << indent() << "- parameter:\n";
        enter_scope();
        oss << indent() << "name: \"" << escape_string(param.name) << "\"\n";
        oss << indent() << "type: " << print_type(*param.type) << "\n";
        leave_scope();
    }
    leave_scope();

    if (func_decl.is_method)
    {
        oss << indent() << "receiver_type: " << print_type(*func_decl.receiver_type) << "\n";
    }

    oss << indent() << "body:\n";
    enter_scope();
    for (const auto &stmt : func_decl.body)
    {
        oss << print(*stmt);
    }
    leave_scope();

    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const ImplBlock &impl_block)
{
    ostringstream oss;
    oss << indent() << "impl_block:\n";
    enter_scope();
    oss << indent() << "target_type: " << print_type(*impl_block.target_type) << "\n";
    oss << indent() << "methods:\n";
    enter_scope();
    for (const auto &method : impl_block.methods)
    {
        oss << indent() << "- \n";
        enter_scope();
        oss << print(*method);
        leave_scope();
    }
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const VarDeclStmt &stmt)
{
    ostringstream oss;
    oss << indent() << "var_decl:\n";
    enter_scope();
    oss << indent() << "name: \"" << escape_string(stmt.name) << "\"\n";
    oss << indent() << "type: " << print_type(*stmt.type) << "\n";
    oss << indent() << "is_const: " << (stmt.is_const ? "true" : "false") << "\n";
    if (stmt.init_expr)
    {
        oss << indent() << "initializer:\n";
        enter_scope();
        oss << print(*stmt.init_expr);
        leave_scope();
    }
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const IntegerLiteralExpr &expr)
{
    ostringstream oss;
    oss << indent() << "integer_literal:\n";
    enter_scope();
    oss << indent() << "value: " << expr.value << "\n";
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const FloatLiteralExpr &expr)
{
    ostringstream oss;
    oss << indent() << "float_literal:\n";
    enter_scope();
    oss << indent() << "value: " << expr.value << "\n";
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const StringLiteralExpr &expr)
{
    ostringstream oss;
    oss << indent() << "string_literal:\n";
    enter_scope();
    oss << indent() << "value: \"" << escape_string(expr.value) << "\"\n";
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const UnaryExpr &expr)
{
    ostringstream oss;
    oss << indent() << "unary_expr:\n";
    enter_scope();
    oss << indent() << "operator: " << token_type_to_string(expr.op) << "\n";
    oss << indent() << "operand:\n";
    enter_scope();
    oss << print(*expr.operand);
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const SizeofExpr &expr)
{
    ostringstream oss;
    oss << indent() << "sizeof_expr:\n";
    enter_scope();
    if (expr.kind == SizeofExpr::Kind::Type)
    {
        oss << indent() << "target_type: " << print_type(*expr.target_type) << "\n";
    }
    else if (expr.kind == SizeofExpr::Kind::Expr)
    {
        oss << indent() << "target_expr:\n";
        enter_scope();
        oss << print(*expr.target_expr);
        leave_scope();
    }
    else
    {
        unreachable();
    }
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const AddressOfExpr &expr)
{
    ostringstream oss;
    oss << indent() << "address_of:\n";
    enter_scope();
    oss << indent() << "operand:\n";
    enter_scope();
    oss << print(*expr.operand);
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const DerefExpr &expr)
{
    ostringstream oss;
    oss << indent() << "deref_expr:\n";
    enter_scope();
    oss << indent() << "operand:\n";
    enter_scope();
    oss << print(*expr.operand);
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const StructLiteralExpr &expr)
{
    ostringstream oss;
    oss << indent() << "struct_literal:\n";
    enter_scope();
    if (!expr.struct_name.empty())
    {
        oss << indent() << "struct_name: \"" << escape_string(expr.struct_name) << "\"\n";
    }
    oss << indent() << "members:\n";
    enter_scope();
    for (const auto &member : expr.members)
    {
        oss << indent() << "- member:\n";
        enter_scope();
        oss << indent() << "name: \"" << escape_string(member.first) << "\"\n";
        oss << indent() << "value:\n";
        enter_scope();
        oss << print(*member.second);
        leave_scope();
        leave_scope();
    }
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const Statement &stmt)
{
    if (auto s = dynamic_cast<const BlockStmt *>(&stmt))
        return print(*s);
    if (auto s = dynamic_cast<const ReturnStmt *>(&stmt))
        return print(*s);
    if (auto s = dynamic_cast<const IfStmt *>(&stmt))
        return print(*s);
    if (auto s = dynamic_cast<const WhileStmt *>(&stmt))
        return print(*s);
    if (auto s = dynamic_cast<const BreakStmt *>(&stmt))
        return print(*s);
    if (auto s = dynamic_cast<const ContinueStmt *>(&stmt))
        return print(*s);
    if (auto s = dynamic_cast<const ExprStmt *>(&stmt))
        return print(*s);
    if (auto s = dynamic_cast<const VarDeclStmt *>(&stmt))
        return print(*s);
    return indent() + "unknown_stmt\n";
}

string ASTPrinter::print(const BlockStmt &stmt)
{
    ostringstream oss;
    oss << indent() << "block_stmt:\n";
    enter_scope();
    for (const auto &stmt : stmt.statements)
    {
        oss << print(*stmt);
    }
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const BreakStmt &stmt)
{
    return indent() + "break_stmt\n";
}

string ASTPrinter::print(const ContinueStmt &stmt)
{
    return indent() + "continue_stmt\n";
}

string ASTPrinter::print(const ExprStmt &stmt)
{
    ostringstream oss;
    oss << indent() << "expr_stmt:\n";
    enter_scope();
    oss << print(*stmt.expr);
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const IfStmt &stmt)
{
    ostringstream oss;
    oss << indent() << "if_stmt:\n";
    enter_scope();

    oss << indent() << "condition:\n";
    enter_scope();
    oss << print(*stmt.condition);
    leave_scope();

    oss << indent() << "then_branch:\n";
    enter_scope();
    oss << print(*stmt.then_branch);
    leave_scope();

    if (stmt.else_branch)
    {
        oss << indent() << "else_branch:\n";
        enter_scope();
        oss << print(*stmt.else_branch);
        leave_scope();
    }

    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const WhileStmt &stmt)
{
    ostringstream oss;
    oss << indent() << "while_stmt:\n";
    enter_scope();
    oss << indent() << "condition:\n";
    enter_scope();
    oss << print(*stmt.condition);
    leave_scope();
    oss << indent() << "body:\n";
    enter_scope();
    oss << print(*stmt.body);
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const ReturnStmt &stmt)
{
    ostringstream oss;
    oss << indent() << "return_stmt:\n";
    enter_scope();
    if (stmt.value)
    {
        oss << indent() << "value:\n";
        enter_scope();
        oss << print(*stmt.value);
        leave_scope();
    }
    else
    {
        oss << indent() << "value: null\n";
    }
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const CastExpr &expr)
{
    ostringstream oss;
    oss << indent() << "cast_expr:\n";
    enter_scope();
    oss << indent() << "target_type: " << print_type(*expr.target_type) << "\n";
    oss << indent() << "expression:\n";
    enter_scope();
    oss << print(*expr.expr);
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const InitListExpr &expr)
{
    ostringstream oss;
    oss << indent() << "init_list:\n";
    enter_scope();
    oss << indent() << "members:\n";
    enter_scope();
    for (const auto &member : expr.members)
    {
        oss << print(*member);
    }
    leave_scope();
    leave_scope();
    return oss.str();
}

string ASTPrinter::print(const FunctionPointerExpr &expr)
{
    ostringstream oss;
    oss << indent() << "function_ptr:\n";
    enter_scope();
    oss << indent() << "params:\n";
    enter_scope();
    for (const auto &param : expr.param_types)
    {
        oss << indent() << "- \n";
        enter_scope();
        oss << print_type(*param);
        leave_scope();
    }
    leave_scope();
    oss << indent() << "return_type: " << print_type(*expr.return_type) << "\n";
    leave_scope();
    return oss.str();
}
