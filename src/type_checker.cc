// type_checker.cc
#include "type_checker.h"
#include "lexer.h"
#include "utils.h"

#include <cassert>
#include <algorithm>
#include <unordered_set>

using namespace ast;

TypeChecker::TypeChecker(ast::Program *program)
    : program_(program),
      global_scope_(std::make_unique<Scope>(nullptr))
{
    current_scope_ = global_scope_.get();
}

TypeChecker::TypeCheckResult TypeChecker::check()
{
    if (!program_)
    {
        return {false, {"Program not set"}};
    }

    for (auto &struct_ : program_->structs)
    {
        visit(*struct_);
    }

    for (auto &alias : program_->aliases)
    {
        visit(*alias);
    }

    for (auto &impl : program_->impl_blocks)
    {
        visit(*impl);
    }

    for (auto &global : program_->globals)
    {
        visit(*global);
        global_scope_->insert_variable(global->name, global->type->clone());
    }

    for (auto &func : program_->functions)
    {
        visit(*func);
    }

    return {errors_.empty(), std::move(errors_)};
}

// Scope management implementation
void TypeChecker::push_scope()
{
    current_scope_ = new Scope{current_scope_};
}

void TypeChecker::pop_scope()
{
    assert(current_scope_ != global_scope_.get() && "Cannot pop global scope");
    auto tmp = current_scope_;
    current_scope_ = current_scope_->parent();
    delete tmp;
}

bool TypeChecker::types_equal(const Type &t1, const Type &t2) const
{
    return t1.equals(&t2);
}

bool TypeChecker::is_valid_lvalue(Expr &expr)
{
    return dynamic_cast<VariableExpr *>(&expr) ||
           dynamic_cast<MemberAccessExpr *>(&expr) ||
           dynamic_cast<ArrayAccessExpr *>(&expr) ||
           dynamic_cast<DerefExpr *>(&expr);
}

bool TypeChecker::is_valid_cond_type(const ast::Type &type)
{
    // TODO: Need strict boolean?
    bool isInt = type.kind() == Type::Kind::Int;
    bool isFloat = type.kind() == Type::Kind::Float;
    bool isBool = type.kind() == Type::Kind::Bool;

    if (!isInt && !isFloat && !isBool)
    {
        add_error("Condition must be of numeric type (int or float), actually is ", type.to_string());
        return false;
    }

    return true;
}

// Expression checking entry point
void TypeChecker::check_expr(Expr &expr)
{
    MO_DEBUG("Checking %s", expr.name().c_str());
    // Dispatch to specific visitor
    if (auto v = dynamic_cast<VariableExpr *>(&expr))
        visit(*v);
    else if (auto i = dynamic_cast<IntegerLiteralExpr *>(&expr))
        visit(*i);
    else if (auto i = dynamic_cast<BooleanLiteralExpr *>(&expr))
        visit(*i);
    else if (auto f = dynamic_cast<FloatLiteralExpr *>(&expr))
        visit(*f);
    else if (auto s = dynamic_cast<StringLiteralExpr *>(&expr))
        visit(*s);
    else if (auto s = dynamic_cast<StructLiteralExpr *>(&expr))
        visit(*s);
    else if (auto b = dynamic_cast<BinaryExpr *>(&expr))
        visit(*b);
    else if (auto u = dynamic_cast<UnaryExpr *>(&expr))
        visit(*u);
    else if (auto c = dynamic_cast<CallExpr *>(&expr))
        visit(*c);
    else if (auto m = dynamic_cast<MemberAccessExpr *>(&expr))
        visit(*m);
    else if (auto a = dynamic_cast<ArrayAccessExpr *>(&expr))
        visit(*a);
    else if (auto c = dynamic_cast<CastExpr *>(&expr))
        visit(*c);
    else if (auto s = dynamic_cast<SizeofExpr *>(&expr))
        visit(*s);
    else if (auto a = dynamic_cast<AddressOfExpr *>(&expr))
        visit(*a);
    else if (auto d = dynamic_cast<DerefExpr *>(&expr))
        visit(*d);
    else if (auto i = dynamic_cast<InitListExpr *>(&expr))
        visit(*i);
    else if (auto i = dynamic_cast<TupleExpr *>(&expr))
        visit(*i);
    else if (auto f = dynamic_cast<FunctionPointerExpr *>(&expr))
        visit(*f);
    else
    {
        MO_ASSERT(false, "Unknown expression type `%s`", expr.name().c_str());
    }

    switch (expr.expr_category)
    {
    case Expr::Category::Unknown:
        // assert(false && "Unknown expression category");
        break;
    case Expr::Category::LValue:
        if (!is_valid_lvalue(expr))
        {
            add_error("Expression marked as LValue is not a valid left value");
            expr.expr_category = Expr::Category::Unknown; // 标记为错误状态
        }
        break;
    case Expr::Category::RValue:
        break;
    }

    MO_ASSERT(expr.type, "Expression '%s' has no type after checking", expr.name().c_str());
    expr.type = resolve_alias(*expr.type);
    MO_DEBUG("Type of %s: %s", expr.name().c_str(), expr.type->to_string().c_str());
}

// Cast expression
void TypeChecker::visit(CastExpr &expr)
{
    check_expr(*expr.expr);
    MO_ASSERT(expr.target_type, "Cast expression has no target type");
    expr.target_type = resolve_alias(*expr.target_type);
    if (!expr.expr->type || !expr.target_type)
    {
        add_error("Invalid cast expression");
        return;
    }

    if (!is_convertible(*expr.expr->type, *expr.target_type, true, true))
    {
        add_error("Invalid type conversion in cast: ", expr.expr->type->to_string(), " to ", expr.target_type->to_string());
    }

    expr.type = expr.target_type->clone();
    expr.expr_category = Expr::Category::RValue;
}

void TypeChecker::visit(SizeofExpr &expr)
{
    switch (expr.kind)
    {
    case SizeofExpr::Kind::Type:
        expr.target_type = resolve_alias(*expr.target_type);
        if (!expr.target_type || expr.target_type->kind() == Type::Kind::Placeholder)
        {
            add_error("Invalid type in sizeof operator");
        }
        break;

    case SizeofExpr::Kind::Expr:
        check_expr(*expr.target_expr);
        break;

    default:
        add_error("Unsupported kind in sizeof operator");
        break;
    }

    // sizeof always returns integer
    expr.type = Type::create_int(64, true);
    expr.expr_category = Expr::Category::RValue;
}

// AddressOf expression
void TypeChecker::visit(AddressOfExpr &expr)
{
    check_expr(*expr.operand);

    if (!expr.operand->type)
        return;

    if (expr.operand->expr_category != Expr::Category::LValue)
    {
        add_error("Cannot take address of rvalue");
        return;
    }

    expr.type = Type::create_pointer(expr.operand->type->clone());
    expr.expr_category = Expr::Category::RValue;
}

// Dereference expression
void TypeChecker::visit(DerefExpr &expr)
{
    check_expr(*expr.operand);

    if (expr.operand->type->kind() != Type::Kind::Pointer)
    {
        add_error("Dereference of non-pointer type");
        return;
    }

    auto pointer_type = static_cast<PointerType *>(expr.operand->type.get());
    expr.type = pointer_type->pointee().clone();
    expr.expr_category = Expr::Category::LValue;
}

// Basic expression visitors
void TypeChecker::visit(VariableExpr &expr)
{
    expr.expr_category = Expr::Category::LValue;

    if (auto type = current_scope_->resolve_variable(expr.identifier))
    {
        expr.type = type->clone();
    }
    else
    {
        add_error("Undefined symbol '" + expr.identifier + "'");
        MO_DEBUG("Undefined symbol: %s", expr.identifier.c_str());
        expr.type = Type::create_placeholder();
    }
}

void TypeChecker::visit(IntegerLiteralExpr &expr)
{
    expr.type = Type::create_int();
    expr.expr_category = Expr::Category::RValue;
}

void TypeChecker::visit(BooleanLiteralExpr &expr)
{
    expr.type = Type::create_int(1);
    expr.expr_category = Expr::Category::RValue;
}

void TypeChecker::visit(FloatLiteralExpr &expr)
{
    expr.type = Type::create_float();
    expr.expr_category = Expr::Category::RValue;
}

void TypeChecker::visit(StringLiteralExpr &expr)
{
    expr.type = Type::create_string();
    expr.expr_category = Expr::Category::RValue;
}

void TypeChecker::visit(StructDecl &decl)
{
    for (auto &field : decl.fields)
    {
        if (field.type->kind() == Type::Kind::Alias)
        {
            TypePtr resolved = resolve_alias(*field.type);
            if (resolved)
            {
                field.type = std::move(resolved);
            }
        }

        if (field.type->kind() == Type::Kind::Struct)
        {
            if (!find_struct(static_cast<StructType *>(field.type.get())->name()))
            {
                add_error("Undefined struct type in field '" + field.name + "': " + static_cast<StructType *>(field.type.get())->name());
            }
        }

        if (types_equal(*field.type, *Type::create_void()))
        {
            add_error("Field '" + field.name + "' cannot have void type");
        }
    }
    MO_DEBUG("Struct '%s':", decl.name.c_str());
    auto struct_ty = decl.type()->clone();
    for (auto &field : *struct_ty->as_struct())
    {
        MO_DEBUG("  %s: %s", field.name.c_str(), field.type->to_string().c_str());
    }
    if (!current_scope_->insert_type(decl.name, std::move(struct_ty)))
    {
        add_error("Duplicate struct name: " + decl.name);
        MO_DEBUG("Duplicate struct name: %s", decl.name.c_str());
    }
    else
    {
        MO_DEBUG("Struct '%s' defined", decl.name.c_str());
    }
}

void TypeChecker::visit(GlobalDecl &decl)
{
    if (decl.type->kind() == Type::Kind::Struct)
    {
        if (!find_struct(static_cast<StructType *>(decl.type.get())->name()))
        {
            add_error("Undefined struct type in global variable '" + decl.name + "': " + static_cast<StructType *>(decl.type.get())->name());
        }
    }

    if (decl.type->kind() == Type::Kind::Alias)
    {
        TypePtr resolved = resolve_alias(*decl.type);
        if (resolved)
        {
            decl.type = std::move(resolved);
        }
    }

    if (decl.init_expr)
    {
        check_expr(*decl.init_expr);
        if (!is_convertible(*decl.init_expr->type, *decl.type))
        {
            add_error("Type mismatch in global variable initialization: " + decl.name);
        }
    }

    if (decl.is_const && !decl.init_expr)
    {
        add_error("Constant global variable '" + decl.name + "' must be initialized");
    }

    if (auto alias = decl.type->as_alias())
    {
        auto name = alias->name();
        if (auto type = current_scope_->resolve_type(name))
        {
            decl.type = type->clone();
        }
        else
        {
            add_error("Undefined type for unresolved symbol: " + name);
        }
    }

    if (!global_scope_->insert_variable(decl.name, decl.type->clone()))
    {
        add_error("Duplicate global variable name: " + decl.name);
    }
}

// Impl blocks validation
void TypeChecker::visit(ImplBlock &impl)
{
    // Verify target type exists
    if (impl.target_type->kind() == Type::Kind::Struct)
    {
        if (!find_struct(static_cast<StructType *>(impl.target_type.get())->name()))
        {
            add_error("Impl for undefined struct: " + static_cast<StructType *>(impl.target_type.get())->name());
        }
    }

    // Check methods
    for (auto &method : impl.methods)
    {
        // Verify receiver type
        if (!method->receiver_type ||
            !types_equal(*method->receiver_type, *impl.target_type))
        {
            add_error("Method receiver type mismatch");
        }

        visit(*method);
    }
}

// Type alias support
void TypeChecker::visit(TypeAliasDecl &decl)
{
    MO_ASSERT(decl.type, "Type alias has no target type");
    TypePtr resolved = resolve_alias(*decl.type);
    if (resolved)
    {
        decl.type = std::move(resolved);
    }
    else
    {
        add_error("Invalid target type for alias: " + decl.name);
        MO_ASSERT(false, "Invalid target type for alias: %s", decl.name.c_str());
    }

    if (!current_scope_->insert_type(decl.name, decl.type->clone()))
    {
        add_error("Duplicate alias: " + decl.name);
        MO_ASSERT(false, "Duplicate alias: %s", decl.name.c_str());
    }
}

// Binary expression type rules
void TypeChecker::visit(BinaryExpr &expr)
{
    MO_DEBUG("Checking BinaryExpr op: %s", token_type_to_string(expr.op).c_str());
    check_expr(*expr.left);
    check_expr(*expr.right);

    if (!expr.left->type || !expr.right->type)
    {
        MO_ASSERT(false, "LHS or RHS of binary expression has no type");
        return;
    }

    switch (expr.op)
    {
    case TokenType::Assign:
    {
        if (expr.left->expr_category != Expr::Category::LValue)
        {
            add_error("Cannot assign to rvalue");
        }
        if (!is_convertible(*expr.right->type, *expr.left->type))
        {
            add_error("Type mismatch in assignment");
        }
        expr.type = expr.left->type->clone();
        expr.expr_category = Expr::Category::RValue;
        break;
    }
    // Compound assignment operators
    case TokenType::AddAssign:
    case TokenType::SubAssign:
    case TokenType::MulAssign:
    case TokenType::DivAssign:
    {
        if (expr.left->expr_category != Expr::Category::LValue)
        {
            add_error("Left operand of compound assignment must be lvalue");
        }
        if (!expr.left->type->is_numeric() || !expr.right->type->is_numeric())
        {
            add_error("Operands for compound assignment must be numeric");
        }
        if (!is_convertible(*expr.right->type, *expr.left->type))
        {
            add_error("Type mismatch in compound assignment");
        }
        expr.type = expr.left->type->clone();
        expr.expr_category = Expr::Category::RValue;
        break;
    }

    case TokenType::ModAssign:
    case TokenType::AndAssign:
    case TokenType::OrAssign:
    case TokenType::XorAssign:
    case TokenType::LSAssign: // <<=
    case TokenType::RSAssign: // >>=
    {
        if (expr.left->expr_category != Expr::Category::LValue)
        {
            add_error("Left operand of compound assignment must be lvalue");
        }
        if (expr.left->type->kind() != Type::Kind::Int ||
            expr.right->type->kind() != Type::Kind::Int)
        {
            add_error("Operands for bitwise compound assignment must be integers");
        }
        expr.type = expr.left->type->clone();
        expr.expr_category = Expr::Category::RValue;
        break;
    }

    // Shift operators
    case TokenType::LShift:
    case TokenType::RShift:
    {
        if (expr.left->type->kind() != Type::Kind::Int ||
            expr.right->type->kind() != Type::Kind::Int)
        {
            add_error("Shift operators require integer operands");
        }
        expr.type = expr.left->type->clone();
        expr.expr_category = Expr::Category::RValue;
        break;
    }

    // Arithemetic operators
    case TokenType::Plus:
    case TokenType::Minus:
    case TokenType::Star:
    case TokenType::Slash:
    {
        MO_DEBUG("Checking arithmetic op: %s", token_type_to_string(expr.op).c_str());
        if (auto lhs_int_ty = expr.left->type->as_integer())
        {
            if (auto rhs_int_ty = expr.right->type->as_integer()) // int +-*/ int = int
            {
                if (!lhs_int_ty->equals(rhs_int_ty))
                {
                    add_error("Operands must have same integer type for arithmetic operation");
                    MO_WARN("Operands must have same integer type for arithmetic operation");
                }
                expr.type = lhs_int_ty->clone();
            }
            else if (auto rhs_float_ty = expr.right->type->as_float()) // int +-*/ float = float
            {
                add_error("Operands must have same numeric type for arithmetic operation");
                MO_WARN("Operands must have same numeric type for arithmetic operation");
                expr.type = Type::create_float(static_cast<uint8_t>(rhs_float_ty->bit_width()));
            }
            else
            {
                add_error("Invalid operands for arithmetic operation");
                MO_WARN("Invalid operands for arithmetic operation");
                expr.type = lhs_int_ty->clone();
            }
        }
        else if (auto lhs_float_ty = expr.left->type->as_float())
        {
            if (auto rhs_float_ty = expr.right->type->as_float()) // float +-*/ float = float
            {
                if (!lhs_float_ty->equals(rhs_float_ty))
                {
                    add_error("Operands must have same float type for arithmetic operation");
                }
                expr.type = lhs_float_ty->clone();
            }
            else
            {
                add_error("Invalid operands for arithmetic operation");
                expr.type = lhs_float_ty->clone();
            }
        }
        else if (expr.op == TokenType::Plus || expr.op == TokenType::Minus)
        {
            if (auto lhs_ptr_ty = expr.left->type->as_pointer())
            {
                if (expr.right->type->is_integer()) // ptr +- int = ptr
                {
                    expr.type = lhs_ptr_ty->clone();
                }
                else
                {
                    add_error("Invalid operands for pointer arithmetic operation");
                }
            }
            else
            {
                add_error("Invalid operands for pointer arithmetic operation");
            }
        }
        else
        {
            add_error("Invalid operands for arithmetic operation");
            MO_ASSERT(false, "Invalid operands for arithmetic operation, left: %s, right: %s",
                      expr.left->type->to_string().c_str(),
                      expr.right->type->to_string().c_str());
        }
        expr.expr_category = Expr::Category::RValue;
        break;
    }
    // Comparison operators
    case TokenType::Eq:
    case TokenType::Ne:
    case TokenType::Lt:
    case TokenType::Le:
    case TokenType::Gt:
    case TokenType::Ge:
    {
        if (!types_equal(*expr.left->type, *expr.right->type))
        {
            add_error("Operands must have same type for comparison");
            break;
        }
        const bool numeric_operands = expr.left->type->is_scalar();

        if (!numeric_operands)
        {
            add_error("Comparison requires numeric operands");
        }

        expr.type = Type::create_bool(); // Comparison result is bool
        expr.expr_category = Expr::Category::RValue;
        break;
    }
    // Logical operators
    case TokenType::And:
    case TokenType::Or:
    {
        if (expr.left->type->kind() != Type::Kind::Bool ||
            expr.right->type->kind() != Type::Kind::Bool)
        {
            add_error("Logical operators require boolean operands");
        }
        expr.type = Type::create_bool(); // Logical result is bool
        expr.expr_category = Expr::Category::RValue;
        break;
    }

    // Bitwise operators
    case TokenType::Modulo:
    case TokenType::Ampersand:
    case TokenType::Pipe:
    case TokenType::Caret:
    case TokenType::Tilde:
    case TokenType::Not:
    {
        if (expr.left->type->kind() != Type::Kind::Int ||
            expr.right->type->kind() != Type::Kind::Int)
        {
            add_error("Bitwise operators require integer operands");
        }
        expr.type = Type::create_int();
        expr.expr_category = Expr::Category::RValue;
        break;
    }

    default:
    {
        MO_ASSERT(false, "Binary operator not covered by type checking: %s", token_type_to_string(expr.op).c_str());
        break;
    }
    }

    MO_ASSERT(expr.type, "Binary expression has no type");
}
void TypeChecker::visit(UnaryExpr &expr)
{
    check_expr(*expr.operand);
    if (!expr.operand->type)
        return;

    switch (expr.op)
    {
    case TokenType::Plus:
        if (expr.operand->type->kind() != Type::Kind::Int &&
            expr.operand->type->kind() != Type::Kind::Float)
        {
            add_error("Unary plus on non-numeric type");
        }
        expr.type = expr.operand->type->clone();
        expr.expr_category = Expr::Category::RValue;
        break;

    case TokenType::Minus:
        if (expr.operand->type->kind() != Type::Kind::Int &&
            expr.operand->type->kind() != Type::Kind::Float)
        {
            add_error("Unary minus on non-numeric type");
        }
        expr.type = expr.operand->type->clone();
        expr.expr_category = Expr::Category::RValue;
        break;

    case TokenType::Ampersand:
        if (expr.operand->expr_category != Expr::Category::LValue)
        {
            add_error("Cannot take address of rvalue");
        }
        expr.type = Type::create_pointer(expr.operand->type->clone());
        expr.expr_category = Expr::Category::RValue;
        break;

    case TokenType::Star:
    {
        if (expr.operand->type->kind() != Type::Kind::Pointer)
        {
            add_error("Dereference of non-pointer type");
        }
        auto pointer_type = static_cast<PointerType *>(expr.operand->type.get());
        expr.type = pointer_type->pointee().clone();
        expr.expr_category = Expr::Category::LValue;
        break;
    }

    case TokenType::Not:
        expr.type = Type::create_bool();
        expr.expr_category = Expr::Category::RValue;
        break;

    case TokenType::Tilde:
        if (expr.operand->type->kind() != Type::Kind::Int)
        {
            add_error("Bitwise complement on non-integer type");
        }
        expr.type = expr.operand->type->clone();
        expr.expr_category = Expr::Category::RValue;
        break;

    default:
        add_error("Unsupported unary operator");
        MO_DEBUG("Unsupported unary operator: %s", token_type_to_string(expr.op).c_str());
    }
}

void TypeChecker::visit(CallExpr &expr)
{
    check_expr(*expr.callee);
    for (auto &arg : expr.args)
        check_expr(*arg);

    if (!expr.callee->type)
        return;

    // Function type checking
    if (expr.callee->type->kind() != Type::Kind::Function)
    {
        add_error("Calling non-function type");
        return;
    }

    auto function_type = static_cast<FunctionType *>(expr.callee->type.get());

    // Check argument count
    if (expr.args.size() != function_type->params().size())
    {
        add_error("Argument count mismatch in function call");
        return;
    }

    // Check argument types
    for (size_t i = 0; i < expr.args.size(); ++i)
    {
        auto &arg_type = expr.args[i]->type;
        MO_ASSERT(arg_type != nullptr, "Argument must have a type");
        auto &param_type = function_type->params()[i];
        MO_ASSERT(param_type != nullptr, "Parameter must have a type");
        if (!is_convertible(*arg_type, *param_type))
        {
            add_error("Argument type mismatch in function call");
        }
    }

    expr.type = function_type->return_type().clone();
    expr.expr_category = Expr::Category::RValue;
}

// Statement checking implementation
void TypeChecker::check_stmt(Statement &stmt)
{
    if (auto e = dynamic_cast<ExprStmt *>(&stmt))
        visit(*e);
    else if (auto v = dynamic_cast<VarDeclStmt *>(&stmt))
        visit(*v);
    else if (auto r = dynamic_cast<ReturnStmt *>(&stmt))
        visit(*r);
    else if (auto b = dynamic_cast<BlockStmt *>(&stmt))
        visit(*b);
    else if (auto i = dynamic_cast<IfStmt *>(&stmt))
        visit(*i);
    else if (auto w = dynamic_cast<WhileStmt *>(&stmt))
        visit(*w);
    else if (auto b = dynamic_cast<BreakStmt *>(&stmt))
        visit(*b);
    else if (auto c = dynamic_cast<ContinueStmt *>(&stmt))
        visit(*c);
}

void TypeChecker::visit(ExprStmt &stmt)
{
    check_expr(*stmt.expr);
}

// Type deduction system
void TypeChecker::visit(VarDeclStmt &stmt)
{
    if (stmt.type)
    {
        TypePtr resolved = resolve_alias(*stmt.type);
        if (resolved)
        {
            stmt.type = std::move(resolved);
        }
    }

    // Handle type deduction and explicit type declarations
    const bool has_explicit_type = static_cast<bool>(stmt.type);
    const bool has_initializer = static_cast<bool>(stmt.init_expr);

    // Case 1: No type and no initializer
    if (!has_explicit_type && !has_initializer)
    {
        add_error("Variable declaration must have either type or initializer: " + stmt.name);
        return;
    }

    // Case 2: Type deduction (no explicit type but has initializer)
    if (!has_explicit_type)
    {
        check_expr(*stmt.init_expr); // Deduce type from the initializer expression

        if (!stmt.init_expr->type)
        {
            add_error("Cannot deduce variable type from invalid initializer: " + stmt.name);
            stmt.type = Type::create_placeholder(); // Create a placeholder for unknown type
        }
        else
        {
            // Copy the type from the initializer expression
            stmt.type = stmt.init_expr->type->clone();
        }
    }
    // Case 3: Explicit type declaration (may have initializer)
    else
    {
        // Verify type validity (e.g., check if user-defined struct exists)
        if (stmt.type->kind() == Type::Kind::Struct)
        {
            auto struct_type = static_cast<StructType *>(stmt.type.get());
            if (!find_struct(struct_type->name()))
            {
                add_error("Undefined struct type: " + struct_type->name());
            }
        }

        if (has_initializer)
        {
            check_expr(*stmt.init_expr);
            if (stmt.init_expr->type && !is_convertible(*stmt.init_expr->type, *stmt.type))
            {
                std::string init_type_name = "unknown";
                if (stmt.init_expr->type->kind() == Type::Kind::Struct)
                {
                    init_type_name = static_cast<StructType *>(stmt.init_expr->type.get())->name();
                }
                std::string stmt_type_name = "unknown";
                if (stmt.type->kind() == Type::Kind::Struct)
                {
                    stmt_type_name = static_cast<StructType *>(stmt.type.get())->name();
                }
                add_error("Type mismatch in variable initialization: " + stmt.name + " (" + stmt_type_name + " vs " + init_type_name + ")");
            }
        }
    }

    // Check for variable redeclaration
    if (current_scope_->resolve_variable(stmt.name))
    {
        add_error("Redeclaration of variable: " + stmt.name);
        return;
    }

    // Register the variable in the current scope
    assert(!stmt.name.empty() && "Variable name must be set");
    if (!current_scope_->insert_variable(stmt.name, stmt.type->clone()))
    {
        add_error("Failed to register variable in scope: " + stmt.name);
    }

    // Constant verification
    if (stmt.is_const)
    {
        if (!has_initializer)
        {
            add_error("Constant variable must be initialized: " + stmt.name);
        }
        else if (stmt.init_expr->expr_category != Expr::Category::RValue)
        {
            add_error("Constant must be initialized with rvalue: " + stmt.name);
        }
    }
}

void TypeChecker::visit(ReturnStmt &stmt)
{
    if (stmt.value)
    {
        check_expr(*stmt.value);
        MO_ASSERT(stmt.value->type != nullptr, "Return value must have a type");
        if (!current_return_type_)
        {
            add_error("Return in non-function context");
        }
        else if (!is_convertible(*stmt.value->type, *current_return_type_))
        {
            MO_WARN("Return type mismatch: %s vs %s", stmt.value->type->to_string().c_str(), current_return_type_->to_string().c_str());
            add_error("Return type mismatch");
        }
    }
    else if (current_return_type_ &&
             current_return_type_->kind() != Type::Kind::Void)
    {
        add_error("Non-void function missing return value");
    }
}

void TypeChecker::visit(BlockStmt &stmt)
{
    push_scope();
    for (auto &s : stmt.statements)
        check_stmt(*s);
    pop_scope();
}

// Function declaration handling
void TypeChecker::visit(FunctionDecl &func)
{
    func.return_type = resolve_alias(*func.return_type);

    // Check for duplicate function name
    auto prev_return_type = current_return_type_;
    current_return_type_ = func.return_type.get();

    // To handle recursive function calls, we need to add current function to the
    // current scope before checking its body.
    assert(!func.name.empty() && "Function name must be set");
    if (!current_scope_->insert_variable(func.name, func.type()))
    {
        add_error("Duplicate function name: " + func.name);
    }

    push_scope();

    // Add parameters to scope
    for (auto &param : func.params)
    {
        assert(!param.name.empty() && "Parameter name must be set");
        if (!current_scope_->insert_variable(param.name, param.type->clone()))
        {
            add_error("Duplicate parameter name: " + param.name);
        }
    }

    // Check function body
    for (auto &stmt : func.body)
        check_stmt(*stmt);

    pop_scope();
    current_return_type_ = prev_return_type;
}

// Struct member access checking
void TypeChecker::visit(MemberAccessExpr &expr)
{
    check_expr(*expr.object);
    if (!expr.object->type)
    {
        MO_ASSERT(false, "Member access on invalid object");
        expr.type = Type::create_placeholder();
        return;
    }

    if (expr.object->type->kind() != Type::Kind::Struct)
    {
        add_error("Member access on non-struct type");
        MO_WARN("Member access on non-struct type: %s", expr.object->type->to_string().c_str());
        expr.type = Type::create_placeholder();
        return;
    }

    MO_DEBUG("Object type: %s", expr.object->type->to_string().c_str());

    expr.object->type = resolve_alias(*expr.object->type);

    auto struct_type = expr.object->type->as_struct();
    if (!struct_type)
    {
        add_error("Member access on non-struct type");
        expr.type = Type::create_placeholder();
        return;
    }

    // if followed by call, prioritize method call over function pointer member call
    // if (expr.is_call)
    // {
    //     // Find method
    //     auto method = struct_type->get_method(expr.member);
    //     if (!method)
    //     {
    //         MO_DEBUG("No method '%s' in struct '%s'", expr.member.c_str(), struct_type->name().c_str());
    //         MO_DEBUG("Falling back to function pointer member access");
    //     }
    //     else
    //     {
    //         // Check method signature
    //         if (method->params.size() != expr.args.size())
    //         {
    //             add_error("Argument count mismatch in method call");
    //         }
    //         for (size_t i = 0; i < expr.args.size(); ++i)
    //         {
    //             auto &arg_type = expr.args[i]->type;
    //             auto &param_type = method->params[i].type;

    //             if (!is_convertible(*arg_type, *param_type))
    //             {
    //                 add_error("Argument type mismatch in method call");
    //             }
    //         }
    //         expr.resolved_func = method;
    //         expr.type = method->return_type->clone();
    //         expr.expr_category = Expr::Category::RValue;
    //         return;
    //     }
    // }

    // Find member
    auto member_type = struct_type->find_member(expr.member);
    if (!member_type)
    {
        add_error("No member '" + expr.member + "' in struct '" + struct_type->name() + "'");
        MO_WARN("No member '%s' in struct '%s'", expr.member.c_str(), struct_type->name().c_str());
        expr.type = Type::create_placeholder();
        return;
    }

    expr.type = member_type->clone();
    // FIXME: to be thought twice
    expr.expr_category = expr.accessor == TokenType::Arrow ? Expr::Category::LValue : Expr::Category::RValue;
}

void TypeChecker::visit(ArrayAccessExpr &expr)
{
    check_expr(*expr.array);
    check_expr(*expr.index);

    if (!expr.array->type || !expr.index->type)
        return;

    // Verify index type
    if (expr.index->type->kind() != Type::Kind::Int)
    {
        add_error("Array index must be integer type");
    }

    if (expr.array->type->kind() == ast::Type::Kind::Pointer)
    {
        auto pointer_type = static_cast<PointerType *>(expr.array->type.get());
        if (pointer_type->pointee().kind() == Type::Kind::Void)
        {
            add_error("Cannot subscript void* pointer");
        }
    }

    // Handle array/pointer types
    switch (expr.array->type->kind())
    {
    case Type::Kind::Array:
    {
        auto array_type = static_cast<ArrayType *>(expr.array->type.get());
        expr.type = array_type->element_type().clone();
        expr.expr_category = Expr::Category::LValue;
        break;
    }
    case Type::Kind::Pointer:
    {
        auto pointer_type = static_cast<PointerType *>(expr.array->type.get());
        expr.type = pointer_type->pointee().clone();
        expr.expr_category = Expr::Category::LValue;
        break;
    }
    default:
        add_error("Subscripted value is not array or pointer");
        expr.type = Type::create_placeholder();
    }
}

void TypeChecker::visit(InitListExpr &expr)
{
    Type *common_type = nullptr;
    for (auto &member : expr.members)
    {
        check_expr(*member);
        if (!common_type)
        {
            common_type = member->type.get();
        }
        else if (!types_equal(*member->type, *common_type))
        {
            add_error("All array elements must have same type");
        }
    }

    // Create array type (size may be unknown)
    expr.type = Type::create_array(common_type ? common_type->clone() : Type::create_placeholder(), expr.members.size());
    expr.expr_category = Expr::Category::RValue;
}

void TypeChecker::visit(TupleExpr &expr)
{
    std::vector<TypePtr> elem_types;
    for (auto &member : expr.elements)
    {
        check_expr(*member);
        MO_ASSERT(member->type != nullptr, "Tuple element must have a type");
        elem_types.push_back(member->type->clone());
    }
    expr.type = Type::create_tuple(std::move(elem_types));
    expr.expr_category = Expr::Category::RValue;
}

/**
 * @brief Determines type conversion validity with strictness and explicit context controls
 * @param from Source type to convert from
 * @param to Target type to convert to
 * @param is_strict Enables strict type safety rules when true
 * @param is_explicit True when in explicit conversion context (e.g. cast operations)
 * @return true if conversion is permitted under current rules, false otherwise
 *
 * Conversion rule hierarchy:
 * 1. Type identity (same type) always permitted
 * 2. Numeric conversions governed by strict/explicit flags
 * 3. Pointer conversions follow memory safety rules
 * 4. Type aliases resolve recursively
 * 5. All other conversions explicitly prohibited
 */
bool TypeChecker::is_convertible(const Type &from,
                                 const Type &to,
                                 bool is_strict,
                                 bool is_explicit) const
{
    // Rule 1: Type identity check
    if (types_equal(from, to))
        return true;

    // Rule 2: Integer to floating-point conversion
    // - Permitted implicitly in non-strict mode
    // - Requires explicit cast in strict mode
    if (from.kind() == Type::Kind::Int && to.kind() == Type::Kind::Float)
    {
        return is_explicit || !is_strict; // Explicit context or non-strict mode
    }

    // Rule 3: Floating-point to integer conversion
    // - Requires matching bit-width AND
    // - Explicit cast in strict mode
    if (from.kind() == Type::Kind::Float && to.kind() == Type::Kind::Int)
    {
        const auto &from_float = static_cast<const FloatType &>(from);
        const auto &to_int = static_cast<const IntegerType &>(to);
        return (from_float.bit_width() == to_int.bit_width()) &&
               (is_explicit || !is_strict); // Width match + context check
    }

    // Rule 4: Array-to-pointer decay
    // - Requires element type match AND
    // - Explicit cast in strict mode
    if (from.kind() == Type::Kind::Array && to.kind() == Type::Kind::Pointer)
    {
        auto array_type = static_cast<const ArrayType *>(&from);
        auto pointer_type = static_cast<const PointerType *>(&to);
        return types_equal(array_type->element_type(), pointer_type->pointee()) &&
               (is_explicit || !is_strict); // Type match + decay allowance
    }

    // Rule 5: Null pointer literal conversion (0 → pointer)
    // - Permitted implicitly in non-strict mode
    // - Requires explicit cast in strict mode
    if (from.kind() == Type::Kind::Int &&
        static_cast<const IntegerType &>(from).bit_width() == MO_DEFAULT_INT_BITWIDTH &&
        to.kind() == Type::Kind::Pointer)
    {
        return is_explicit || !is_strict; // Null literal conversion check
    }

    // Rule 6: Pointer compatibility (including void*)
    // - Direct match always permitted
    // - void* conversions require:
    //   - Non-strict mode OR explicit cast
    if (from.kind() == Type::Kind::Pointer && to.kind() == Type::Kind::Pointer)
    {
        auto from_pointer = static_cast<const PointerType *>(&from);
        auto to_pointer = static_cast<const PointerType *>(&to);

        const bool from_void = from_pointer->pointee().kind() == Type::Kind::Void;
        const bool to_void = to_pointer->pointee().kind() == Type::Kind::Void;

        return types_equal(from_pointer->pointee(), to_pointer->pointee()) ||
               ((from_void || to_void) && (is_explicit || !is_strict));
    }

    // Rule 7: Type alias resolution (forward)
    // Preserves strict/explicit flags during resolution
    if (from.kind() == Type::Kind::Alias)
    {
        auto resolved = resolve_alias(*from.clone());
        return is_convertible(*resolved, to, is_strict, is_explicit);
    }

    // Rule 8: Type alias resolution (reverse)
    if (to.kind() == Type::Kind::Alias)
    {
        auto resolved = resolve_alias(*to.clone());
        return is_convertible(from, *resolved, is_strict, is_explicit);
    }

    // Fallthrough: No conversion path found
    return false;
}

// Function pointer handling
void TypeChecker::visit(FunctionPointerExpr &expr)
{
    // Verify parameter types
    for (auto &param : expr.param_types)
    {
        if (!param->clone())
        {
            add_error("Invalid parameter type in function pointer");
        }
    }

    // Build function type
    std::vector<TypePtr> params;
    for (auto &param : expr.param_types)
    {
        params.push_back(param->clone());
    }
    expr.type = Type::create_function(expr.return_type->clone(), std::move(params));
    expr.expr_category = Expr::Category::RValue;
}

// Enhanced error recovery
void TypeChecker::check_expr_safe(Expr &expr)
{
    try
    {
        check_expr(expr);
    }
    catch (const std::exception &e)
    {
        // Create dummy type to prevent cascading errors
        expr.type = Type::create_placeholder();
        expr.expr_category = Expr::Category::RValue;
        add_error("Critical error in expression: " + std::string(e.what()));
    }
}

// Struct literals
void TypeChecker::visit(StructLiteralExpr &expr)
{
    // Find struct definition
    auto struct_decl = find_struct(expr.struct_name);
    if (!struct_decl)
    {
        add_error("Undefined struct type: " + expr.struct_name);
        return;
    }

    // Verify member initialization
    std::unordered_set<std::string> initialized_members;
    for (auto &[name, value] : expr.members)
    {
        check_expr(*value);

        auto field = struct_decl->get_field(name);
        if (!field)
        {
            add_error("No member '" + name + "' in struct " + expr.struct_name);
            continue;
        }
        // Find struct field
        const Type *field_type = field->type.get();
        if (!field_type)
        {
            add_error("No type for member '" + name + "' in struct " + expr.struct_name);
            continue;
        }

        // Check type compatibility
        if (!is_convertible(*value->type, *field_type))
        {
            add_error("Type mismatch initializing member '" + name + "'");
        }

        initialized_members.insert(name);
    }

    // Check missing required members
    auto struct_type = struct_decl->type()->as_struct();
    MO_ASSERT(struct_type, "Struct type must be a struct");
    MO_DEBUG("struct_type has %zu members", struct_type->member_count());
    for (size_t i = 0; i < struct_type->member_count(); ++i)
    {
        const auto &member = struct_type->get_member(i); // no error
        printf("member ptr: %p\n", &member);
        printf("mem.type ptr: %p\n", &member.type);
        printf("type %s\n", member.type->to_string().c_str()); // Segmentation fault.
        printf("member %s\n", member.name.c_str());            // Segmentation fault.
        if (!initialized_members.count(member.name))           // Segmentation fault.
        {
            add_error("Missing initialization for member '" + member.name + "'");
        }
    }

    expr.type = struct_decl->type()->clone();
    expr.expr_category = Expr::Category::RValue;
}

// Alias resolution
TypePtr TypeChecker::resolve_alias(const Type &type) const
{
    if (!type.is_alias())
    {
        return type.clone();
    }

    std::unordered_set<std::string> visited;
    TypePtr current = type.clone();

    while (auto alias = current->as_alias())
    {
        const std::string &name = alias->name();

        // loop ref dectect
        if (visited.count(name))
        {
            add_error("Detected cyclic alias: ", name);
            return nullptr;
        }
        visited.insert(name);

        // resolve target
        TypePtr resolved = current_scope_->resolve_type(name);
        if (!resolved)
        {
            add_error("Unresolved alias: ", name);
            return nullptr;
        }
        current = std::move(resolved->clone());
    }

    if (auto fin = current->as_alias())
    {
        add_error("Alias is still unresolved: ", fin->name());
        return nullptr;
    }

    return current;
}

StructDecl *TypeChecker::find_struct(const std::string &name) const
{
    // TODO: make this more efficient by storing a map of struct names to decls
    for (const auto &struct_decl : program_->structs)
    {
        if (static_cast<StructType *>(struct_decl->type())->name() == name)
        {
            return struct_decl.get();
        }
    }
    return nullptr;
}

// Break statement
void TypeChecker::visit(BreakStmt &stmt)
{
    if (loop_depth_ <= 0)
    {
        add_error("Break statement outside loop context");
    }
}

// Continue statement
void TypeChecker::visit(ContinueStmt &stmt)
{
    if (loop_depth_ <= 0)
    {
        add_error("Continue statement outside loop context");
    }
}

void TypeChecker::visit(WhileStmt &stmt)
{
    // Check the condition expression
    check_expr(*stmt.condition);

    // Enter loop context
    loop_depth_++;
    check_stmt(*stmt.body);
    loop_depth_--;
    assert(loop_depth_ >= 0);

    // Ensure the condition is of a numeric type (int or float)
    if (stmt.condition->type)
    {
        is_valid_cond_type(*stmt.condition->type);
    }
}

void TypeChecker::visit(IfStmt &stmt)
{
    // Check the condition expression
    check_expr(*stmt.condition);

    // Check the 'then' branch
    check_stmt(*stmt.then_branch);

    // Check the 'else' branch if it exists
    if (stmt.else_branch)
    {
        check_stmt(*stmt.else_branch);
    }

    // Ensure the condition is of a numeric type (int or float)
    if (stmt.condition->type)
    {
        is_valid_cond_type(*stmt.condition->type);
    }
}
