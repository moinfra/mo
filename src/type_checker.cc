// type_checker.cc
#include "type_checker.h"
#include "lexer.h"
#include <cassert>
#include <algorithm>
#include <unordered_set>

using namespace ast;

// Find a variable in the current or parent scopes
TypePtr Scope::find(const std::string &name) const
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot find empty name");
    }

    auto var_it = variables_.find(name);
    if (var_it != variables_.end())
    {
        return var_it->second->clone();
    }

    if (parent)
    {
        return parent->find(name);
    }

    return nullptr;
}

// Insert a variable into the current scope
bool Scope::insert(std::string name, TypePtr type)
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot insert empty name");
    }
    if (!type)
    {
        throw std::invalid_argument("Type cannot be null");
    }

    if (variables_.find(name) != variables_.end() || types_.find(name) != types_.end())
    {
        return false; // Variable or type already exists
    }

    variables_[name] = std::move(type);
    return true;
}

// Resolve a type in the current or parent scopes
TypePtr Scope::resolve_type(const std::string &name) const
{
    if (name.empty())
    {
        throw std::invalid_argument("Cannot resolve empty name");
    }

    auto type_it = types_.find(name);
    if (type_it != types_.end())
    {
        return type_it->second->clone();
    }

    if (parent)
    {
        return parent->resolve_type(name);
    }

    return nullptr;
}

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
        global_scope_->insert(global->name, global->type->clone());
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
    current_scope_ = current_scope_->parent;
}

// Error reporting
void TypeChecker::add_error(const std::string &message)
{
    errors_.push_back(message);
}

// Type system implementation. NOTE: alias kind t1 will be resolved
bool TypeChecker::types_equal(const Type &t1, const Type &t2) const
{
    if (t1.kind() != t2.kind())
        return false;

    switch (t1.kind())
    {
    case Type::Kind::Placeholder:
        return true;
    case Type::Kind::Void:
        return true;
    case Type::Kind::Int:
    {
        auto i1 = static_cast<const IntegerType *>(&t1);
        auto i2 = static_cast<const IntegerType *>(&t2);
        return i1->bit_width() == i2->bit_width();
    }
    case Type::Kind::Float:
    {
        auto f1 = static_cast<const FloatType *>(&t1);
        auto f2 = static_cast<const FloatType *>(&t2);
        return f1->precision() == f2->precision();
    }
    case Type::Kind::Bool:
        return true;
    case Type::Kind::String:
        return true;
    case Type::Kind::Pointer:
    {
        auto p1 = static_cast<const PointerType *>(&t1);
        auto p2 = static_cast<const PointerType *>(&t2);
        return types_equal(p1->pointee(), p2->pointee());
    }
    case Type::Kind::Array:
    {
        auto a1 = static_cast<const ArrayType *>(&t1);
        auto a2 = static_cast<const ArrayType *>(&t2);
        return a1->size() == a2->size() &&
               types_equal(a1->element_type(), a2->element_type());
    }
    case Type::Kind::Tuple:
    {
        auto t1_tuple = static_cast<const TupleType *>(&t1);
        auto t2_tuple = static_cast<const TupleType *>(&t2);
        if (t1_tuple->element_types().size() != t2_tuple->element_types().size())
            return false;
        return std::equal(t1_tuple->element_types().begin(), t1_tuple->element_types().end(),
                          t2_tuple->element_types().begin(),
                          [this](const TypePtr &t1, const TypePtr &t2)
                          {
                              return types_equal(*t1, *t2);
                          });
    }
    case Type::Kind::Struct:
    {
        auto s1 = static_cast<const StructType *>(&t1);
        auto s2 = static_cast<const StructType *>(&t2);
        return s1->name() == s2->name();
    }
    case Type::Kind::Function:
    {
        auto f1 = static_cast<const FunctionType *>(&t1);
        auto f2 = static_cast<const FunctionType *>(&t2);
        if (!types_equal(f1->return_type(), f2->return_type()))
            return false;
        if (f1->params().size() != f2->params().size())
            return false;
        for (size_t i = 0; i < f1->params().size(); ++i)
        {
            if (!types_equal(*f1->params()[i], *f2->params()[i]))
                return false;
        }
        return true;
    }
    case Type::Kind::Alias:
    {
        auto a1 = static_cast<const AliasType *>(&t1);
        return types_equal(a1->target(), t2);
    }
    case Type::Kind::Qualified:
    {
        auto q1 = static_cast<const QualifiedType *>(&t1);
        auto q2 = static_cast<const QualifiedType *>(&t2);
        return q1->qualifiers() == q2->qualifiers() && types_equal(q1->base_type(), q2->base_type());
    }
    }

    return false;
}

bool TypeChecker::is_valid_lvalue(Expr &expr)
{
    return dynamic_cast<VariableExpr *>(&expr) ||
           dynamic_cast<MemberAccessExpr *>(&expr) ||
           dynamic_cast<ArrayAccessExpr *>(&expr) ||
           dynamic_cast<DerefExpr *>(&expr);
}

// Expression checking entry point
void TypeChecker::check_expr(Expr &expr)
{
    // Dispatch to specific visitor
    if (auto v = dynamic_cast<VariableExpr *>(&expr))
        visit(*v);
    else if (auto i = dynamic_cast<IntegerLiteralExpr *>(&expr))
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
    else if (auto f = dynamic_cast<FunctionPointerExpr *>(&expr))
        visit(*f);
    else
    {
        assert(false && "Unknown expression type");
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

    // assert(expr.type && "Expression has no type");
}

// Cast expression
void TypeChecker::visit(CastExpr &expr)
{
    check_expr(*expr.expr);

    if (!expr.expr->type || !expr.target_type)
    {
        add_error("Invalid cast expression");
        return;
    }

    if (!is_convertible(*expr.expr->type, *expr.target_type))
    {
        add_error("Invalid type conversion in cast");
    }

    expr.type = expr.target_type->clone();
    expr.expr_category = Expr::Category::RValue;
}

void TypeChecker::visit(SizeofExpr &expr)
{
    switch (expr.kind)
    {
    case SizeofExpr::Kind::Type:
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
    expr.type = Type::create_int();
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

    if (auto type = current_scope_->find(expr.name))
    {
        expr.type = type->clone();
    }
    else
    {
        add_error("Undefined symbol '" + expr.name + "'");
        expr.type = Type::create_placeholder();
    }
}

void TypeChecker::visit(IntegerLiteralExpr &expr)
{
    expr.type = Type::create_int();
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

    if (!current_scope_->types_.emplace(decl.name, decl.type()).second)
    {
        add_error("Duplicate struct name: " + decl.name);
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

    if (!global_scope_->insert(decl.name, decl.type->clone()))
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
    // Register alias in current scope
    // Ensure that the alias name does not already exist
    if (!current_scope_->types_.emplace(decl.name, decl.type->clone()).second)
    {
        add_error("Duplicate type name for alias: " + decl.name);
    }
}

// Binary expression type rules
void TypeChecker::visit(BinaryExpr &expr)
{
    check_expr(*expr.left);
    check_expr(*expr.right);

    if (!expr.left->type || !expr.right->type)
        return;

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
    // Arithemetic operators
    case TokenType::Plus:
    case TokenType::Minus:
    case TokenType::Star:
    case TokenType::Slash:
    {
        if (expr.left->type->kind() == Type::Kind::Int && expr.right->type->kind() == Type::Kind::Int)
        {
            expr.type = Type::create_int();
        }
        else if (expr.left->type->kind() == Type::Kind::Float && expr.right->type->kind() == Type::Kind::Float)
        {
            expr.type = Type::create_float();
        }
        else
        {
            add_error("Invalid operands for arithmetic operation");
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
    // case TokenType::Pipe:
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
        add_error("Unsupported binary operator");
        break;
    }
    }
}

// type_checker.cpp 续
void TypeChecker::visit(UnaryExpr &expr)
{
    check_expr(*expr.operand);
    if (!expr.operand->type)
        return;

    switch (expr.op)
    {
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

    default:
        add_error("Unsupported unary operator");
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
        auto &param_type = function_type->params()[i];
        if (!arg_type || !param_type)
            continue;

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
    if (current_scope_->find(stmt.name))
    {
        add_error("Redeclaration of variable: " + stmt.name);
        return;
    }

    // Register the variable in the current scope
    assert(!stmt.name.empty() && "Variable name must be set");
    if (!current_scope_->insert(stmt.name, stmt.type->clone()))
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
        if (!current_return_type_)
        {
            add_error("Return in non-function context");
        }
        else if (!is_convertible(*stmt.value->type, *current_return_type_))
        {
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
    auto prev_return_type = current_return_type_;
    current_return_type_ = func.return_type.get();

    // To handle recursive function calls, we need to add current function to the
    // current scope before checking its body.
    assert(!func.name.empty() && "Function name must be set");
    if (!current_scope_->insert(func.name, func.type()))
    {
        add_error("Duplicate function name: " + func.name);
    }

    push_scope();

    // Add parameters to scope
    for (auto &param : func.params)
    {
        assert(!param.name.empty() && "Parameter name must be set");
        if (!current_scope_->insert(param.name, param.type->clone()))
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
        return;

    if (expr.object->type->kind() != Type::Kind::Struct)
    {
        add_error("Member access on non-struct type");
        return;
    }

    auto struct_type = static_cast<StructType *>(expr.object->type.get());

    // Find struct declaration
    StructDecl *struct_decl = find_struct(struct_type->name());

    if (!struct_decl)
    {
        add_error("Undefined struct type: " + struct_type->name());
        return;
    }

    // if followed by call, prioritize method call over function pointer member call
    if (expr.is_call)
    {
        // Find method
        auto method = struct_decl->get_method(expr.member);
        if (!method)
        {
            debug("No method '%s' in struct '%s'", expr.member.c_str(), struct_type->name().c_str());
            debug("Falling back to function pointer member access");
        }
        else
        {
            // Check method signature
            if (method->params.size() != expr.args.size())
            {
                add_error("Argument count mismatch in method call");
            }
            for (size_t i = 0; i < expr.args.size(); ++i)
            {
                auto &arg_type = expr.args[i]->type;
                auto &param_type = method->params[i].type;

                if (!is_convertible(*arg_type, *param_type))
                {
                    add_error("Argument type mismatch in method call");
                }
            }
            expr.resolved_func = method;
            expr.type = method->return_type->clone();
            expr.expr_category = Expr::Category::RValue;
            return;
        }
    }

    // Find member
    Type *member_type = struct_decl->get_field(expr.member)->type.get();
    if (!member_type)
    {
        add_error("No member '" + expr.member + "' in struct '" + struct_type->name() + "'");
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

// Enhanced type conversion rules
bool TypeChecker::is_convertible(const Type &from, const Type &to) const
{
    if (types_equal(from, to))
        return true;

    if (from.kind() == Type::Kind::Int && to.kind() == Type::Kind::Float)
    {
        return true;
    }

    // Array to pointer decay
    if (from.kind() == Type::Kind::Array &&
        to.kind() == Type::Kind::Pointer)
    {
        auto array_type = static_cast<const ArrayType *>(&from);
        auto pointer_type = static_cast<const PointerType *>(&to);
        return types_equal(array_type->element_type(), pointer_type->pointee());
    }

    // Null pointer conversion
    if (from.kind() == Type::Kind::Int && static_cast<const IntegerType &>(from).bit_width() == MO_DEFAULT_INT_BITWIDTH && to.kind() == Type::Kind::Pointer)
    {
        return true; // Allow 0 to null pointer conversion
    }

    // Void pointer conversions
    if (from.kind() == Type::Kind::Pointer && to.kind() == Type::Kind::Pointer)
    {
        auto from_pointer = static_cast<const PointerType *>(&from);
        auto to_pointer = static_cast<const PointerType *>(&to);

        const bool from_void = from_pointer->pointee().kind() == Type::Kind::Void;
        const bool to_void = to_pointer->pointee().kind() == Type::Kind::Void;
        return from_void || to_void || types_equal(from_pointer->pointee(), to_pointer->pointee());
    }

    // Conversion from alias to its target type
    if (from.kind() == Type::Kind::Alias)
    {
        auto alias_type = static_cast<const AliasType *>(&from);
        return is_convertible(alias_type->target(), to);
    }

    // Conversion to alias from its target type
    if (to.kind() == Type::Kind::Alias)
    {
        auto alias_type = static_cast<const AliasType *>(&to);
        return is_convertible(from, alias_type->target());
    }

    return false;
}

// type_checker.cpp 续
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

        // Find struct field
        const Type *field_type = struct_decl->get_field(name)->type.get();
        if (!field_type)
        {
            add_error("No member '" + name + "' in struct " + expr.struct_name);
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
    auto struct_type = static_cast<StructType *>(struct_decl->type().get());
    for (size_t i = 0; i < struct_type->member_count(); ++i)
    {
        const auto &member = struct_type->get_member(i);
        if (!initialized_members.count(member.name))
        {
            add_error("Missing initialization for member '" + member.name + "'");
        }
    }

    expr.type = struct_decl->type()->clone();
    expr.expr_category = Expr::Category::RValue;
}

// Alias resolution
TypePtr TypeChecker::resolve_alias(const std::string &name) const
{
    auto type = current_scope_->resolve_type(name);
    if (!type)
        return nullptr;

    while (type->kind() == Type::Kind::Alias)
    {
        auto alias_type = static_cast<AliasType *>(type.get());
        type = current_scope_->resolve_type(alias_type->name());
        if (!type)
            break;
    }
    return type;
}

StructDecl *TypeChecker::find_struct(const std::string &name) const
{
    // TODO: make this more efficient by storing a map of struct names to decls
    for (const auto &struct_decl : program_->structs)
    {
        if (static_cast<StructType *>(struct_decl->type().get())->name() == name)
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
        bool isInt = stmt.condition->type->kind() == Type::Kind::Int;
        bool isFloat = stmt.condition->type->kind() == Type::Kind::Float;

        if (!isInt && !isFloat)
        {
            add_error("Loop condition must be of numeric type (int or float)");
        }
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
        bool isInt = stmt.condition->type->kind() == Type::Kind::Int;
        bool isFloat = stmt.condition->type->kind() == Type::Kind::Float;

        if (!isInt && !isFloat)
        {
            add_error("Condition must be of numeric type (int or float)");
        }
    }
}
