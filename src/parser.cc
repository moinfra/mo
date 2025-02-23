// src/parser.cc
#include "parser.h"
#include <cassert>
#include <unordered_set>

Parser::Parser(Lexer &&lexer) : lexer_(std::move(lexer)), current_(lexer_.next_token())
{
    init_pratt_rules();
}

ExprPtr Parser::parse_expr()
{
    return parse_expression();
}

void Parser::init_pratt_rules()
{
    auto add_rule = [&](TokenType type, int prec,
                        std::function<ExprPtr()> prefix,
                        std::function<ExprPtr(ExprPtr)> infix)
    {
        pratt_rules_[type] = {prec, std::move(prefix), std::move(infix)};
    };

    // 前缀表达式
    add_rule(TokenType::Identifier, 0, [&]
             { return parse_identifier(); }, nullptr);
    add_rule(TokenType::IntegerLiteral, 0, [&]
             { return parse_literal(); }, nullptr);
    add_rule(TokenType::FloatLiteral, 0, [&]
             { return parse_literal(); }, nullptr);
    add_rule(TokenType::LParen, 0, [&]
             { return parse_grouped(); }, nullptr);
    add_rule(TokenType::Cast, 0, [&]
             { return parse_cast(); }, nullptr);
    add_rule(TokenType::Sizeof, 0, [&]
             { return parse_sizeof(); }, nullptr);
    add_rule(TokenType::LBrace, 0, [&]
             { return parse_init_list(); }, nullptr);

    // 中缀表达式
    add_rule(TokenType::Plus, 10, nullptr, [&](ExprPtr l)
             { return parse_binary(std::move(l), 10); });
    add_rule(TokenType::Star, 20, nullptr, [&](ExprPtr l)
             { return parse_binary(std::move(l), 20); });
    add_rule(TokenType::Dot, 30, nullptr, [&](ExprPtr l)
             { return parse_member_access(std::move(l)); });
    add_rule(TokenType::Arrow, 30, nullptr, [&](ExprPtr l)
             { return parse_member_access(std::move(l)); });
    add_rule(TokenType::LParen, 40, nullptr, [&](ExprPtr l)
             { return parse_call(std::move(l)); });
}

ExprPtr Parser::parse_expression(int precedence)
{
    auto token_type = current_.type;
    auto &rule = pratt_rules_.at(token_type);
    if (!rule.prefix)
    {
        error("Unexpected token in expression");
        return nullptr;
    }

    auto left = rule.prefix();

    while (precedence < get_precedence(current_.type))
    {
        auto &infix_rule = pratt_rules_.at(current_.type);
        if (!infix_rule.infix)
            break;

        left = infix_rule.infix(std::move(left));
    }

    return left;
}

ExprPtr Parser::parse_member_access(ExprPtr left)
{
    auto expr = std::make_unique<MemberAccessExpr>();
    expr->object = std::move(left);
    expr->accessor = current_.type;

    advance(); // Skip . or ->
    expr->member = current_.lexeme;
    consume(TokenType::Identifier, "Expected member name after access operator");

    // 处理链式访问 Handle chained member access
    if (current_.type == TokenType::Dot ||
        current_.type == TokenType::Arrow)
    {
        return parse_expression(get_precedence(expr->accessor));
    }

    return expr;
}

ExprPtr Parser::parse_call(ExprPtr left)
{
    auto call_expr = std::make_unique<CallExpr>();
    call_expr->callee = std::move(left);

    consume(TokenType::LParen, "Expected '(' after function name");

    // Parse arguments
    if (current_.type != TokenType::RParen)
    {
        do
        {
            call_expr->args.push_back(parse_expression());
        } while (match(TokenType::Comma));
    }

    consume(TokenType::RParen, "Expected ')' after arguments");

    return call_expr;
}

void Parser::consume(TokenType type, const std::string &message)
{
    if (current_.type == type)
    {
        Token consumed = current_;
        advance();
        // return consumed;
    }
    else
    {
        error(message);
    }
}

bool Parser::match(TokenType type)
{
    if (current_.type == type)
    {
        advance();
        return true;
    }
    return false;
}

std::unique_ptr<BlockStmt> Parser::parse_block()
{
    auto block = std::make_unique<BlockStmt>();

    consume(TokenType::LBrace, "Expected '{' at the start of block");

    while (current_.type != TokenType::RBrace && current_.type != TokenType::Eof)
    {
        block->statements.push_back(parse_statement());
    }

    consume(TokenType::RBrace, "Expected '}' at the end of block");

    return block;
}

StructDecl Parser::parse_struct_decl()
{
    StructDecl struct_decl;

    consume(TokenType::Struct, "Expected 'struct' keyword");
    struct_decl.name = current_.lexeme;
    consume(TokenType::Identifier, "Expected struct name");

    consume(TokenType::LBrace, "Expected '{' at the start of struct body");

    while (current_.type != TokenType::RBrace && current_.type != TokenType::Eof)
    {
        struct_decl.add_field(parse_struct_member());
    }

    consume(TokenType::RBrace, "Expected '}' at the end of struct body");

    return struct_decl;
}

TypedField Parser::parse_struct_member()
{
    // Parse the type of the struct member
    TypePtr type = parse_type();

    // Parse the name of the struct member
    std::string name = current_.lexeme;
    consume(TokenType::Identifier, "Expected struct member name");

    // Consume the semicolon at the end of the struct member declaration
    consume(TokenType::Semicolon, "Expected ';' after struct member declaration");

    // Return the struct member
    return TypedField(std::move(type), name);
}

std::unique_ptr<Type> Parser::parse_type()
{
    auto type = std::make_unique<Type>();

    // Handle const qualification
    if (match(TokenType::Const))
    {
        type->is_const = true;
        advance();
    }

    // Handle pointer type
    while (match(TokenType::Star))
    {
        auto ptr_type = std::make_unique<Type>();
        ptr_type->kind = Type::Kind::Pointer;
        ptr_type->pointee = std::move(type);
        type = std::move(ptr_type);
        advance();
    }

    // Basic type or struct type
    if (match(TokenType::Identifier))
    {
        type->kind = Type::Kind::Basic;
        type->name = current_.lexeme;
        advance();
    }
    else if (match(TokenType::Fn))
    {
        // Function type
        type->kind = Type::Kind::Function;
        advance();
        consume(TokenType::LParen, "Expected '(' in function type");

        // Param list
        while (!match(TokenType::RParen))
        {
            type->params.push_back(parse_type());
            if (!match(TokenType::Comma))
                break;
            advance();
        }
        consume(TokenType::RParen, "Expected ')' in function type");

        // Return type
        if (match(TokenType::Arrow))
        {
            advance();
            type->return_type = parse_type();
        }
    }
    else if (match(TokenType::Struct))
    {
        // Anonymous struct type
        type->kind = Type::Kind::Struct;
        advance();
        consume(TokenType::LBrace, "Expected '{' in struct type");

        while (!match(TokenType::RBrace))
        {
            std::string field_name = current_.lexeme;
            consume(TokenType::Identifier, "Expected field name");
            consume(TokenType::Colon, "Expected ':' after field name");
            type->members[field_name] = parse_type();
            if (!match(TokenType::Comma))
                break;
            advance();
        }
        consume(TokenType::RBrace, "Expected '}' in struct type");
    }

    // Dimension of array type
    if (match(TokenType::LBracket))
    {
        advance();
        auto array_type = std::make_unique<Type>();
        array_type->kind = Type::Kind::Array;
        array_type->element_type = std::move(type);
        if (match(TokenType::IntegerLiteral))
        {
            array_type->array_size = std::stoi(current_.lexeme);
            advance();
        }
        type = std::move(array_type);
        consume(TokenType::RBracket, "Expected ']' in array type");
    }

    return type;
}

ExprPtr Parser::parse_function_pointer_expr()
{
    consume(TokenType::Fn, "Expected 'fn'");
    auto expr = std::make_unique<FunctionPointerExpr>();

    consume(TokenType::LParen, "Expected '('");
    while (!match(TokenType::RParen))
    {
        expr->param_types.push_back(parse_type());
        if (!match(TokenType::Comma))
            break;
        advance();
    }
    consume(TokenType::RParen, "Expected ')'");

    if (match(TokenType::Arrow))
    {
        advance();
        expr->return_type = parse_type();
    }

    return expr;
}

ExprPtr Parser::parse_struct_literal()
{
    consume(TokenType::Struct, "Expected 'struct'");
    auto expr = std::make_unique<StructLiteralExpr>();

    if (match(TokenType::Identifier))
    {
        expr->struct_name = current_.lexeme;
        advance();
    }

    consume(TokenType::LBrace, "Expected '{'");
    std::unordered_set<std::string> seen_fields;

    while (!match(TokenType::RBrace))
    {
        std::string name = current_.lexeme;
        consume(TokenType::Identifier, "Expected field name");

        if (seen_fields.count(name))
        {
            error("Duplicate field '" + name + "' in struct literal");
        }
        seen_fields.insert(name);

        consume(TokenType::Colon, "Expected ':'");
        ExprPtr value = parse_expression();

        expr->add_member(name, std::move(value));

        if (!match(TokenType::Comma))
            break;
        advance();
    }

    consume(TokenType::RBrace, "Expected '}'");
    return expr;
}

void Parser::synchronize_type()
{
    while (current_.type != TokenType::Eof)
    {
        switch (current_.type)
        {
        case TokenType::Semicolon:
        case TokenType::RParen:
        case TokenType::RBrace:
        case TokenType::RBracket:
            return;
        case TokenType::Let:
        case TokenType::Fn:
        case TokenType::Struct:
            return;
        default:
            advance();
        }
    }
}

std::unique_ptr<Type> Parser::parse_type_safe()
{
    try
    {
        return parse_type();
    }
    catch (const ParseError &)
    {
        synchronize_type();
        // Placeholder
        return std::make_unique<Type>();
    }
}

ExprPtr Parser::parse_identifier()
{
    auto expr = std::make_unique<VariableExpr>(current_.lexeme);
    advance();
    return expr;
}
ExprPtr Parser::parse_literal()
{
    ExprPtr expr;
    switch (current_.type)
    {
    case TokenType::IntegerLiteral:
        expr = std::make_unique<IntegerLiteralExpr>(std::stoi(current_.lexeme));
        break;
    case TokenType::FloatLiteral:
        expr = std::make_unique<FloatLiteralExpr>(std::stof(current_.lexeme));
        break;
    case TokenType::StringLiteral:
        expr = std::make_unique<StringLiteralExpr>(current_.lexeme);
        break;
    default:
        error("Unexpected literal in expression");
        break;
    }
    advance();
    return expr;
}

ExprPtr Parser::parse_grouped()
{
    advance();
    auto expr = parse_expression();
    consume(TokenType::RParen, "Expected ')' after expression");
    return expr;
}

ExprPtr Parser::parse_cast()
{
    consume(TokenType::Cast, "Expected 'cast'");
    consume(TokenType::LParen, "Expected '(' after 'cast'");
    auto expr = std::make_unique<CastExpr>();
    expr->target_type = parse_type();
    consume(TokenType::RParen, "Expected ')' after cast type");
    expr->expr = parse_expression();
    return expr;
}

ExprPtr Parser::parse_sizeof()
{
    consume(TokenType::Sizeof, "Expected'sizeof'");
    consume(TokenType::LParen, "Expected '(' after'sizeof'");
    auto expr = std::make_unique<SizeofExpr>();
    expr->target_type = parse_type_safe();
    consume(TokenType::RParen, "Expected ')' after sizeof type");
    return expr;
}

ExprPtr Parser::parse_init_list()
{
    consume(TokenType::LBrace, "Expected '{' for initializer list");

    auto init_list = std::make_unique<InitListExpr>();

    while (current_.type != TokenType::RBrace && current_.type != TokenType::Eof)
    {
        init_list->members.push_back(parse_expression());

        if (current_.type == TokenType::Comma)
        {
            advance(); // Consume the comma
        }
        else if (current_.type != TokenType::RBrace)
        {
            throw std::runtime_error("Expected ',' or '}' in initializer list");
        }
    }

    consume(TokenType::RBrace, "Expected '}' at the end of initializer list");

    return init_list;
}

ExprPtr Parser::parse_binary(ExprPtr left, int min_precedence)
{
    while (true)
    {
        int precedence = get_precedence(current_.type);

        if (precedence < min_precedence)
        {
            return left;
        }

        Token op = current_;
        advance();

        ExprPtr right = parse_unary();

        int next_precedence = get_precedence(current_.type);

        if (precedence < next_precedence)
        {
            right = parse_binary(std::move(right), precedence + 1);
        }

        left = std::make_unique<BinaryExpr>(op.type, std::move(left), std::move(right));
    }
}

int Parser::get_precedence(TokenType type)
{
    switch (type)
    {
    case TokenType::Plus:
    case TokenType::Minus:
        return 1;
    case TokenType::Star:
    case TokenType::Divide:
        return 2;
    // case TokenType::Caret: // Exponentiation
    //     return 3;
    default:
        return 0; // Default precedence for non-operators
    }
}
ExprPtr Parser::parse_unary()
{
    // Check if the current token is a unary operator
    if (current_.type == TokenType::Minus || current_.type == TokenType::Plus)
    {
        Token op = current_;
        advance(); // Consume the unary operator

        // Parse the operand (which could be another unary expression or a primary expression)
        ExprPtr operand = parse_unary();

        // Return a unary expression node
        return std::make_unique<UnaryExpr>(op.type, std::move(operand));
    }

    // If it's not a unary operator, parse a primary expression
    return parse_primary();
}

ExprPtr Parser::parse_primary()
{
    // Check the type of the current token and parse accordingly
    if (current_.type == TokenType::IntegerLiteral)
    {
        int value = std::stoi(current_.lexeme);
        advance();
        return std::make_unique<IntegerLiteralExpr>(value);
    }
    else if (current_.type == TokenType::FloatLiteral)
    {
        float value = std::stof(current_.lexeme);
        advance();
        return std::make_unique<FloatLiteralExpr>(value);
    }
    else if (current_.type == TokenType::StringLiteral)
    {
        std::string value = current_.lexeme;
        advance();
        return std::make_unique<StringLiteralExpr>(value);
    }
    else if (current_.type == TokenType::Identifier)
    {
        std::string name = current_.lexeme;
        advance();
        return std::make_unique<VariableExpr>(name);
    }
    else if (current_.type == TokenType::LParen)
    {
        advance();
        ExprPtr expr = parse_expression();
        consume(TokenType::RParen, "Expect ')' after expression.");
        return expr;
    }
    else
    {
        error("Unexpected token in primary expression.");
        return nullptr;
    }
}

StmtPtr Parser::parse_statement()
{
    if (current_.type == TokenType::Let || current_.type == TokenType::Const)
    {
        VarDeclStmt stmt = parse_var_decl();
        return std::make_unique<VarDeclStmt>(std::move(stmt));
    }
    else if (current_.type == TokenType::Return)
    {
        return parse_return();
    }
    else if (current_.type == TokenType::If)
    {
        return parse_if();
    }
    else
    {
        error("Unexpected statement.");
        return nullptr;
    }
}

StmtPtr Parser::parse_return()
{
    advance(); // Move past the 'return' keyword
    ExprPtr expr = nullptr;
    if (current_.type != TokenType::Semicolon)
    {
        expr = parse_expression();
    }
    consume(TokenType::Semicolon, "Expect ';' after return value.");
    return std::make_unique<ReturnStmt>(std::move(expr));
}

StmtPtr Parser::parse_if()
{
    advance(); // Move past the 'if' keyword
    consume(TokenType::LParen, "Expect '(' after 'if'.");
    ExprPtr condition = parse_expression();
    consume(TokenType::RParen, "Expect ')' after condition.");
    StmtPtr then_branch = parse_statement();
    StmtPtr else_branch = nullptr;
    if (match(TokenType::Else))
    {
        else_branch = parse_statement();
    }
    return std::make_unique<IfStmt>(std::move(condition), std::move(then_branch), std::move(else_branch));
}

VarDeclStmt Parser::parse_var_decl()
{
    VarDeclStmt stmt;

    if (current_.type == TokenType::Const)
    {
        stmt.is_const = true;
        advance();
    }
    else
    {
        consume(TokenType::Let, "Expected 'let' or 'const'");
    }

    stmt.name = current_.lexeme;
    consume(TokenType::Identifier, "Expected variable name");

    if (match(TokenType::Colon))
    {
        advance();
        stmt.type = parse_type(); // Assuming parse_type returns a Type
    }

    if (match(TokenType::Assign))
    {
        advance();
        stmt.init_expr = parse_expression();
    }

    consume(TokenType::Semicolon, "Expected ';' after declaration");
    return stmt;
}

FunctionDecl Parser::parse_function_decl()
{
    FunctionDecl func;
    consume(TokenType::Fn, "Expected 'fn'");

    func.name = current_.lexeme;
    consume(TokenType::Identifier, "Expected function name");

    // 解析参数
    consume(TokenType::LParen, "Expected (");
    while (!match(TokenType::RParen))
    {
        std::string name = current_.lexeme;
        consume(TokenType::Identifier, "Expected parameter name");
        consume(TokenType::Colon, "Expected : after parameter name");
        auto type = parse_type();
        func.params.emplace_back(std::move(type), name);

        if (!match(TokenType::Comma))
            break;
        advance();
    }
    consume(TokenType::RParen, "Expected )");

    // Return type
    if (match(TokenType::Arrow))
    {
        advance();
        func.return_type = parse_type();
    }

    // Receiver
    if (func.is_method && match(TokenType::LParen))
    {
        /*1. `self: Type`
        2. `self: *Type`
        3. `&self` */
        advance();
        consume(TokenType::Identifier, "Expected self parameter");
        consume(TokenType::Colon, "Expected : in receiver");
        std::unique_ptr<Type> receiver_type = parse_type();
        func.receiver_type = std::make_unique<Type>(*receiver_type);
        consume(TokenType::RParen, "Expected ) in receiver");
        func.is_method = true;
        func.add_param("self", std::move(receiver_type));
    }

    StmtPtr body = parse_block();
    if (auto blockStmt = dynamic_cast<BlockStmt *>(body.get()))
    {
        func.body = std::move(blockStmt->statements);
    }
    else
    {
        error("Expected block statement after function body");
    }
    return func;
}

void Parser::error(const std::string &message) const
{
    throw ParseError{current_.start_line, current_.start_col, message};
}

void Parser::advance()
{
    // previous_ = current_;
    current_ = lexer_.next_token();
}

void Parser::synchronize()
{
    while (current_.type != TokenType::Eof)
    {
        if (current_.type == TokenType::Semicolon)
        {
            advance();
            return;
        }

        switch (current_.type)
        {
        case TokenType::Struct:
        case TokenType::Impl:
        case TokenType::Fn:
        case TokenType::Let:
            return;
        default:
            advance();
        }
    }
}

Program Parser::parse()
{
    Program program;

    while (current_.type != TokenType::Eof)
    {
        try
        {
            if (match(TokenType::Struct))
            {
                program.structs.push_back(std::make_unique<StructDecl>(parse_struct_decl()));
            }
            else if (match(TokenType::Impl))
            {
                program.impl_blocks.push_back(std::make_unique<ImplBlock>(parse_impl_block()));
            }
            else if (match(TokenType::Fn))
            {
                program.functions.push_back(std::make_unique<FunctionDecl>(parse_function_decl()));
            }
            else if (match(TokenType::Let))
            {
                program.globals.push_back(std::make_unique<GlobalDecl>(parse_global_decl()));
            }
            else
            {
                error("Unexpected token at top level: " + current_.lexeme);
                synchronize();
            }
        }
        catch (const ParseError &e)
        {
            errors_.push_back(e.what());
            synchronize();
        }
    }

    return program;
}

FunctionDecl Parser::parse_method()
{
    FunctionDecl method = parse_function_decl();
    method.is_method = true;
    return method;
}

ImplBlock Parser::parse_impl_block()
{
    consume(TokenType::Impl, "Expected 'impl'");

    ImplBlock impl;
    impl.target_type = parse_type();

    consume(TokenType::LBrace, "Expected '{' after impl type");

    while (!match(TokenType::RBrace))
    {
        impl.methods.push_back(std::make_unique<FunctionDecl>(parse_method()));
    }

    consume(TokenType::RBrace, "Expected '}' to close impl block");
    return impl;
}

// 解析全局变量声明
GlobalDecl Parser::parse_global_decl()
{
    auto decl = GlobalDecl(parse_var_decl());

    if (!decl.is_const && !decl.init_expr)
    {
        error("Global variable must be const or have initializer");
    }
    return decl;
}
