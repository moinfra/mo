// src/parser.cc
#include "parser.h"
#include <cassert>
#include <unordered_set>
#include <algorithm>
#include <iostream>

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
        // Add the new rule (std::set will automatically avoid duplicates)
        pratt_rules_[type].insert({prec, std::move(prefix), std::move(infix)});
    };

    // Prefix rules
    add_rule(TokenType::Identifier, 0, [&]
             { return parse_identifier(); }, nullptr);
    add_rule(TokenType::IntegerLiteral, 0, [&]
             { return parse_literal(); }, nullptr);
    add_rule(TokenType::FloatLiteral, 0, [&]
             { return parse_literal(); }, nullptr);
    add_rule(TokenType::StringLiteral, 0, [&]
             { return parse_literal(); }, nullptr);
    add_rule(TokenType::Ampersand, 0, [&]
             { return parse_address_of(); }, nullptr);
    add_rule(TokenType::Star, 0, [&]
             { return parse_deref(); }, nullptr);
    // add_rule(TokenType::Minus, 0, [&]
    //          { return parse_prefix_expr(); }, nullptr);
    add_rule(TokenType::LParen, 0, [&]
             { return parse_grouped(); }, nullptr);
    add_rule(TokenType::Cast, 0, [&]
             { return parse_cast(); }, nullptr);
    add_rule(TokenType::Sizeof, 0, [&]
             { return parse_sizeof(); }, nullptr);
    add_rule(TokenType::LBrace, 0, [&]
             { return parse_init_list(); }, nullptr);

    // Infix/postfix rules
    add_rule(TokenType::Assign, 1, nullptr, [&](ExprPtr l)
             { return parse_binary(std::move(l), 1); });
    add_rule(TokenType::Plus, 10, nullptr, [&](ExprPtr l)
             { return parse_binary(std::move(l), 10); });
    add_rule(TokenType::Star, 20, nullptr, [&](ExprPtr l)
             { return parse_binary(std::move(l), 20); });
    add_rule(TokenType::Dot, 30, nullptr, [&](ExprPtr l)
             { return parse_member_access(std::move(l)); });
    add_rule(TokenType::Arrow, 30, nullptr, [&](ExprPtr l)
             { return parse_member_access(std::move(l)); });
    add_rule(TokenType::DoubleColon, 30, nullptr, [&](ExprPtr l)
             { return parse_member_access(std::move(l)); });
    add_rule(TokenType::LParen, 40, nullptr, [&](ExprPtr l)
             { return parse_call(std::move(l)); });
}

std::unique_ptr<Expr> Parser::parse_assignment(std::unique_ptr<Expr> left)
{
    // if (!is_valid_lvalue(*left)) {
    //     error("Invalid assignment target");
    // }

    auto equals_token = previous_;
    advance(); // Skip =
    auto value = parse_expression();

    return std::make_unique<BinaryExpr>(
        equals_token.type,
        std::move(left),
        std::move(value));
}

ExprPtr Parser::parse_expression(int precedence)
{
    debug("parser: parsing expression with precedence %d", precedence);
    auto token_type = current_.type;

    auto it = pratt_rules_.find(token_type);
    if (it == pratt_rules_.end())
    {
        error(vstring("Unexpected token ", token_to_string(current_), " in expression"));
        return nullptr;
    }

    // Find the first prefix rule
    const auto &rules = it->second;
    auto prefix_it = std::find_if(rules.begin(), rules.end(), [](const PrattRule &rule)
                                  { return rule.prefix != nullptr; });

    if (prefix_it == rules.end())
    {
        error(vstring("Unexpected prefix token ", token_to_string(current_), " in expression"));
        return nullptr;
    }

    auto left = prefix_it->prefix();

    debug("parser: parsed prefix expression. new token is %s (precedence %d)",
          token_type_to_string(current_.type).c_str(), get_precedence(current_.type));

    while (precedence < get_precedence(current_.type))
    {
        auto infix_it = pratt_rules_.find(current_.type);
        if (infix_it == pratt_rules_.end())
            break;

        const auto &infix_rules = infix_it->second;
        auto infix_rule_it = std::find_if(infix_rules.begin(), infix_rules.end(), [](const PrattRule &rule)
                                          { return rule.infix != nullptr; });

        if (infix_rule_it == infix_rules.end())
            break;

        left = infix_rule_it->infix(std::move(left));
        debug("parser: parsed infix expression. new token is %s (precedence %d)",
              token_type_to_string(current_.type).c_str(), get_precedence(current_.type));
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
        current_.type == TokenType::Arrow ||
        current_.type == TokenType::DoubleColon)
    {
        return parse_expression(get_precedence(expr->accessor));
    }

    return expr;
}

ExprPtr Parser::parse_call(ExprPtr left)
{
    debug("parser: parsing call expression");
    auto call_expr = std::make_unique<CallExpr>();
    call_expr->callee = std::move(left);

    consume(TokenType::LParen, "Expected '(' after function name");

    // Parse arguments
    if (current_.type != TokenType::RParen)
    {
        do
        {
            call_expr->args.push_back(parse_expression());
        } while (try_consume(TokenType::Comma));
    }

    consume(TokenType::RParen, "Expected ')' after arguments");

    return call_expr;
}

void Parser::consume(TokenType type, const std::string &message)
{
    if (current_.type == type)
    {
        Token consumed = current_;
        debug("parser: consumed token %s", token_to_string(consumed).c_str());
        advance();
        // return consumed;
    }
    else
    {
        error(message);
    }
}

bool Parser::try_consume(TokenType type)
{
    if (current_.type == type)
    {
        Token consumed = current_;
        debug("parser: consumed token %s", token_to_string(consumed).c_str());
        advance();
        return true;
    }

    return false;
}

bool Parser::match(TokenType type)
{
    if (current_.type == type)
    {
        debug("parser: matched token %s", token_to_string(current_).c_str());
        return true;
    }
    debug("parser: no match for token %s, actual token is %s", token_type_to_string(type).c_str(), token_to_string(current_).c_str());
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

        if (current_.type == TokenType::Comma)
        {
            consume(TokenType::Comma, "Expected ',' between struct members");
        }

        if (current_.type == TokenType::RBrace)
        {
            break;
        }
    }

    consume(TokenType::RBrace, "Expected '}' at the end of struct body");

    return struct_decl;
}

// Parse a struct member declaration (field name and type) e.g. "x: int"
TypedField Parser::parse_struct_member()
{
    std::string name = current_.lexeme;
    consume(TokenType::Identifier, "Expected field name");
    consume(TokenType::Colon, "Expected ':' after field name");
    auto type = parse_type();

    return TypedField(std::move(type), name);
}

Type::BasicKind basic_kind_from_string(const std::string &name)
{
    if (name == "int")
        return Type::BasicKind::Int;
    if (name == "float")
        return Type::BasicKind::Float;
    if (name == "string")
        return Type::BasicKind::String;

    throw std::runtime_error("Invalid basic type name: " + name);
}

void apply_const_to_innermost(Type *type)
{
    Type *current = type;
    while (current)
    {
        if (current->kind == Type::Kind::Pointer)
        {
            if (!current->pointee)
                break;
            current = current->pointee.get();
        }
        else if (current->kind == Type::Kind::Array)
        {
            current = current->element_type.get();
        }
        else
        {
            current->is_const = true;
            break;
        }
    }
}

// 主入口：解析完整类型（可能含后缀修饰）
std::unique_ptr<Type> Parser::parse_type()
{
    auto type = parse_prefix_type();
    while (match(TokenType::LBracket))
    {
        type = parse_array_type(std::move(type));
    }
    return type;
}

// 解析可能含前缀修饰的类型（const/指针）
std::unique_ptr<Type> Parser::parse_prefix_type()
{
    if (match(TokenType::LParen))
    {
        advance(); // Consume '('
        auto type = parse_type();
        consume(TokenType::RParen, "Expected ')' after grouped type");

        // 处理括号后的指针修饰
        while (match(TokenType::Star))
        {
            auto ptr_type = parse_pointer_type();
            ptr_type->pointee = std::move(type);
            type = std::move(ptr_type);
        }
        return type;
    }

    if (match(TokenType::Const))
    {
        advance();
        auto type = parse_prefix_type();
        apply_const_to_innermost(type.get());
        return type;
    }

    if (match(TokenType::Star))
    {
        return parse_pointer_type();
    }

    return parse_non_pointer_type();
}

// 解析指针类型（包含指针后的const）
std::unique_ptr<Type> Parser::parse_pointer_type()
{
    consume(TokenType::Star, "Expected '*' in pointer type");
    auto ptr_type = std::make_unique<Type>();
    ptr_type->kind = Type::Kind::Pointer;

    // 解析指针自身的const
    if (match(TokenType::Const))
    {
        advance();
        ptr_type->is_const = true;
    }

    // 递归解析底层类型（可能包含新的前缀）
    ptr_type->pointee = parse_prefix_type();
    return ptr_type;
}

// 解析非指针的基础类型（基本类型/结构体/函数/别名）
std::unique_ptr<Type> Parser::parse_non_pointer_type()
{
    auto type = std::make_unique<Type>();

    if (match(TokenType::Int) || match(TokenType::Float) || match(TokenType::String))
    {
        parse_basic_type(type);
    }
    else if (match(TokenType::Identifier))
    {
        parse_type_alias(type);
    }
    else if (match(TokenType::Fn))
    {
        parse_function_type(type);
    }
    else if (match(TokenType::Struct))
    {
        parse_struct_type(type);
    }
    else
    {
        error("Expected type name");
    }
    return type;
}

// 解析数组类型（后缀处理）
std::unique_ptr<Type> Parser::parse_array_type(std::unique_ptr<Type> base_type)
{
    consume(TokenType::LBracket, "Expected '[' in array type");
    auto array_type = std::make_unique<Type>();
    array_type->kind = Type::Kind::Array;
    array_type->element_type = std::move(base_type);

    if (array_type->is_const)
    {
        apply_const_to_innermost(array_type->element_type.get());
    }

    if (match(TokenType::IntegerLiteral))
    {
        array_type->array_size = std::stoi(current_.lexeme);
        advance();
    }
    else
    {
        array_type->array_size = -1; // 动态数组
    }

    consume(TokenType::RBracket, "Expected ']' in array type");
    return array_type;
}

//--- 具体类型解析实现 ---
void Parser::parse_basic_type(std::unique_ptr<Type> &type)
{
    type->kind = Type::Kind::Basic;
    type->name = current_.lexeme;
    type->basic_kind = basic_kind_from_string(type->name);
    advance();
}

void Parser::parse_type_alias(std::unique_ptr<Type> &type)
{
    std::string name = current_.lexeme;
    type->kind = Type::Kind::Alias;
    type->name = name;
    // if (auto it = type_aliases_.find(name); it != type_aliases_.end()) {
    //     type = it->second->clone();
    // } else {
    //     error("Undefined type alias: " + name);
    // }
    advance();
}

void Parser::parse_function_type(std::unique_ptr<Type> &type)
{
    type->kind = Type::Kind::Function;
    advance();
    consume(TokenType::LParen, "Expected '(' in function type");

    while (!match(TokenType::RParen))
    {
        type->params.push_back(parse_type());

        if (!match(TokenType::Comma))
            break;
        advance();
    }
    consume(TokenType::RParen, "Expected ')' in function type");

    if (match(TokenType::Arrow))
    {
        advance();
        type->return_type = parse_type();
    }
}

void Parser::parse_struct_type(std::unique_ptr<Type> &type)
{
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
    catch (const ParseError &e)
    {
        debug("parse_type_safe error");
        errors_.push_back(e.what());
        synchronize_type();
        // Placeholder
        return std::make_unique<Type>();
    }
}

ExprPtr Parser::parse_identifier()
{
    debug("parser: parsing identifier");
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

ExprPtr Parser::parse_address_of()
{
    consume(TokenType::Ampersand, "Expected '&' in address-of expression");
    auto operand = parse_expression();
    auto expr = std::make_unique<AddressOfExpr>(std::move(operand));
    return expr;
}

ExprPtr Parser::parse_deref()
{
    consume(TokenType::Star, "Expected '*' in dereference expression");
    auto operand = parse_expression();
    auto expr = std::make_unique<DerefExpr>(std::move(operand));
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
            error("Expected ',' or '}' in initializer list");
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
    case TokenType::Assign:
        return 1;
    case TokenType::Plus:
    case TokenType::Minus:
        return 2;
    case TokenType::Star:
    case TokenType::Divide:
        return 3;
    // case TokenType::Caret: // Exponentiation
    //     return 4;
    case TokenType::LParen:
    case TokenType::LBracket:
    case TokenType::Dot:
    case TokenType::Arrow:
    case TokenType::DoubleColon:
        return 9;
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
    else if (current_.type == TokenType::While)
    {
        return parse_while();
    }
    else
    {
        auto expr = parse_expression();
        consume(TokenType::Semicolon, "Expect ';' after expression");
        return std::make_unique<ExprStmt>(std::move(expr));
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

StmtPtr Parser::parse_while()
{
    advance(); // Move past the 'while' keyword
    consume(TokenType::LParen, "Expect '(' after 'while'.");
    ExprPtr condition = parse_expression();
    consume(TokenType::RParen, "Expect ')' after condition.");
    StmtPtr body = parse_statement();
    return std::make_unique<WhileStmt>(std::move(condition), std::move(body));
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

    consume(TokenType::Semicolon, vstring("Expected ';' after variable declaration for ", stmt.name, ", but got ", current_.lexeme));
    return stmt;
}

FunctionDecl Parser::parse_function_decl()
{
    FunctionDecl func;
    consume(TokenType::Fn, "Expected 'fn'");

    func.name = current_.lexeme;
    consume(TokenType::Identifier, "Expected function name");

    // Params
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
    else
    {
        func.return_type = std::make_unique<Type>(Type::get_void_type());
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
    debug("parser: advancing to next token (from %s)", current_.lexeme.c_str());
    previous_ = current_;
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
            else if (match(TokenType::Let) || match(TokenType::Const))
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
            debug(e.what());
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

GlobalDecl Parser::parse_global_decl()
{
    auto decl = GlobalDecl(parse_var_decl());

    if (!decl.is_const && !decl.init_expr)
    {
        error("Global variable must be const or have initializer");
    }
    return decl;
}
