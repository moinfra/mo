// src/parser.cc
#include "parser.h"
#include "mo_debug.h"

#include <cassert>
#include <unordered_set>
#include <algorithm>
#include <iostream>

using namespace ast;

#define ASSOC unsigned
#define R_ASSOC 1
#define L_ASSOC 2

void apply_const_to_innermost(Type *type);

Parser::Parser(Lexer &&lexer) : lexer_(std::move(lexer)), current_(lexer_.next_token())
{
    init_pratt_rules();
    init_type_pratt_rules();
}

struct TokenTypeAssocHash
{
    std::size_t operator()(const std::pair<TokenType, ASSOC> &p) const
    {
        return std::hash<TokenType>()(p.first) ^ std::hash<ASSOC>()(p.second);
    }
};

const static std::unordered_map<std::pair<TokenType, ASSOC>, int, TokenTypeAssocHash> precedence_map =
    {
        {{TokenType::Assign, R_ASSOC}, 1},    // right assoc
        {{TokenType::AddAssign, R_ASSOC}, 1}, // right assoc
        {{TokenType::SubAssign, R_ASSOC}, 1}, // right assoc
        {{TokenType::MulAssign, R_ASSOC}, 1}, // right assoc
        {{TokenType::DivAssign, R_ASSOC}, 1}, // right assoc
        {{TokenType::ModAssign, R_ASSOC}, 1}, // right assoc
        {{TokenType::AndAssign, R_ASSOC}, 1}, // right assoc
        {{TokenType::OrAssign, R_ASSOC}, 1},  // right assoc
        {{TokenType::XorAssign, R_ASSOC}, 1}, // right assoc
        {{TokenType::LSAssign, R_ASSOC}, 1},  // right assoc
        {{TokenType::RSAssign, R_ASSOC}, 1},  // right assoc

        {{TokenType::Or, L_ASSOC}, 10},
        {{TokenType::And, L_ASSOC}, 11},
        {{TokenType::Pipe, L_ASSOC}, 12},
        {{TokenType::Caret, L_ASSOC}, 12},
        {{TokenType::Ampersand, L_ASSOC}, 13},

        {{TokenType::Eq, L_ASSOC}, 20},
        {{TokenType::Ne, L_ASSOC}, 20},

        {{TokenType::Lt, L_ASSOC}, 30},
        {{TokenType::Le, L_ASSOC}, 30},
        {{TokenType::Gt, L_ASSOC}, 30},
        {{TokenType::Ge, L_ASSOC}, 30},

        {{TokenType::LShift, L_ASSOC}, 40},
        {{TokenType::RShift, L_ASSOC}, 40},

        {{TokenType::Plus, L_ASSOC}, 50},
        {{TokenType::Minus, L_ASSOC}, 50},

        {{TokenType::Star, L_ASSOC}, 60},
        {{TokenType::Slash, L_ASSOC}, 60},
        {{TokenType::Modulo, L_ASSOC}, 60},

        // {{TokenType::DotStar, L_ASSOC}, 70},
        // {{TokenType::ArrowStar, L_ASSOC}, 70},

        {{TokenType::Cast, R_ASSOC}, 80},
        {{TokenType::Star, R_ASSOC}, 80},      // Deref
        {{TokenType::Ampersand, R_ASSOC}, 13}, // Address of
        {{TokenType::Not, R_ASSOC}, 80},       // !
        {{TokenType::Tilde, R_ASSOC}, 80},     // ~
        {{TokenType::Sizeof, R_ASSOC}, 80},

        {{TokenType::Decrement, R_ASSOC}, 90},
        {{TokenType::Increment, R_ASSOC}, 90},
        {{TokenType::LParen, L_ASSOC}, 90},   // Call
        {{TokenType::LBracket, L_ASSOC}, 90}, // Index
        {{TokenType::Dot, L_ASSOC}, 90},      // Member access
        {{TokenType::Arrow, L_ASSOC}, 90},    // Member access

        {{TokenType::DoubleColon, L_ASSOC}, 100},
};

int get_precedence(TokenType type, ASSOC assoc = L_ASSOC | R_ASSOC)
{
    int precedence = 0;
    bool found = false;
    if (assoc & L_ASSOC)
    {
        auto it = precedence_map.find(std::make_pair(type, L_ASSOC));
        if (it != precedence_map.end())
        {
            precedence = it->second;
            found = true;
        }
    }
    if (assoc & R_ASSOC)
    {
        auto it = precedence_map.find(std::make_pair(type, R_ASSOC));
        if (it != precedence_map.end())
        {
            precedence = it->second;
            found = true;
        }
    }
    if (!found)
    {
        MO_DEBUG("parser: warning: no precedence for token '%s' with associativity %s, "
                 "falling back to 0",
                 token_type_to_string(type).c_str(), assoc ? "right" : "left");
    }
    return precedence;
};

void Parser::init_pratt_rules()
{
    auto add_prefix_rule = [&](TokenType type, std::function<ExprPtr()> prefix)
    {
        pratt_rules_[type].insert({std::move(prefix), nullptr});
    };
    auto add_infix_rule = [&](TokenType type, std::function<ExprPtr(ExprPtr)> infix)
    {
        pratt_rules_[type].insert({nullptr, std::move(infix)});
    };

    // Prefix rules
    add_prefix_rule(TokenType::Identifier, [&]
                    { return parse_identifier(get_precedence(TokenType::Identifier) - 1); });
    add_prefix_rule(TokenType::IntegerLiteral, [&]
                    { return parse_literal(); });
    add_prefix_rule(TokenType::BooleanLiteral, [&]
                    { return parse_literal(); });
    add_prefix_rule(TokenType::FloatLiteral, [&]
                    { return parse_literal(); });
    add_prefix_rule(TokenType::StringLiteral, [&]
                    { return parse_literal(); });
    add_prefix_rule(TokenType::Ampersand, [&]
                    { return parse_address_of(); });
    add_prefix_rule(TokenType::Star, [&]
                    { return parse_deref(get_precedence(TokenType::Star, R_ASSOC) - 1); });
    add_prefix_rule(TokenType::Plus, [&]
                    { return parse_unary(get_precedence(TokenType::Plus, R_ASSOC) - 1); });
    add_prefix_rule(TokenType::Minus, [&]
                    { return parse_unary(get_precedence(TokenType::Minus, R_ASSOC) - 1); });
    add_prefix_rule(TokenType::Not, [&]
                    { return parse_unary(get_precedence(TokenType::Not, R_ASSOC) - 1); });
    add_prefix_rule(TokenType::Tilde, [&]
                    { return parse_unary(get_precedence(TokenType::Tilde, R_ASSOC) - 1); });
    add_prefix_rule(TokenType::LParen, [&]
                    { return parse_tuple_or_grouped(); });
    add_prefix_rule(TokenType::Cast, [&]
                    { return parse_cast(); });
    add_prefix_rule(TokenType::Sizeof, [&]
                    { return parse_sizeof(); });
    add_prefix_rule(TokenType::LBracket, [&]
                    { return parse_init_list(); });

    // Infix/postfix rules
    add_infix_rule(TokenType::Assign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Assign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::AddAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::AddAssign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::SubAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::SubAssign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::MulAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::MulAssign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::DivAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::DivAssign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::ModAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::ModAssign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::AndAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::AndAssign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::OrAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::OrAssign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::XorAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::XorAssign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::LSAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::LSAssign, R_ASSOC) - 1);/* Right assoc*/ });
    add_infix_rule(TokenType::RSAssign, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::RSAssign, R_ASSOC) - 1);/* Right assoc*/ });

    add_infix_rule(TokenType::Plus, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Plus)); });
    add_infix_rule(TokenType::Minus, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Minus)); });
    add_infix_rule(TokenType::Star, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Star)); });
    add_infix_rule(TokenType::Slash, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Slash)); });
    add_infix_rule(TokenType::Modulo, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Modulo)); });

    add_infix_rule(TokenType::And, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::And)); });
    add_infix_rule(TokenType::Or, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Or)); });
    add_infix_rule(TokenType::Eq, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Eq)); });
    add_infix_rule(TokenType::Ne, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Ne)); });
    add_infix_rule(TokenType::Lt, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Lt)); });
    add_infix_rule(TokenType::Le, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Le)); });
    add_infix_rule(TokenType::Gt, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Gt)); });
    add_infix_rule(TokenType::Lt, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::Lt)); });
    add_infix_rule(TokenType::LShift, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::LShift)); });
    add_infix_rule(TokenType::RShift, [&](ExprPtr l)
                   { return parse_binary(std::move(l), get_precedence(TokenType::RShift)); });

    add_infix_rule(TokenType::Dot, [&](ExprPtr l)
                   { return parse_member_access(std::move(l)); });
    add_infix_rule(TokenType::Arrow, [&](ExprPtr l)
                   { return parse_member_access(std::move(l)); });
    add_infix_rule(TokenType::DoubleColon, [&](ExprPtr l)
                   { return parse_member_access(std::move(l)); });
    add_infix_rule(TokenType::LBracket, [&](ExprPtr l)
                   { return parse_array_access(std::move(l)); });
    add_infix_rule(TokenType::LParen, [&](ExprPtr l)
                   { return parse_call(std::move(l)); });
}

const static std::unordered_map<std::pair<TokenType, ASSOC>, int, TokenTypeAssocHash> type_precedence_map = {
    {{TokenType::Const, R_ASSOC}, 90},

    {{TokenType::Star, R_ASSOC}, 80},
    // {{TokenType::Caret, R_ASSOC}, 80},

    {{TokenType::LBracket, L_ASSOC}, 100},
    {{TokenType::LParen, L_ASSOC}, 100},
    {{TokenType::Arrow, L_ASSOC}, 50},
};

int get_type_precedence(TokenType token_type, ASSOC assoc = L_ASSOC | R_ASSOC)
{
    int precedence = 0;
    bool found = false;
    if (assoc & L_ASSOC)
    {
        auto it = type_precedence_map.find(std::make_pair(token_type, L_ASSOC));
        if (it != type_precedence_map.end())
        {
            precedence = it->second;
            found = true;
        }
    }
    if (assoc & R_ASSOC)
    {
        auto it = type_precedence_map.find(std::make_pair(token_type, R_ASSOC));
        if (it != type_precedence_map.end())
        {
            precedence = it->second;
            found = true;
        }
    }
    if (!found)
    {
        MO_DEBUG("parser: warning: no precedence for token '%s' with associativity %s, "
                 "falling back to 0",
                 token_type_to_string(token_type).c_str(), assoc ? "right" : "left");
    }
    return precedence;
};

uint8_t Parser::parse_bitwidth()
{
    auto lexeme = current_.lexeme;
    advance();

    assert(!lexeme.empty() && "Expected bitwidth lexeme");
    if (lexeme == "int")
        return 32;
    if (lexeme == "bool")
        return 1;
    else if (lexeme == "float")
        return 64;
    else if (lexeme[0] == 'i')
    {
        return std::stoi(lexeme.substr(1));
    }
    else if (lexeme[0] == 'f')
    {
        return std::stoi(lexeme.substr(1));
    }
    else
    {
        error("Invalid bitwidth: " + lexeme);
        return 0;
    }
}

void Parser::init_type_pratt_rules()
{
    auto add_prefix_rule = [&](TokenType type, std::function<TypePtr()> fn)
    {
        type_pratt_rules_[type].insert({std::forward<decltype(fn)>(fn), nullptr});
    };

    auto add_postfix_rule = [&](TokenType type, std::function<TypePtr(TypePtr)> fn)
    {
        type_pratt_rules_[type].insert({nullptr, std::forward<decltype(fn)>(fn)});
    };

    add_prefix_rule(TokenType::Int, [&]
                    { 
                        auto unsigned_ = current_.lexeme[0] == 'u';
                        return Type::create_int(parse_bitwidth(), unsigned_); });
    add_prefix_rule(TokenType::Float, [&]
                    { return Type::create_float(parse_bitwidth()); });
    add_prefix_rule(TokenType::String, [&]
                    { advance(); return Type::create_string(); });

    add_prefix_rule(TokenType::Identifier, [&]
                    {
        const std::string name = current_.lexeme;
        advance();
        return Type::create_alias(name); });

    add_prefix_rule(TokenType::Star, [&]
                    {
        advance(); // *
        TypePtr pointee = parse_type(get_precedence(TokenType::Star, R_ASSOC) - 1);
        return Type::create_pointer(std::move(pointee)); });

    add_postfix_rule(TokenType::LBracket, [&](TypePtr element_type)
                     {
        consume(TokenType::LBracket);
        
        int size = -1;
        if (match(TokenType::IntegerLiteral)) {
            size = std::stoi(current_.lexeme);
            advance();
        }
        
        consume(TokenType::RBracket);
        return Type::create_array(std::move(element_type), size); });

    add_postfix_rule(TokenType::LParen, [&](TypePtr return_type)
                     {
        consume(TokenType::LParen);
        
        std::vector<TypePtr> params;
        while (!match(TokenType::RParen)) {
            params.emplace_back(parse_type());
            if (!try_consume(TokenType::Comma)) break;
        }
        
        consume(TokenType::RParen);
        
        if (try_consume(TokenType::Arrow)) {
            return_type = parse_type();
        }
        
        return Type::create_function(std::move(return_type), std::move(params)); });

    add_prefix_rule(TokenType::Const, [&]
                    {
        advance(); // const
        TypePtr base = parse_type(get_precedence(TokenType::Const, R_ASSOC) - 1);
        return Type::create_qualified(Qualifier::Const, std::move(base)); });
}

void apply_const_to_innermost(Type *type, Qualifier q)
{
    if (auto *qualified = type->as_qualified())
    {
        apply_const_to_innermost(&qualified->base_type(), q);
    }
    else
    {
        TypePtr base = type->clone();
        type = Type::create_qualified(q, std::move(base)).release();
    }
}

std::unique_ptr<Type> Parser::parse_type(int precedence)
{
    auto token = current_;
    auto it = type_pratt_rules_.find(token.type);

    if (it == type_pratt_rules_.end())
    {
        error("Unexpected token in type: " + token_type_to_string(token.type));
        return nullptr;
    }

    // prefix
    auto &rules = it->second;
    auto prefix_rule = std::find_if(rules.begin(), rules.end(),
                                    [](auto &r)
                                    { return r.prefix != nullptr; });

    if (prefix_rule == rules.end())
    {
        error("Missing prefix handler for: " + token_type_to_string(token.type));
        return nullptr;
    }

    auto left = prefix_rule->prefix();

    // infix/postfix
    while (precedence < get_type_precedence(current_.type))
    {
        auto next = type_pratt_rules_.find(current_.type);
        if (next == type_pratt_rules_.end())
            break;

        auto &infix_rules = next->second;
        auto infix_rule = std::find_if(infix_rules.begin(), infix_rules.end(),
                                       [](auto &r)
                                       { return r.infix != nullptr; });

        if (infix_rule == infix_rules.end())
            break;

        left = infix_rule->infix(std::move(left));
    }

    return left;
}

ExprPtr Parser::parse_expr(int precedence)
{
    MO_DEBUG("parser: parsing expression with precedence %d", precedence);
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

    MO_DEBUG("parser: parsed %s prefix expression. new token is %s (precedence %d)",
             token_type_to_string(token_type).c_str(), token_type_to_string(current_.type).c_str(), get_precedence(current_.type));

    while (precedence < get_precedence(current_.type))
    {
        auto infix_it = pratt_rules_.find(current_.type);
        if (infix_it == pratt_rules_.end())
        {
            MO_DEBUG("parser: no infix rule for token %s", token_type_to_string(current_.type).c_str());
            break;
        }

        const auto &infix_rules = infix_it->second;
        auto infix_rule_it = std::find_if(infix_rules.begin(), infix_rules.end(), [](const PrattRule &rule)
                                          { return rule.infix != nullptr; });

        if (infix_rule_it == infix_rules.end())
        {
            MO_DEBUG("parser: no infix rule for token %s", token_type_to_string(current_.type).c_str());
            break;
        }

        left = infix_rule_it->infix(std::move(left));
        MO_DEBUG("parser: parsed infix expression. new token is %s (precedence %d)",
                 token_type_to_string(current_.type).c_str(), get_precedence(current_.type));
    }

    return left;
}

ExprPtr Parser::parse_member_access(ExprPtr left)
{
    auto accessor = current_.type;
    advance(); // Skip . or ->
    auto member = current_.lexeme;
    consume(TokenType::Identifier, "Expected member name after access operator");

    auto expr = std::make_unique<MemberAccessExpr>(std::move(left), member, accessor);

    // 处理链式访问 Handle chained member access
    if (current_.type == TokenType::Dot ||
        current_.type == TokenType::Arrow ||
        current_.type == TokenType::DoubleColon)
    {
        return parse_expr(get_precedence(accessor));
    }

    if (match(TokenType::LParen))
    {
        expr->is_call = true;
    }

    return expr;
}

ExprPtr Parser::parse_array_access(ExprPtr left)
{
    consume(TokenType::LBracket, "Expected '[' after array name");
    auto index = parse_expr();
    consume(TokenType::RBracket, "Expected ']' after array index");
    return std::make_unique<ArrayAccessExpr>(std::move(left), std::move(index));
}

ExprPtr Parser::parse_call(ExprPtr left)
{
    MO_DEBUG("parser: parsing call expression");
    consume(TokenType::LParen, "Expected '(' after function name");

    std::vector<ExprPtr> args;
    if (current_.type != TokenType::RParen)
    {
        do
        {
            args.push_back(parse_expr());
        } while (try_consume(TokenType::Comma));
    }

    consume(TokenType::RParen, "Expected ')' after arguments");
    return std::make_unique<CallExpr>(std::move(left), std::move(args));
}

void Parser::consume(TokenType type, const std::string &message)
{
    if (current_.type == type)
    {
        Token consumed = current_;
        MO_DEBUG("parser: consumed token %s", token_to_string(consumed).c_str());
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
        MO_DEBUG("parser: consumed token %s", token_to_string(consumed).c_str());
        advance();
        return true;
    }

    return false;
}

bool Parser::match(TokenType type)
{
    if (current_.type == type)
    {
        MO_DEBUG("parser: matched token %s", token_to_string(current_).c_str());
        return true;
    }
    MO_DEBUG("parser: no match for token %s, actual token is %s", token_type_to_string(type).c_str(), token_to_string(current_).c_str());
    return false;
}

std::unique_ptr<BlockStmt> Parser::parse_block()
{
    consume(TokenType::LBrace, "Expected '{' at the start of block");

    std::vector<StmtPtr> statements;
    while (current_.type != TokenType::RBrace && current_.type != TokenType::Eof)
    {
        statements.push_back(parse_statement());
    }

    consume(TokenType::RBrace, "Expected '}' at the end of block");

    return std::make_unique<BlockStmt>(std::move(statements));
}

TypeAliasDecl Parser::parse_type_alias_decl()
{
    TypeAliasDecl decl;

    consume(TokenType::Type, "Expected 'type' keyword");

    decl.name = current_.lexeme;
    consume(TokenType::Identifier, "Expected alias name");

    consume(TokenType::Assign, "Expected '=' after alias name");

    decl.type = parse_type();

    consume(TokenType::Semicolon, "Expected ';' after type alias declaration");

    // Register the type alias in your type_aliases_ map
    type_aliases_.emplace(decl.name, std::move(decl.type));

    return decl;
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

    return TypedField(name, std::move(type));
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

ExprPtr Parser::parse_struct_literal(std::string struct_name)
{
    if (!struct_name.empty())
    {
        // struct_name is already set
    }
    else if (match(TokenType::Identifier))
    {
        struct_name = current_.lexeme;
        advance();
    }

    consume(TokenType::LBrace, "Expected '{'");
    std::unordered_set<std::string> seen_fields;
    std::vector<std::pair<std::string, ExprPtr>> members;

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
        ExprPtr value = parse_expr();

        members.emplace_back(name, std::move(value));

        if (!match(TokenType::Comma))
            break;
        advance();
    }

    consume(TokenType::RBrace, "Expected '}'");
    return std::make_unique<StructLiteralExpr>(std::move(struct_name), std::move(members));
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
        case TokenType::Type:
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
        MO_DEBUG("parse_type_safe error");
        errors_.push_back(e.what());
        synchronize_type();
        // Placeholder
        return Type::create_placeholder();
    }
}

ExprPtr Parser::parse_identifier(int min_precedence)
{

    auto ident = current_.lexeme;
    advance();

    // handle MyStruct { ... } expr
    if (match(TokenType::LBrace))
    {
        MO_DEBUG("parser: parsing struct value");
        return parse_struct_literal(ident);
    }
    else
    {
        MO_DEBUG("parser: parsing identifier");
        return std::make_unique<VariableExpr>(ident);
    }
}
ExprPtr Parser::parse_literal()
{
    ExprPtr expr;
    switch (current_.type)
    {
    case TokenType::IntegerLiteral:
        expr = std::make_unique<IntegerLiteralExpr>(std::stoi(current_.lexeme));
        break;
    case TokenType::BooleanLiteral:
    {
        bool val = false;
        if (current_.lexeme == "true")
        {
            val = true;
        }
        else if (current_.lexeme == "false")
        {
            val = false;
        }
        else
        {
            error("Invalid boolean literal: " + current_.lexeme);
        }
        expr = std::make_unique<BooleanLiteralExpr>(val);
        break;
    }
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

ExprPtr Parser::parse_tuple_or_grouped()
{
    consume(TokenType::LParen, "Expect '('");

    if (try_consume(TokenType::RParen))
    {
        return std::make_unique<TupleExpr>(std::vector<ExprPtr>{});
    }

    auto first_expr = parse_expr();

    if (match(TokenType::Comma))
    {
        std::vector<ExprPtr> elements;
        elements.push_back(std::move(first_expr));

        do
        {
            consume(TokenType::Comma, "Expect ',' in tuple");
            // Allow trailing comma like (a, b, )
            if (match(TokenType::RParen))
                break;
            elements.push_back(parse_expr());
        } while (match(TokenType::Comma));

        consume(TokenType::RParen, "Expect ')' after tuple");
        return std::make_unique<TupleExpr>(std::move(elements));
    }
    else if (try_consume(TokenType::RParen))
    {
        return first_expr;
    }
    else
    {
        error("Expected ',' or ')' after expression");
        return nullptr;
    }
}

ExprPtr Parser::parse_cast()
{
    consume(TokenType::Cast, "Expected 'cast'");
    consume(TokenType::LParen, "Expected '(' after 'cast'");
    auto target_type = parse_type();
    consume(TokenType::Comma, "Expected ',' after cast target type");
    auto expr = parse_expr();
    consume(TokenType::RParen, "Expected ')' after cast expression");
    return std::make_unique<CastExpr>(std::move(target_type), std::move(expr));
}

ExprPtr Parser::parse_sizeof()
{
    consume(TokenType::Sizeof, "Expected 'sizeof'");
    consume(TokenType::LParen, "Expected '(' after 'sizeof'");

    TypePtr target_type = parse_type_safe();
    if (target_type)
    {
        consume(TokenType::RParen, "Expected ')' after sizeof type");
        return std::make_unique<SizeofExpr>(std::move(target_type));
    }
    else
    {
        MO_DEBUG("parser: sizeof target is not a type, try parse expr");
        ExprPtr target_expr = parse_expr();
        consume(TokenType::RParen, "Expected ')' after sizeof expr");
        return std::make_unique<SizeofExpr>(std::move(target_expr));
    }
}

ExprPtr Parser::parse_address_of()
{
    consume(TokenType::Ampersand, "Expected '&' in address-of expression");
    auto operand = parse_expr();
    auto expr = std::make_unique<AddressOfExpr>(std::move(operand));
    return expr;
}

ExprPtr Parser::parse_deref(int min_precedence)
{
    consume(TokenType::Star, "Expected '*' in dereference expression");
    auto operand = parse_expr(min_precedence);
    auto expr = std::make_unique<DerefExpr>(std::move(operand));
    return expr;
}

ExprPtr Parser::parse_init_list()
{
    consume(TokenType::LBracket, "Expected '[' for initializer list");

    std::vector<ExprPtr> members;
    while (current_.type != TokenType::RBracket && current_.type != TokenType::Eof)
    {
        members.push_back(parse_expr());

        if (current_.type == TokenType::Comma)
        {
            advance(); // Consume the comma
        }
        else if (current_.type != TokenType::RBracket)
        {
            error("Expected ',' or ']' in initializer list");
        }
    }

    consume(TokenType::RBracket, "Expected ']' at the end of initializer list");
    return std::make_unique<InitListExpr>(std::move(members));
}

ExprPtr Parser::parse_binary(ExprPtr left, int min_precedence)
{
    MO_DEBUG("parser: parsing binary expression");
    auto op = current_.type;
    advance();
    return std::make_unique<BinaryExpr>(op, std::move(left), std::move(parse_expr(min_precedence)));
}

ExprPtr Parser::parse_unary(int min_precedence)
{
    MO_DEBUG("parser: parsing unary expression");
    auto op = current_.type;
    advance();
    return std::make_unique<UnaryExpr>(op, std::move(parse_expr(min_precedence)));
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
        auto expr = parse_expr();
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
        expr = parse_expr();
    }
    consume(TokenType::Semicolon, "Expect ';' after return value.");
    return std::make_unique<ReturnStmt>(std::move(expr));
}

StmtPtr Parser::parse_if()
{
    advance(); // Move past the 'if' keyword
    consume(TokenType::LParen, "Expect '(' after 'if'.");
    ExprPtr condition = parse_expr();
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
    ExprPtr condition = parse_expr();
    consume(TokenType::RParen, "Expect ')' after condition.");
    StmtPtr body = parse_statement();
    return std::make_unique<WhileStmt>(std::move(condition), std::move(body));
}

VarDeclStmt Parser::parse_var_decl()
{
    bool is_const = (current_.type == TokenType::Const);
    if (is_const)
    {
        advance();
    }
    else
    {
        consume(TokenType::Let, "Expected 'let' or 'const'");
    }

    std::string name = current_.lexeme;
    consume(TokenType::Identifier, "Expected variable name");

    TypePtr type = nullptr;
    if (match(TokenType::Colon))
    {
        advance();
        type = parse_type();
    }

    ExprPtr init_expr = nullptr;
    if (match(TokenType::Assign))
    {
        advance();
        init_expr = parse_expr();
    }

    consume(TokenType::Semicolon, vstring("Expected ';' after variable declaration for ", name, ", but got ", current_.lexeme));
    return VarDeclStmt{is_const, std::move(name), std::move(type), std::move(init_expr)};
}

FunctionDecl Parser::parse_function_decl(StructType *receiver_type)
{
    auto is_static = try_consume(TokenType::Static);

    consume(TokenType::Fn, "Expected 'fn'");

    std::string name = current_.lexeme;
    consume(TokenType::Identifier, "Expected function name");

    // Params
    consume(TokenType::LParen, "Expected (");
    std::vector<TypedField> params;

    // receiver
    TypePtr receiver_type_ptr;
    if (receiver_type != nullptr)
    {
        receiver_type_ptr = receiver_type->clone();
        name = receiver_type->name() + "::" + name;
    }

    while (!match(TokenType::RParen))
    {
        std::string param_name = current_.lexeme;
        consume(TokenType::Identifier, "Expected parameter name");
        consume(TokenType::Colon, "Expected : after parameter name");
        auto type = parse_type();
        params.emplace_back(param_name, std::move(type));

        if (!match(TokenType::Comma))
            break;
        advance();
    }
    consume(TokenType::RParen, "Expected )");

    // Return type
    TypePtr return_type = Type::create_void();
    if (match(TokenType::Arrow))
    {
        advance();
        return_type = parse_type();
    }

    StmtPtr body = parse_block();
    std::vector<StmtPtr> body_statements;
    if (auto blockStmt = dynamic_cast<BlockStmt *>(body.get()))
    {
        body_statements = std::move(blockStmt->statements);
    }
    else
    {
        error("Expected block statement after function body");
    }

    auto decl = FunctionDecl(
        std::move(name),
        std::move(return_type),
        std::move(params),
        std::move(body_statements),
        std::move(receiver_type_ptr),
        is_static);
    return decl;
}

void Parser::error(const std::string &message) const
{
    throw ParseError{current_.start_line, current_.start_col, message};
}

void Parser::advance()
{
    MO_DEBUG("parser: advancing to next token (from %s)", current_.lexeme.c_str());
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
            MO_DEBUG(e.what());
            errors_.push_back(e.what());
            synchronize();
        }
    }

    return program;
}

FunctionDecl Parser::parse_method(ast::StructType *target_type)
{
    FunctionDecl method = parse_function_decl(target_type);
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
        auto ty = impl.target_type.get()->as_struct();
        if (ty == nullptr)
        {
            error("Impl block can only be used for struct types");
        }
        impl.methods.push_back(std::make_unique<FunctionDecl>(parse_method(ty)));
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
