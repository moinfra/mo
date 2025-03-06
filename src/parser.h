// src/parser.h
#pragma once
#include "lexer.h"
#include "ast.h"
#include <memory>
#include <functional>
#include <unordered_map>
#include <set>
#include <sstream>
#include <string>

template<typename... Args>
std::string vstring(Args... args) {
    std::stringstream ss;
    (ss << ... << args);
    return ss.str();
}

class Parser
{
public:
    explicit Parser(Lexer &&lexer);

    ast::Program parse();
    ast::ExprPtr parse_expr(int precedence = 0);
    ast::TypePtr parse_type(int precedence = 0);

    std::vector<std::string> errors() const { return errors_; }

private:
    Lexer lexer_;
    Token current_;
    Token previous_;
    std::vector<std::string> errors_;
    std::unordered_map<std::string, std::unique_ptr<ast::Type>> type_aliases_;

    struct PrattRule
    {
        std::function<ast::ExprPtr()> prefix;
        std::function<ast::ExprPtr(ast::ExprPtr)> infix;

        bool operator<(const PrattRule& other) const
        {
            if (prefix && other.prefix)
                return false;
            if (infix && other.infix)
                return false;
            return true;
        }
    };

    struct TypePrattRule
    {
        std::function<ast::TypePtr()> prefix;
        std::function<ast::TypePtr(ast::TypePtr)> infix;

        bool operator<(const TypePrattRule& other) const
        {
            if (prefix && other.prefix)
                return false;
            if (infix && other.infix)
                return false;
            return true;
        }
    };

    std::unordered_map<TokenType, std::set<PrattRule>> pratt_rules_;
    std::unordered_map<TokenType, std::set<TypePrattRule>> type_pratt_rules_;

    void init_pratt_rules();
    void init_type_pratt_rules();

    void advance();
    bool match(TokenType type);
    void consume(TokenType type, const std::string &message = "");
    bool try_consume(TokenType type);
    void error(const std::string &message) const;
    void synchronize();

    uint8_t parse_bitwidth();
    ast::TypePtr parse_type_safe();
    ast::TypePtr parse_prefix_type();
    ast::TypePtr parse_pointer_type();
    ast::TypePtr parse_array_type(std::unique_ptr<ast::Type> base_type);
    ast::TypePtr parse_non_pointer_type();
    void parse_basic_type(std::unique_ptr<ast::Type> &type);
    void parse_function_type(std::unique_ptr<ast::Type> &type);
    void parse_struct_type(std::unique_ptr<ast::Type> &type);
    void parse_type_alias(std::unique_ptr<ast::Type> &type);
    void synchronize_type();

    ast::TypeAliasDecl parse_type_alias_decl();
    ast::StructDecl parse_struct_decl();
    ast::TypedField parse_struct_member();
    ast::FunctionDecl parse_function_decl(ast::StructType* receiver_type = nullptr);
    ast::FunctionDecl parse_method(ast::Type* target_type);
    ast::GlobalDecl parse_global_decl();
    ast::ImplBlock parse_impl_block();
    ast::StmtPtr parse_statement();
    ast::VarDeclStmt parse_var_decl();
    ast::StmtPtr parse_return();
    ast::StmtPtr parse_if();
    ast::StmtPtr parse_while();
    std::unique_ptr<ast::BlockStmt> parse_block();
    ast::ExprPtr parse_function_pointer_expr();
    ast::ExprPtr parse_struct_literal(std::string name = "");

    ast::ExprPtr parse_identifier(int min_precedence);
    ast::ExprPtr parse_literal();
    ast::ExprPtr parse_tuple_or_grouped();
    ast::ExprPtr parse_cast();
    ast::ExprPtr parse_sizeof();
    ast::ExprPtr parse_address_of();
    ast::ExprPtr parse_deref(int min_precedence);
    ast::ExprPtr parse_init_list();

    ast::ExprPtr parse_unary(int min_precedence);
    ast::ExprPtr parse_binary(ast::ExprPtr left, int min_precedence);
    ast::ExprPtr parse_call(ast::ExprPtr left);
    ast::ExprPtr parse_member_access(ast::ExprPtr left);
    ast::ExprPtr parse_array_access(ast::ExprPtr left);

};

struct ParseError : std::exception
{
    int line;
    int column;
    std::string message;

    ParseError(int line, int column, const std::string &message)
        : line(line), column(column), message(message) {}

    const char *what() const noexcept override
    {
        static std::string str;
        str = "Parse error at " + std::to_string(line) + ":" + std::to_string(column) + " - " + message;
        return str.c_str();
    }
};
