// src/parser.h
#pragma once
#include "lexer.h"
#include "ast.h"
#include <memory>
#include <functional>
#include <unordered_map>
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

    Program parse();
    ExprPtr parse_expr();
    std::vector<std::string> errors() const { return errors_; }

private:
    Lexer lexer_;
    Token current_;
    std::vector<std::string> errors_;
    std::unordered_map<std::string, std::unique_ptr<Type>> type_aliases_;

    struct PrattRule
    {
        int precedence;
        std::function<ExprPtr()> prefix;
        std::function<ExprPtr(ExprPtr)> infix;
    };

    std::unordered_map<TokenType, PrattRule> pratt_rules_;

    void init_pratt_rules();
    int get_precedence(TokenType type);
    ExprPtr parse_expression(int precedence = 0);

    void advance();
    bool match(TokenType type);
    void consume(TokenType type, const std::string &message);
    void error(const std::string &message) const;
    void synchronize();


    TypePtr parse_type();
    TypePtr parse_type_safe();
    TypePtr parse_prefix_type();
    TypePtr parse_pointer_type();
    TypePtr parse_array_type(std::unique_ptr<Type> base_type);
    TypePtr parse_non_pointer_type();
    void parse_basic_type(std::unique_ptr<Type> &type);
    void parse_function_type(std::unique_ptr<Type> &type);
    void parse_struct_type(std::unique_ptr<Type> &type);
    void parse_type_alias(std::unique_ptr<Type> &type);
    void synchronize_type();

    StructDecl parse_struct_decl();
    TypedField parse_struct_member();
    FunctionDecl parse_function_decl();
    FunctionDecl parse_method();
    GlobalDecl parse_global_decl();
    ImplBlock parse_impl_block();
    StmtPtr parse_statement();
    VarDeclStmt parse_var_decl();
    StmtPtr parse_return();
    StmtPtr parse_if();
    std::unique_ptr<BlockStmt> parse_block();
    ExprPtr parse_function_pointer_expr();
    ExprPtr parse_struct_literal();

    ExprPtr parse_identifier();
    ExprPtr parse_literal();
    ExprPtr parse_grouped();
    ExprPtr parse_cast();
    ExprPtr parse_sizeof();
    ExprPtr parse_init_list();

    ExprPtr parse_unary();
    ExprPtr parse_binary(ExprPtr left, int min_precedence);
    ExprPtr parse_primary();
    ExprPtr parse_call(ExprPtr left);
    ExprPtr parse_member_access(ExprPtr left);

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
