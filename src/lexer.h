#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <unordered_map>
#include <vector>
#include <cctype>
#include <stdexcept>

enum class TokenType
{
    // Keywords
    Let,
    Struct,
    Impl,
    Fn,
    Return,
    Int,
    Float,
    Const,
    Sizeof,
    Cast,
    If,
    Else,
    While,
    For,
    // Identifiers
    Identifier,
    // Literals
    IntegerLiteral,
    FloatLiteral,
    StringLiteral,
    // s
    Dot,         // .
    Arrow,       // ->
    DoubleColon, // ::
    Assign,      // =
    Colon,       // :
    Semicolon,   // ;
    Comma,       // ,
    Star,        // *
    Ampersand,   // &
    LParen,      // (
    RParen,      // )
    LBrace,      // {
    RBrace,      // }
    LBracket,    // [
    RBracket,    // ]
    Plus,        // +
    Minus,       // -
    Divide,      // /
    Modulo,      // %
    Eq,          // ==
    Ne,          // !=
    Lt,          // <
    Le,          // <=
    Gt,          // >
    Ge,          // >=
    And,         // &&
    Or,          // ||
    // End of File
    Eof,
};

struct Token
{

    TokenType type;
    int start_line;
    int start_col;
    int end_line;
    int end_col;
    std::string lexeme;

    Token(TokenType type, int start_line, int start_col, int end_line, int end_col, const std::string &lexeme);
};

class Lexer
{
public:
    Lexer(const std::string &input);
    Lexer(Lexer &&other) noexcept;
    Token next_token();

private:
    std::string input;
    size_t pos;
    int current_line;
    int current_col;

    void advance();
    char peek(size_t offset = 0) const;
    void skip_whitespace_and_comments();
    void skip_line_comment();
    void skip_block_comment();
    Token parse_number();
    Token parse_string();
    Token parse_identifier_or_keyword();
    Token parse_arrow_or_minus();
    Token parse_double_colon_or_colon();
    Token parse_dot();
    Token parse_equal_or_eq();
    Token parse_ne();
    Token parse_le_or_lt();
    Token parse_ge_or_gt();
    Token parse_and();
    Token parse_or();
    Token parse_single_char(TokenType type, const std::string &lexeme);
};

class LexerError : public std::exception
{
public:
    explicit LexerError(const std::string &message)
        : message_(message) {}

    virtual const char *what() const noexcept override
    {
        return message_.c_str();
    }

private:
    std::string message_;
};

std::string token_to_string(const Token &token);
std::string token_type_to_string(TokenType type);

#endif // LEXER_H
