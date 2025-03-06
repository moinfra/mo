// lexer.h

#pragma once

#include <string>
#include <unordered_map>
#include <vector>
#include <cctype>
#include <stdexcept>
#include <cstdio>
#if defined(MO_UNICODE)
#include <unicode/uchar.h>
#endif // MO_UNICODE


struct LexerError
{
    std::string message;
    int line;
    int col;
};

enum class TokenType
{
    Invalid,
    // Keywords
    Let,
    Struct,
    Impl,
    Fn,
    This,
    Return,
    Type,
    // Types
    Int,
    Float,
    String,

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
    BooleanLiteral,
    FloatLiteral,
    StringLiteral,
    // Operators
    Dot,         // .
    Arrow,       // ->
    DoubleColon, // ::
    Assign,      // =
    AddAssign,   // +=
    SubAssign,   // -=
    MulAssign,   // *=
    DivAssign,   // /=
    ModAssign,   // %=
    AndAssign,   // &=
    OrAssign,    // |=
    XorAssign,   // ^=
    LSAssign,    // <<=
    RSAssign,    // >>=
    LShift,      // <<
    RShift,      // >>
    Increment,   // ++
    Decrement,   // --
    DoubleDot,   // ..
    Colon,       // :
    Semicolon,   // ;
    Comma,       // ,
    Star,        // *
    Ampersand,   // & also used for bitwise and
    LParen,      // (
    RParen,      // )
    LBrace,      // {
    RBrace,      // }
    LBracket,    // [
    RBracket,    // ]
    Plus,        // +
    Minus,       // -
    Slash,       // /
    Modulo,      // %
    Eq,          // ==
    Ne,          // !=
    Lt,          // <
    Le,          // <=
    Gt,          // >
    Ge,          // >=
    And,         // &&
    Or,          // ||
    Not,         // !
    // Bitwise operators
    // Ampersand
    Pipe,  // |
    Caret, // ^
    Tilde, // ~
    // End of File
    Eof,
};

const std::unordered_map<std::string, TokenType> keywords = {
    {"let", TokenType::Let},
    {"struct", TokenType::Struct},
    {"impl", TokenType::Impl},
    {"fn", TokenType::Fn},
    {"this", TokenType::This},
    {"type", TokenType::Type},
    {"return", TokenType::Return},
    {"int", TokenType::Int},
    {"i8", TokenType::Int},
    {"i16", TokenType::Int},
    {"i32", TokenType::Int},
    {"i64", TokenType::Int},
    {"u8", TokenType::Int},
    {"u16", TokenType::Int},
    {"u32", TokenType::Int},
    {"u64", TokenType::Int},
    {"float", TokenType::Float},
    {"f32", TokenType::Float},
    {"f64", TokenType::Float},
    {"const", TokenType::Const},
    {"sizeof", TokenType::Sizeof},
    {"cast", TokenType::Cast},
    {"if", TokenType::If},
    {"else", TokenType::Else},
    {"while", TokenType::While},
    {"for", TokenType::For},
};

struct Token
{

    TokenType type;
    int start_line;
    int start_col;
    int end_line;
    int end_col;
    std::string lexeme;
    Token();
    Token(TokenType type, int start_line, int start_col, int end_line, int end_col, const std::string &lexeme);
};

class Lexer
{
public:
    Lexer(const std::string &input);
    Lexer(Lexer &&other) noexcept;
    Token next_token();
    const std::vector<LexerError>& get_errors() const { return errors; }

private:
    std::string input;
    size_t pos;
    int current_line;
    int current_col;
    std::vector<LexerError> errors;

    #if defined(MO_UNICODE)
    std::pair<char32_t, int> decode_utf8();
    #endif // MO_UNICODE
    
    void advance();
    char peek(size_t offset = 0) const;
    void skip_whitespace_and_comments();
    void skip_line_comment();
    void skip_block_comment();
    Token parse_complex_operators();
    Token parse_double_char_operators();
    Token parse_number();
    Token parse_string();
    Token parse_identifier_or_keyword();
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

std::string token_to_string(const Token &token);
std::string token_type_to_string(TokenType type);
