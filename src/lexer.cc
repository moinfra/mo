//===----------------------------------------------------------------------===//
//                             Headers
//===----------------------------------------------------------------------===//

#include "lexer.h"
#include <string>
#include <unordered_map>

#include "utils.h"

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

std::string token_type_to_string(TokenType type)
{
    switch (type)
    {
    case TokenType::Invalid:
        return "<invalid>";
    case TokenType::Let:
        return "let";
    case TokenType::Struct:
        return "struct";
    case TokenType::Impl:
        return "impl";
    case TokenType::Fn:
        return "fn";
    case TokenType::This:
        return "this";
    case TokenType::Type:
        return "type";
    case TokenType::Return:
        return "return";
    case TokenType::Int:
        return "int";
    case TokenType::Float:
        return "float";
    case TokenType::String:
        return "string";
    case TokenType::Const:
        return "const";
    case TokenType::Sizeof:
        return "sizeof";
    case TokenType::Cast:
        return "cast";
    case TokenType::If:
        return "if";
    case TokenType::Else:
        return "else";
    case TokenType::While:
        return "while";
    case TokenType::For:
        return "for";
    case TokenType::Identifier:
        return "<id>";
    case TokenType::IntegerLiteral:
        return "<integer>";
    case TokenType::FloatLiteral:
        return "<float>";
    case TokenType::StringLiteral:
        return "<string>";
    case TokenType::Dot:
        return ".";
    case TokenType::Arrow:
        return "->";
    case TokenType::DoubleColon:
        return "::";
    case TokenType::Assign:
        return "=";
    case TokenType::Colon:
        return ":";
    case TokenType::Semicolon:
        return ";";
    case TokenType::Comma:
        return ",";
    case TokenType::Star:
        return "*";
    case TokenType::Ampersand:
        return "&";
    case TokenType::LParen:
        return "(";
    case TokenType::RParen:
        return ")";
    case TokenType::LBrace:
        return "{";
    case TokenType::RBrace:
        return "}";
    case TokenType::LBracket:
        return "[";
    case TokenType::RBracket:
        return "]";
    case TokenType::Plus:
        return "+";
    case TokenType::Minus:
        return "-";
    case TokenType::Slash:
        return "/";
    case TokenType::Modulo:
        return "%";
    case TokenType::Eq:
        return "==";
    case TokenType::Ne:
        return "!=";
    case TokenType::Lt:
        return "<";
    case TokenType::Le:
        return "<=";
    case TokenType::Gt:
        return ">";
    case TokenType::Ge:
        return ">=";
    case TokenType::And:
        return "&&";
    case TokenType::Or:
        return "||";
    case TokenType::Not:
        return "!";
    case TokenType::Pipe:
        return "|";
    case TokenType::Caret:
        return "^";
    case TokenType::Tilde:
        return "~";
    case TokenType::Eof:
        return "<eof>";
    default:
        MO_ASSERT(false, "Unknown token type %d", (int)type);
        return "<unknown>"; // 添加 default 情况
    }
}

std::string token_to_string(const Token &token)
{
    std::string result = token.lexeme;
    if (result.empty())
    {
        return token_type_to_string(token.type);
    }
    return result;
}

//===----------------------------------------------------------------------===//
//                             Token Class Implementation
//===----------------------------------------------------------------------===//

Token::Token() : type(TokenType::Invalid), start_line(0), start_col(0), end_line(0), end_col(0), lexeme("") {}

Token::Token(TokenType type, int start_line, int start_col, int end_line, int end_col, const std::string &lexeme)
    : type(type), start_line(start_line), start_col(start_col), end_line(end_line), end_col(end_col), lexeme(lexeme) {}

//===----------------------------------------------------------------------===//
//                             Lexer Class Implementation
//===----------------------------------------------------------------------===//

Lexer::Lexer(const std::string &input) : input(input), pos(0), current_line(1), current_col(1) {}

Lexer::Lexer(Lexer &&other) noexcept : input(std::move(other.input)),
                                       pos(other.pos), current_line(other.current_line), current_col(other.current_col)
{
}

Token Lexer::next_token()
{
    skip_whitespace_and_comments();

    if (pos >= input.size())
    {
        return Token(TokenType::Eof, current_line, current_col, current_line, current_col, "");
    }

    char c = input[pos];
    if (isdigit(c) || (c == '.' && isdigit(peek(1))))
    {
        return parse_number();
    }
    if (c == '"')
    {
        return parse_string();
    }
    if (isalpha(c) || c == '_')
    {
        return parse_identifier_or_keyword();
    }

    switch (c)
    {
    case '-':
        return parse_arrow_or_minus();
    case ':':
        return parse_double_colon_or_colon();
    case '.':
        return parse_dot();
    case ';':
        return parse_single_char(TokenType::Semicolon, ";");
    case ',':
        return parse_single_char(TokenType::Comma, ",");
    case '=':
        return parse_equal_or_eq();
    case '!':
        return parse_ne();
    case '<':
        return parse_le_or_lt();
    case '>':
        return parse_ge_or_gt();
    case '&':
        return parse_and();
    case '|':
        return parse_or();
    case '(':
        return parse_single_char(TokenType::LParen, "(");
    case ')':
        return parse_single_char(TokenType::RParen, ")");
    case '{':
        return parse_single_char(TokenType::LBrace, "{");
    case '}':
        return parse_single_char(TokenType::RBrace, "}");
    case '[':
        return parse_single_char(TokenType::LBracket, "[");
    case ']':
        return parse_single_char(TokenType::RBracket, "]");
    case '*':
        return parse_single_char(TokenType::Star, "*");
    case '+':
        return parse_single_char(TokenType::Plus, "+");
    case '/':
        return parse_single_char(TokenType::Slash, "/");
    case '%':
        return parse_single_char(TokenType::Modulo, "%");
    default:
        throw LexerError("Unexpected character: " + std::string(1, c));
    }
}

void Lexer::advance()
{
    if (pos >= input.size())
        return;
    if (input[pos] == '\n')
    {
        current_line++;
        current_col = 1;
    }
    else
    {
        current_col++;
    }
    pos++;
}

char Lexer::peek(size_t offset) const
{
    if (pos + offset >= input.size())
        return '\0';
    return input[pos + offset];
}

void Lexer::skip_whitespace_and_comments()
{
    while (pos < input.size())
    {
        char c = input[pos];
        if (isspace(c))
        {
            advance();
        }
        else if (c == '/')
        {
            if (peek(1) == '/')
            {
                skip_line_comment();
            }
            else if (peek(1) == '*')
            {
                skip_block_comment();
            }
            else
            {
                break;
            }
        }
        else
        {
            break;
        }
    }
}

void Lexer::skip_line_comment()
{
    while (pos < input.size() && input[pos] != '\n')
    {
        advance();
    }
    if (pos < input.size())
        advance(); // Skip the newline
}

void Lexer::skip_block_comment()
{
    advance(); // Skip '/'
    advance(); // Skip '*'
    while (pos < input.size())
    {
        if (input[pos] == '*' && peek(1) == '/')
        {
            advance(); // Skip '*'
            advance(); // Skip '/'
            return;
        }
        advance();
    }
    throw LexerError("Unclosed block comment");
}

Token Lexer::parse_number()
{
    int start_line = current_line;
    int start_col = current_col;
    std::string numStr;
    bool isFloat = false;

    // Parse the integer part
    while (pos < input.size() && isdigit(input[pos]))
    {
        numStr += input[pos];
        advance();
    }

    // Parse the fractional part (if any)
    if (pos < input.size() && input[pos] == '.')
    {
        isFloat = true;
        numStr += input[pos]; // Add the decimal point
        advance();

        // Parse the digits after the decimal point
        while (pos < input.size() && isdigit(input[pos]))
        {
            numStr += input[pos];
            advance();
        }

        // Ensure there is at least one digit after the decimal point
        if (numStr.back() == '.')
        {
            throw std::runtime_error("Invalid float literal: " + numStr + " (missing fractional part)");
        }
    }

    // Parse the exponent part (if any, for scientific notation)
    if (pos < input.size() && (input[pos] == 'e' || input[pos] == 'E'))
    {
        isFloat = true;
        numStr += input[pos]; // Add 'e' or 'E'
        advance();

        // Parse the optional sign
        if (pos < input.size() && (input[pos] == '+' || input[pos] == '-'))
        {
            numStr += input[pos];
            advance();
        }

        // Parse the exponent digits
        if (pos < input.size() && isdigit(input[pos]))
        {
            while (pos < input.size() && isdigit(input[pos]))
            {
                numStr += input[pos];
                advance();
            }
        }
        else
        {
            throw std::runtime_error("Invalid exponent in float literal: " + numStr + " (missing exponent)");
        }
    }

    // Ensure the generated numStr is not empty
    if (numStr.empty())
    {
        throw std::runtime_error("Empty number literal");
    }

    // Determine the token type
    TokenType type = isFloat ? TokenType::FloatLiteral : TokenType::IntegerLiteral;
    return Token(type, start_line, start_col, current_line, current_col - 1, numStr);
}

Token Lexer::parse_string()
{
    int start_line = current_line;
    int start_col = current_col;
    advance(); // Skip opening "
    std::string str;
    bool escape = false;

    while (pos < input.size())
    {
        char c = input[pos];
        if (escape)
        {
            switch (c)
            {
            case 'n':
                str += '\n';
                break;
            case 't':
                str += '\t';
                break;
            case 'r':
                str += '\r';
                break;
            case '"':
                str += '"';
                break;
            case '\\':
                str += '\\';
                break;
            default:
                str += '\\';
                str += c;
                break;
            }
            escape = false;
        }
        else if (c == '\\')
        {
            escape = true;
        }
        else if (c == '"')
        {
            advance(); // Skip closing "
            return Token(TokenType::StringLiteral, start_line, start_col, current_line, current_col - 1, str);
        }
        else
        {
            str += c;
        }
        advance();
    }

    throw LexerError("Unclosed string literal");
}

Token Lexer::parse_identifier_or_keyword()
{
    int start_line = current_line;
    int start_col = current_col;
    std::string ident;

    while (pos < input.size() && (isalnum(input[pos]) || input[pos] == '_'))
    {
        ident += input[pos];
        advance();
    }

    static const std::unordered_map<std::string, TokenType> keywords = {
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

    auto it = keywords.find(ident);
    TokenType type = (it != keywords.end()) ? it->second : TokenType::Identifier;
    return Token(type, start_line, start_col, current_line, current_col - 1, ident);
}

Token Lexer::parse_arrow_or_minus()
{
    int start_line = current_line;
    int start_col = current_col;
    advance(); // Skip '-'
    if (peek() == '>')
    {
        advance(); // Skip '>'
        return Token(TokenType::Arrow, start_line, start_col, current_line, current_col - 1, "->");
    }
    return Token(TokenType::Minus, start_line, start_col, current_line, current_col - 1, "-");
}

Token Lexer::parse_double_colon_or_colon()
{
    int start_line = current_line;
    int start_col = current_col;
    advance(); // Skip ':'
    if (peek() == ':')
    {
        advance(); // Skip ':'
        return Token(TokenType::DoubleColon, start_line, start_col, current_line, current_col - 1, "::");
    }
    return Token(TokenType::Colon, start_line, start_col, current_line, current_col - 1, ":");
}

Token Lexer::parse_dot()
{
    return parse_single_char(TokenType::Dot, ".");
}

Token Lexer::parse_equal_or_eq()
{
    int start_line = current_line;
    int start_col = current_col;
    advance(); // Skip '='
    if (peek() == '=')
    {
        advance(); // Skip '='
        return Token(TokenType::Eq, start_line, start_col, current_line, current_col - 1, "==");
    }
    return Token(TokenType::Assign, start_line, start_col, current_line, current_col - 1, "=");
}

Token Lexer::parse_ne()
{
    int start_line = current_line;
    int start_col = current_col;
    advance(); // Skip '!'
    if (peek() == '=')
    {
        advance(); // Skip '='
        return Token(TokenType::Ne, start_line, start_col, current_line, current_col - 1, "!=");
    }
    else
    {
        return Token(TokenType::Not, start_line, start_col, current_line, current_col - 1, "!");
    }
}

Token Lexer::parse_le_or_lt()
{
    int start_line = current_line;
    int start_col = current_col;
    advance(); // Skip '<'
    if (peek() == '=')
    {
        advance(); // Skip '='
        return Token(TokenType::Le, start_line, start_col, current_line, current_col - 1, "<=");
    }
    return Token(TokenType::Lt, start_line, start_col, current_line, current_col - 1, "<");
}

Token Lexer::parse_ge_or_gt()
{
    int start_line = current_line;
    int start_col = current_col;
    advance(); // Skip '>'
    if (peek() == '=')
    {
        advance(); // Skip '='
        return Token(TokenType::Ge, start_line, start_col, current_line, current_col - 1, ">=");
    }
    return Token(TokenType::Gt, start_line, start_col, current_line, current_col - 1, ">");
}

Token Lexer::parse_and()
{
    int start_line = current_line;
    int start_col = current_col;
    advance(); // Skip '&'
    if (peek() == '&')
    {
        advance(); // Skip '&'
        return Token(TokenType::And, start_line, start_col, current_line, current_col - 1, "&&");
    }
    return Token(TokenType::Ampersand, start_line, start_col, current_line, current_col - 1, "&");
}

Token Lexer::parse_or()
{
    int start_line = current_line;
    int start_col = current_col;
    advance(); // Skip '|'
    if (peek() == '|')
    {
        advance(); // Skip '|'
        return Token(TokenType::Or, start_line, start_col, current_line, current_col - 1, "||");
    }
    throw LexerError("Unexpected '|'");
}

Token Lexer::parse_single_char(TokenType type, const std::string &lexeme)
{
    int start_line = current_line;
    int start_col = current_col;
    advance();
    return Token(type, start_line, start_col, current_line, current_col - 1, lexeme);
}
