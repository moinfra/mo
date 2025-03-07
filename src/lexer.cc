//
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
    case TokenType::Return:
        return "return";
    case TokenType::Type:
        return "type";
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
    case TokenType::BooleanLiteral:
        return "<boolean>";
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
    case TokenType::AddAssign:
        return "+=";
    case TokenType::SubAssign:
        return "-=";
    case TokenType::MulAssign:
        return "*=";
    case TokenType::DivAssign:
        return "/=";
    case TokenType::ModAssign:
        return "%=";
    case TokenType::AndAssign:
        return "&=";
    case TokenType::OrAssign:
        return "|=";
    case TokenType::XorAssign:
        return "^=";
    case TokenType::LSAssign:
        return "<<=";
    case TokenType::RSAssign:
        return ">>=";
    case TokenType::LShift:
        return "<<";
    case TokenType::RShift:
        return ">>";
    case TokenType::Increment:
        return "++";
    case TokenType::Decrement:
        return "--";
    case TokenType::DoubleDot:
        return "..";
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
        return "<unknown>";
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

Token Lexer::parse_complex_operators()
{
    int start_line = current_line;
    int start_col = current_col;
    char c = input[pos];
    advance(); // skip first char

    if (peek() == '=')
    {
        advance(); // skip '='
        switch (c)
        {
        case '+':
            return Token(TokenType::AddAssign, start_line, start_col, current_line, current_col - 1, "+=");
        case '-':
            return Token(TokenType::SubAssign, start_line, start_col, current_line, current_col - 1, "-=");
        case '*':
            return Token(TokenType::MulAssign, start_line, start_col, current_line, current_col - 1, "*=");
        case '/':
            return Token(TokenType::DivAssign, start_line, start_col, current_line, current_col - 1, "/=");
        case '%':
            return Token(TokenType::ModAssign, start_line, start_col, current_line, current_col - 1, "%=");
        case '&':
            return Token(TokenType::AndAssign, start_line, start_col, current_line, current_col - 1, "&=");
        case '|':
            return Token(TokenType::OrAssign, start_line, start_col, current_line, current_col - 1, "|=");
        case '^':
            return Token(TokenType::XorAssign, start_line, start_col, current_line, current_col - 1, "^=");
        case '<':
            return Token(TokenType::Le, start_line, start_col, current_line, current_col - 1, "<=");
        case '>':
            return Token(TokenType::Ge, start_line, start_col, current_line, current_col - 1, ">=");
        default:
            break;
        }
    }

    // other cases
    switch (c)
    {
    case '+':
        if (peek() == '+')
        {
            advance(); // skip second '+'
            return Token(TokenType::Increment, start_line, start_col, current_line, current_col - 1, "++");
        }
        return Token(TokenType::Plus, start_line, start_col, current_line, current_col - 1, "+");
    case '-':
        if (peek() == '>')
        {
            advance(); // skip '>'
            return Token(TokenType::Arrow, start_line, start_col, current_line, current_col - 1, "->");
        }
        if (peek() == '-')
        {
            advance(); // skip second '-'
            return Token(TokenType::Decrement, start_line, start_col, current_line, current_col - 1, "--");
        }
        return Token(TokenType::Minus, start_line, start_col, current_line, current_col - 1, "-");
    case '*':
        return Token(TokenType::Star, start_line, start_col, current_line, current_col - 1, "*");
    case '/':
        return Token(TokenType::Slash, start_line, start_col, current_line, current_col - 1, "/");
    case '%':
        return Token(TokenType::Modulo, start_line, start_col, current_line, current_col - 1, "%");
    case '&':
        if (peek() == '&')
        {
            advance(); // skip second '&'
            return Token(TokenType::And, start_line, start_col, current_line, current_col - 1, "&&");
        }
        return Token(TokenType::Ampersand, start_line, start_col, current_line, current_col - 1, "&");
    case '|':
        if (peek() == '|')
        {
            advance(); // skip second '|'
            return Token(TokenType::Or, start_line, start_col, current_line, current_col - 1, "||");
        }
        return Token(TokenType::Pipe, start_line, start_col, current_line, current_col - 1, "|");
    case '^':
        return Token(TokenType::Caret, start_line, start_col, current_line, current_col - 1, "^");
    case '<':
        if (peek() == '<')
        {
            advance(); // skip second '<'
            if (peek() == '=')
            {
                advance(); // skip '='
                return Token(TokenType::LSAssign, start_line, start_col, current_line, current_col - 1, "<<=");
            }
            else
            {
                return Token(TokenType::LShift, start_line, start_col, current_line, current_col - 1, "<<");
            }
        }
        else
        {
            return Token(TokenType::Lt, start_line, start_col, current_line, current_col - 1, "<");
        }
    case '>':
        if (peek() == '>')
        {
            advance(); // skip second '>'
            if (peek() == '=')
            {
                advance(); // skip '='
                return Token(TokenType::RSAssign, start_line, start_col, current_line, current_col - 1, ">>=");
            }
            else
            {
                return Token(TokenType::RShift, start_line, start_col, current_line, current_col - 1, ">>");
            }
        }
        else
        {
            return Token(TokenType::Gt, start_line, start_col, current_line, current_col - 1, ">");
        }
    default:
        errors.push_back({"Unexpected character: " + std::string(1, c), current_line, current_col});
        return Token(TokenType::Invalid, start_line, start_col, current_line, current_col - 1, std::string(1, c));
    }
}
Token Lexer::parse_double_char_operators()
{
    int start_line = current_line;
    int start_col = current_col;
    char c = input[pos];
    advance(); // Skip the first character

    if (peek() == c)
    {
        advance(); // Skip the second character
        switch (c)
        {
        case '<':
            return Token(TokenType::LShift, start_line, start_col, current_line, current_col - 1, "<<");
        case '>':
            return Token(TokenType::RShift, start_line, start_col, current_line, current_col - 1, ">>");
        case '+':
            return Token(TokenType::Increment, start_line, start_col, current_line, current_col - 1, "++");
        case '-':
            return Token(TokenType::Decrement, start_line, start_col, current_line, current_col - 1, "--");
        case '.':
            return Token(TokenType::DoubleDot, start_line, start_col, current_line, current_col - 1, "..");
        default:
            break;
        }
    }

    // If no match, return the single character token
    switch (c)
    {
    case '<':
        return Token(TokenType::Lt, start_line, start_col, current_line, current_col - 1, "<");
    case '>':
        return Token(TokenType::Gt, start_line, start_col, current_line, current_col - 1, ">");
    case '+':
        return Token(TokenType::Plus, start_line, start_col, current_line, current_col - 1, "+");
    case '-':
        return Token(TokenType::Minus, start_line, start_col, current_line, current_col - 1, "-");
    case '.':
        return Token(TokenType::Dot, start_line, start_col, current_line, current_col - 1, ".");
    default:
        errors.push_back({"Unexpected character: " + std::string(1, c), current_line, current_col});
        return Token(TokenType::Invalid, start_line, start_col, current_line, current_col - 1, std::string(1, c));
    }
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
    case '+':
    case '-':
    case '*':
    case '/':
    case '%':
    case '&':
    case '|':
    case '^':
    case '<':
    case '>':
        return parse_complex_operators();
    case '=':
        return parse_equal_or_eq();
    case '!':
        return parse_ne();
    case ':':
        return parse_double_colon_or_colon();
    case '.':
        return parse_double_char_operators();
    case ';':
        return parse_single_char(TokenType::Semicolon, ";");
    case ',':
        return parse_single_char(TokenType::Comma, ",");
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
    case '~':
        return parse_single_char(TokenType::Tilde, "~");
    default:
        errors.push_back({"Unexpected character: " + std::string(1, c), current_line, current_col});
        advance();
        return Token(TokenType::Invalid, current_line, current_col - 1, current_line, current_col - 1, std::string(1, c));
    }
}

#if defined(MO_UNICODE)

std::pair<char32_t, int> Lexer::decode_utf8()
{
    if (pos >= input.size())
    {
        return {U_SENTINEL, 0};
    }
    unsigned char c = input[pos];
    if (c <= 0x7F)
    {
        return {c, 1};
    }
    else if ((c & 0xE0) == 0xC0)
    {
        if (pos + 1 >= input.size())
            return {U_SENTINEL, 0};
        char32_t cp = (c & 0x1F) << 6;
        cp |= (input[pos + 1] & 0x3F);
        return {cp, 2};
    }
    else if ((c & 0xF0) == 0xE0)
    {
        if (pos + 2 >= input.size())
            return {U_SENTINEL, 0};
        char32_t cp = (c & 0x0F) << 12;
        cp |= (input[pos + 1] & 0x3F) << 6;
        cp |= (input[pos + 2] & 0x3F);
        return {cp, 3};
    }
    else if ((c & 0xF8) == 0xF0)
    {
        if (pos + 3 >= input.size())
            return {U_SENTINEL, 0};
        char32_t cp = (c & 0x07) << 18;
        cp |= (input[pos + 1] & 0x3F) << 12;
        cp |= (input[pos + 2] & 0x3F) << 6;
        cp |= (input[pos + 3] & 0x3F);
        return {cp, 4};
    }
    else
    {
        return {U_SENTINEL, 0};
    }
}

#endif // MO_UNICODE

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
    int start_line = current_line;
    int start_col = current_col;
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
    errors.push_back({"Unclosed block comment", start_line, start_col});
}

Token Lexer::parse_number()
{
    int start_line = current_line;
    int start_col = current_col;
    std::string numStr;
    bool isFloat = false;

    if (peek() == '0' && (peek(1) == 'x' || peek(1) == 'b'))
    {
        int start_line = current_line;
        int start_col = current_col;

        numStr += input[pos];
        advance();
        numStr += input[pos];
        advance();

        if (numStr.back() == 'x')
        {
            bool has_digits = false;
            while (pos < input.size() && isxdigit(input[pos]))
            {
                has_digits = true;
                numStr += input[pos];
                advance();
            }
            if (!has_digits)
            {
                errors.push_back({"Invalid hexadecimal literal: no digits after 0x", start_line, start_col});
                return Token(TokenType::Invalid, start_line, start_col, current_line, current_col - 1, numStr);
            }
        }
        else if (numStr.back() == 'b')
        {
            bool has_digits = false;
            while (pos < input.size() && (input[pos] == '0' || input[pos] == '1'))
            {
                has_digits = true;
                numStr += input[pos];
                advance();
            }
            if (!has_digits)
            {
                errors.push_back({"Invalid binary literal: no digits after 0b", start_line, start_col});
                return Token(TokenType::Invalid, start_line, start_col, current_line, current_col - 1, numStr);
            }
        }
    }
    else
    {
        while (pos < input.size() && isdigit(input[pos]))
        {
            numStr += input[pos];
            advance();
        }

        if (pos < input.size() && input[pos] == '.')
        {
            isFloat = true;
            numStr += input[pos];
            advance();
            if (pos >= input.size() || !isdigit(input[pos]))
            {
                errors.push_back({"Invalid float literal: missing fractional part", start_line, start_col});
                return Token(TokenType::Invalid, start_line, start_col, current_line, current_col - 1, numStr);
            }
            while (pos < input.size() && isdigit(input[pos]))
            {
                numStr += input[pos];
                advance();
            }
        }

        if (pos < input.size() && (input[pos] == 'e' || input[pos] == 'E'))
        {
            isFloat = true;
            numStr += input[pos];
            advance();
            if (pos < input.size() && (input[pos] == '+' || input[pos] == '-'))
            {
                numStr += input[pos];
                advance();
            }
            if (pos >= input.size() || !isdigit(input[pos]))
            {
                errors.push_back({"Invalid exponent in float literal: missing exponent", start_line, start_col});
                return Token(TokenType::Invalid, start_line, start_col, current_line, current_col - 1, numStr);
            }
            while (pos < input.size() && isdigit(input[pos]))
            {
                numStr += input[pos];
                advance();
            }
        }

        if (pos < input.size() && isalpha(input[pos]))
        {
            errors.push_back({"Invalid decimal literal: invalid suffix", start_line, start_col});
            while (pos < input.size() && isalpha(input[pos]))
            {
                numStr += input[pos];
                advance();
            }
            return Token(TokenType::Invalid, start_line, start_col, current_line, current_col - 1, numStr);
        }
    }

    if (numStr.empty())
    {
        errors.push_back({"Empty number literal", start_line, start_col});
        return Token(TokenType::Invalid, start_line, start_col, current_line, current_col - 1, "");
    }

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
            advance(); // end "
            return Token(TokenType::StringLiteral, start_line, start_col, current_line, current_col - 1, str);
        }
        else
        {
            str += c;
        }
        advance();
    }

    errors.push_back({"Unclosed string literal", start_line, start_col});
    return Token(TokenType::Invalid, start_line, start_col, current_line, current_col - 1, str);
}

Token Lexer::parse_identifier_or_keyword()
{
    int start_line = current_line;
    int start_col = current_col;
    std::string ident;

    if (pos >= input.size())
    {
        return Token(TokenType::Invalid, start_line, start_col, current_line, current_col, "");
    }

    // handle initial char
    size_t initial_pos = pos;
#if defined(MO_UNICODE)
    auto [cp, bytes] = decode_utf8();
    if (cp == U_SENTINEL)
    {
        errors.push_back({"Invalid UTF-8 sequence", current_line, current_col});
        pos++;
        current_col++;
        return Token(TokenType::Invalid, start_line, start_col, current_line, current_col, "");
    }
#else
    char32_t cp = input[pos];
    int bytes = 1;
#endif // MO_UNICODE

    // Check if the first character is a valid start of an identifier (ID_Start or underscore/letter)
#if defined(MO_UNICODE)
    bool valid_start = (cp == '_') || (cp >= 'a' && cp <= 'z') || (cp >= 'A' && cp <= 'Z') || u_isIDStart(cp);
#else
    bool valid_start = (cp == '_') || (cp >= 'a' && cp <= 'z') || (cp >= 'A' && cp <= 'Z');
#endif // MO_UNICODE
    if (!valid_start)
    {
        pos = initial_pos;
        return Token(TokenType::Invalid, start_line, start_col, current_line, current_col, "");
    }

    // add the first character to the identifier
    ident += input.substr(initial_pos, bytes);
    pos += bytes;
    current_col++; // every unicode character takes 1 column

    // handle following characters
    while (pos < input.size())
    {
        size_t current_pos = pos;

#if defined(MO_UNICODE)
        auto [next_cp, next_bytes] = decode_utf8();
        if (next_cp == U_SENTINEL)
        {
            errors.push_back({"Invalid UTF-8 sequence", current_line, current_col});
            pos++;
            current_col++;
            break;
        }

        bool valid_continue = (next_cp >= '0' && next_cp <= '9') || (next_cp == '_') || u_isIDPart(next_cp);
#else
        char next_cp = input[pos];
        int next_bytes = 1;
        bool valid_continue = (next_cp >= '0' && next_cp <= '9') ||
                              (next_cp == '_') ||
                              (next_cp >= 'a' && next_cp <= 'z') ||
                              (next_cp >= 'A' && next_cp <= 'Z');
#endif // MO_UNICODE

        if (valid_continue)
        {
            ident += input.substr(current_pos, next_bytes);
            pos += next_bytes;
            current_col++;
        }
        else
        {
            pos = current_pos;
            break;
        }
    }

    if (ident == "true")
    {
        return Token(TokenType::BooleanLiteral, start_line, start_col, current_line, current_col - 1, "true");
    }
    else if (ident == "false")
    {
        return Token(TokenType::BooleanLiteral, start_line, start_col, current_line, current_col - 1, "false");
    }

    auto it = keywords.find(ident);
    TokenType type = (it != keywords.end()) ? it->second : TokenType::Identifier;
    return Token(type, start_line, start_col, current_line, current_col - 1, ident);
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
    return Token(TokenType::Pipe, start_line, start_col, current_line, current_col - 1, "|");
}

Token Lexer::parse_single_char(TokenType type, const std::string &lexeme)
{
    int start_line = current_line;
    int start_col = current_col;
    advance();
    return Token(type, start_line, start_col, current_line, current_col - 1, lexeme);
}
