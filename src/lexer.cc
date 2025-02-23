#include "lexer.h"

Token::Token(TokenType type, int start_line, int start_col, int end_line, int end_col, const std::string &lexeme)
    : type(type), start_line(start_line), start_col(start_col), end_line(end_line), end_col(end_col), lexeme(lexeme) {}

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
    if (isdigit(c))
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
        return parse_single_char(TokenType::Divide, "/");
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

    while (pos < input.size() && (isdigit(input[pos]) || input[pos] == '.'))
    {
        if (input[pos] == '.')
        {
            if (isFloat)
                break;
            isFloat = true;
        }
        numStr += input[pos];
        advance();
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
        {"return", TokenType::Return},
        {"int", TokenType::Int},
        {"float", TokenType::Float},
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
    throw LexerError("Unexpected '!'");
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

std::string tokenToString(const Token &token)
{
    std::string result = token.lexeme;
    if (result.empty())
    {
        return tokenTypeToString(token.type);
    }
    return result;
}

std::string tokenTypeToString(TokenType type)
{
    std::string result;
    switch (type)
    {
    case TokenType::Let:
        result = "let";
        break;
    case TokenType::Struct:
        result = "struct";
        break;
    case TokenType::Impl:
        result = "impl";
        break;
    case TokenType::Fn:
        result = "fn";
        break;
    case TokenType::Return:
        result = "return";
        break;
    case TokenType::Int:
        result = "int";
        break;
    case TokenType::Float:
        result = "float";
        break;
    case TokenType::Const:
        result = "const";
        break;
    case TokenType::Sizeof:
        result = "sizeof";
        break;
    case TokenType::Cast:
        result = "cast";
        break;
    case TokenType::If:
        result = "if";
        break;
    case TokenType::Else:
        result = "else";
        break;
    case TokenType::While:
        result = "while";
        break;
    case TokenType::For:
        result = "for";
        break;
    case TokenType::Identifier:
        result = "<id>";
        break;
    case TokenType::IntegerLiteral:
        result = "<integer>";
        break;
    case TokenType::FloatLiteral:
        result = "<float>";
        break;
    case TokenType::StringLiteral:
        result = "<string>";
        break;
    case TokenType::Dot:
        result = ".";
        break;
    case TokenType::Arrow:
        result = "->";
        break;
    case TokenType::DoubleColon:
        result = "::";
        break;
    case TokenType::Assign:
        result = "=";
        break;
    case TokenType::Colon:
        result = ":";
        break;
    case TokenType::Semicolon:
        result = ";";
        break;
    case TokenType::Comma:
        result = ",";
        break;
    case TokenType::Star:
        result = "*";
        break;
    case TokenType::Ampersand:
        result = "&";
        break;
    case TokenType::LParen:
        result = "(";
        break;
    case TokenType::RParen:
        result = ")";
        break;
    case TokenType::LBrace:
        result = "{";
        break;
    case TokenType::RBrace:
        result = "}";
        break;
    case TokenType::LBracket:
        result = "[";
        break;
    case TokenType::RBracket:
        result = "]";
        break;
    case TokenType::Plus:
        result = "+";
        break;
    case TokenType::Minus:
        result = "-";
        break;
    case TokenType::Divide:
        result = "/";
        break;
    case TokenType::Modulo:
        result = "%";
        break;
    case TokenType::Eq:
        result = "==";
        break;
    case TokenType::Ne:
        result = "!=";
        break;
    case TokenType::Lt:
        result = "<";
        break;
    case TokenType::Le:
        result = "<=";
        break;
    case TokenType::Gt:
        result = ">";
        break;
    case TokenType::Ge:
        result = ">=";
        break;
    case TokenType::And:
        result = "&&";
        break;
    case TokenType::Or:
        result = "||";
        break;
    case TokenType::Eof:
        result = "<eof>";
        break;
    }

    return result;
}
