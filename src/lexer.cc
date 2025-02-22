#include "lexer.h"

Token::Token(Type type, int startLine, int startCol, int endLine, int endCol, const std::string& lexeme)
    : type(type), startLine(startLine), startCol(startCol), endLine(endLine), endCol(endCol), lexeme(lexeme) {}

Lexer::Lexer(const std::string& input) : input(input), pos(0), currentLine(1), currentCol(1) {}

Token Lexer::nextToken() {
    skipWhitespaceAndComments();

    if (pos >= input.size()) {
        return Token(Token::Type::Eof, currentLine, currentCol, currentLine, currentCol, "");
    }

    char c = input[pos];
    if (isdigit(c)) {
        return parseNumber();
    }
    if (c == '"') {
        return parseString();
    }
    if (isalpha(c) || c == '_') {
        return parseIdentifierOrKeyword();
    }

    switch (c) {
        case '-': return parseArrowOrMinus();
        case ':': return parseDoubleColonOrColon();
        case '.': return parseDot();
        case ';': return parseSingleChar(Token::Type::OperatorSemicolon, ";");
        case ',': return parseSingleChar(Token::Type::OperatorComma, ",");
        case '=': return parseEqualOrEq();
        case '!': return parseNe();
        case '<': return parseLeOrLt();
        case '>': return parseGeOrGt();
        case '&': return parseAnd();
        case '|': return parseOr();
        case '(': return parseSingleChar(Token::Type::OperatorLParen, "(");
        case ')': return parseSingleChar(Token::Type::OperatorRParen, ")");
        case '{': return parseSingleChar(Token::Type::OperatorLBrace, "{");
        case '}': return parseSingleChar(Token::Type::OperatorRBrace, "}");
        case '[': return parseSingleChar(Token::Type::OperatorLBracket, "[");
        case ']': return parseSingleChar(Token::Type::OperatorRBracket, "]");
        case '*': return parseSingleChar(Token::Type::OperatorStar, "*");
        case '+': return parseSingleChar(Token::Type::OperatorPlus, "+");
        case '/': return parseSingleChar(Token::Type::OperatorDivide, "/");
        case '%': return parseSingleChar(Token::Type::OperatorModulo, "%");
        default: throw std::runtime_error("Unexpected character: " + std::string(1, c));
    }
}

void Lexer::advance() {
    if (pos >= input.size()) return;
    if (input[pos] == '\n') {
        currentLine++;
        currentCol = 1;
    } else {
        currentCol++;
    }
    pos++;
}

char Lexer::peek(size_t offset) const {
    if (pos + offset >= input.size()) return '\0';
    return input[pos + offset];
}

void Lexer::skipWhitespaceAndComments() {
    while (pos < input.size()) {
        char c = input[pos];
        if (isspace(c)) {
            advance();
        } else if (c == '/') {
            if (peek(1) == '/') {
                skipLineComment();
            } else if (peek(1) == '*') {
                skipBlockComment();
            } else {
                break;
            }
        } else {
            break;
        }
    }
}

void Lexer::skipLineComment() {
    while (pos < input.size() && input[pos] != '\n') {
        advance();
    }
    if (pos < input.size()) advance(); // Skip the newline
}

void Lexer::skipBlockComment() {
    advance(); // Skip '/'
    advance(); // Skip '*'
    while (pos < input.size()) {
        if (input[pos] == '*' && peek(1) == '/') {
            advance(); // Skip '*'
            advance(); // Skip '/'
            return;
        }
        advance();
    }
    throw std::runtime_error("Unclosed block comment");
}

Token Lexer::parseNumber() {
    int startLine = currentLine;
    int startCol = currentCol;
    std::string numStr;
    bool isFloat = false;

    while (pos < input.size() && (isdigit(input[pos]) || input[pos] == '.')) {
        if (input[pos] == '.') {
            if (isFloat) break;
            isFloat = true;
        }
        numStr += input[pos];
        advance();
    }

    Token::Type type = isFloat ? Token::Type::FloatLiteral : Token::Type::IntegerLiteral;
    return Token(type, startLine, startCol, currentLine, currentCol - 1, numStr);
}

Token Lexer::parseString() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip opening "
    std::string str;
    bool escape = false;

    while (pos < input.size()) {
        char c = input[pos];
        if (escape) {
            switch (c) {
                case 'n': str += '\n'; break;
                case 't': str += '\t'; break;
                case 'r': str += '\r'; break;
                case '"': str += '"'; break;
                case '\\': str += '\\'; break;
                default: str += '\\'; str += c; break;
            }
            escape = false;
        } else if (c == '\\') {
            escape = true;
        } else if (c == '"') {
            advance(); // Skip closing "
            return Token(Token::Type::StringLiteral, startLine, startCol, currentLine, currentCol - 1, str);
        } else {
            str += c;
        }
        advance();
    }

    throw std::runtime_error("Unclosed string literal");
}

Token Lexer::parseIdentifierOrKeyword() {
    int startLine = currentLine;
    int startCol = currentCol;
    std::string ident;

    while (pos < input.size() && (isalnum(input[pos]) || input[pos] == '_')) {
        ident += input[pos];
        advance();
    }

    static const std::unordered_map<std::string, Token::Type> keywords = {
        {"let", Token::Type::Let},
        {"struct", Token::Type::Struct},
        {"impl", Token::Type::Impl},
        {"fn", Token::Type::Fn},
        {"return", Token::Type::Return},
        {"int", Token::Type::Int},
        {"float", Token::Type::Float},
        {"const", Token::Type::Const},
        {"sizeof", Token::Type::Sizeof},
        {"cast", Token::Type::Cast},
        {"if", Token::Type::If},
        {"else", Token::Type::Else},
        {"while", Token::Type::While},
        {"for", Token::Type::For},
    };

    auto it = keywords.find(ident);
    Token::Type type = (it != keywords.end()) ? it->second : Token::Type::Identifier;
    return Token(type, startLine, startCol, currentLine, currentCol - 1, ident);
}

Token Lexer::parseArrowOrMinus() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '-'
    if (peek() == '>') {
        advance(); // Skip '>'
        return Token(Token::Type::OperatorArrow, startLine, startCol, currentLine, currentCol - 1, "->");
    }
    return Token(Token::Type::OperatorMinus, startLine, startCol, currentLine, currentCol - 1, "-");
}

Token Lexer::parseDoubleColonOrColon() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip ':'
    if (peek() == ':') {
        advance(); // Skip ':'
        return Token(Token::Type::OperatorDoubleColon, startLine, startCol, currentLine, currentCol - 1, "::");
    }
    return Token(Token::Type::OperatorColon, startLine, startCol, currentLine, currentCol - 1, ":");
}

Token Lexer::parseDot() {
    return parseSingleChar(Token::Type::OperatorDot, ".");
}

Token Lexer::parseEqualOrEq() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '='
    if (peek() == '=') {
        advance(); // Skip '='
        return Token(Token::Type::OperatorEq, startLine, startCol, currentLine, currentCol - 1, "==");
    }
    return Token(Token::Type::OperatorAssign, startLine, startCol, currentLine, currentCol - 1, "=");
}

Token Lexer::parseNe() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '!'
    if (peek() == '=') {
        advance(); // Skip '='
        return Token(Token::Type::OperatorNe, startLine, startCol, currentLine, currentCol - 1, "!=");
    }
    throw std::runtime_error("Unexpected '!'");
}

Token Lexer::parseLeOrLt() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '<'
    if (peek() == '=') {
        advance(); // Skip '='
        return Token(Token::Type::OperatorLe, startLine, startCol, currentLine, currentCol - 1, "<=");
    }
    return Token(Token::Type::OperatorLt, startLine, startCol, currentLine, currentCol - 1, "<");
}

Token Lexer::parseGeOrGt() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '>'
    if (peek() == '=') {
        advance(); // Skip '='
        return Token(Token::Type::OperatorGe, startLine, startCol, currentLine, currentCol - 1, ">=");
    }
    return Token(Token::Type::OperatorGt, startLine, startCol, currentLine, currentCol - 1, ">");
}

Token Lexer::parseAnd() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '&'
    if (peek() == '&') {
        advance(); // Skip '&'
        return Token(Token::Type::OperatorAnd, startLine, startCol, currentLine, currentCol - 1, "&&");
    }
    return Token(Token::Type::OperatorAmpersand, startLine, startCol, currentLine, currentCol - 1, "&");
}

Token Lexer::parseOr() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '|'
    if (peek() == '|') {
        advance(); // Skip '|'
        return Token(Token::Type::OperatorOr, startLine, startCol, currentLine, currentCol - 1, "||");
    }
    throw std::runtime_error("Unexpected '|'");
}

Token Lexer::parseSingleChar(Token::Type type, const std::string& lexeme) {
    int startLine = currentLine;
    int startCol = currentCol;
    advance();
    return Token(type, startLine, startCol, currentLine, currentCol - 1, lexeme);
}
