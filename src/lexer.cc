#include "lexer.h"

Token::Token(TokenType type, int startLine, int startCol, int endLine, int endCol, const std::string& lexeme)
    : type(type), startLine(startLine), startCol(startCol), endLine(endLine), endCol(endCol), lexeme(lexeme) {}

Lexer::Lexer(const std::string& input) : input(input), pos(0), currentLine(1), currentCol(1) {}

Token Lexer::nextToken() {
    skipWhitespaceAndComments();

    if (pos >= input.size()) {
        return Token(TokenType::Eof, currentLine, currentCol, currentLine, currentCol, "");
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
        case ';': return parseSingleChar(TokenType::OperatorSemicolon, ";");
        case ',': return parseSingleChar(TokenType::OperatorComma, ",");
        case '=': return parseEqualOrEq();
        case '!': return parseNe();
        case '<': return parseLeOrLt();
        case '>': return parseGeOrGt();
        case '&': return parseAnd();
        case '|': return parseOr();
        case '(': return parseSingleChar(TokenType::OperatorLParen, "(");
        case ')': return parseSingleChar(TokenType::OperatorRParen, ")");
        case '{': return parseSingleChar(TokenType::OperatorLBrace, "{");
        case '}': return parseSingleChar(TokenType::OperatorRBrace, "}");
        case '[': return parseSingleChar(TokenType::OperatorLBracket, "[");
        case ']': return parseSingleChar(TokenType::OperatorRBracket, "]");
        case '*': return parseSingleChar(TokenType::OperatorStar, "*");
        case '+': return parseSingleChar(TokenType::OperatorPlus, "+");
        case '/': return parseSingleChar(TokenType::OperatorDivide, "/");
        case '%': return parseSingleChar(TokenType::OperatorModulo, "%");
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

    TokenType type = isFloat ? TokenType::FloatLiteral : TokenType::IntegerLiteral;
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
            return Token(TokenType::StringLiteral, startLine, startCol, currentLine, currentCol - 1, str);
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
    return Token(type, startLine, startCol, currentLine, currentCol - 1, ident);
}

Token Lexer::parseArrowOrMinus() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '-'
    if (peek() == '>') {
        advance(); // Skip '>'
        return Token(TokenType::OperatorArrow, startLine, startCol, currentLine, currentCol - 1, "->");
    }
    return Token(TokenType::OperatorMinus, startLine, startCol, currentLine, currentCol - 1, "-");
}

Token Lexer::parseDoubleColonOrColon() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip ':'
    if (peek() == ':') {
        advance(); // Skip ':'
        return Token(TokenType::OperatorDoubleColon, startLine, startCol, currentLine, currentCol - 1, "::");
    }
    return Token(TokenType::OperatorColon, startLine, startCol, currentLine, currentCol - 1, ":");
}

Token Lexer::parseDot() {
    return parseSingleChar(TokenType::OperatorDot, ".");
}

Token Lexer::parseEqualOrEq() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '='
    if (peek() == '=') {
        advance(); // Skip '='
        return Token(TokenType::OperatorEq, startLine, startCol, currentLine, currentCol - 1, "==");
    }
    return Token(TokenType::OperatorAssign, startLine, startCol, currentLine, currentCol - 1, "=");
}

Token Lexer::parseNe() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '!'
    if (peek() == '=') {
        advance(); // Skip '='
        return Token(TokenType::OperatorNe, startLine, startCol, currentLine, currentCol - 1, "!=");
    }
    throw std::runtime_error("Unexpected '!'");
}

Token Lexer::parseLeOrLt() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '<'
    if (peek() == '=') {
        advance(); // Skip '='
        return Token(TokenType::OperatorLe, startLine, startCol, currentLine, currentCol - 1, "<=");
    }
    return Token(TokenType::OperatorLt, startLine, startCol, currentLine, currentCol - 1, "<");
}

Token Lexer::parseGeOrGt() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '>'
    if (peek() == '=') {
        advance(); // Skip '='
        return Token(TokenType::OperatorGe, startLine, startCol, currentLine, currentCol - 1, ">=");
    }
    return Token(TokenType::OperatorGt, startLine, startCol, currentLine, currentCol - 1, ">");
}

Token Lexer::parseAnd() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '&'
    if (peek() == '&') {
        advance(); // Skip '&'
        return Token(TokenType::OperatorAnd, startLine, startCol, currentLine, currentCol - 1, "&&");
    }
    return Token(TokenType::OperatorAmpersand, startLine, startCol, currentLine, currentCol - 1, "&");
}

Token Lexer::parseOr() {
    int startLine = currentLine;
    int startCol = currentCol;
    advance(); // Skip '|'
    if (peek() == '|') {
        advance(); // Skip '|'
        return Token(TokenType::OperatorOr, startLine, startCol, currentLine, currentCol - 1, "||");
    }
    throw std::runtime_error("Unexpected '|'");
}

Token Lexer::parseSingleChar(TokenType type, const std::string& lexeme) {
    int startLine = currentLine;
    int startCol = currentCol;
    advance();
    return Token(type, startLine, startCol, currentLine, currentCol - 1, lexeme);
}
