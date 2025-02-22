#include "gtest/gtest.h"
#include "src/lexer.h"

TEST(LexerTest, TestNumber) {
    Lexer lexer("123 45.67");
    Token token1 = lexer.nextToken();
    EXPECT_EQ(token1.type, Token::Type::IntegerLiteral);
    EXPECT_EQ(token1.lexeme, "123");

    Token token2 = lexer.nextToken();
    EXPECT_EQ(token2.type, Token::Type::FloatLiteral);
    EXPECT_EQ(token2.lexeme, "45.67");
}

TEST(LexerTest, TestString) {
    Lexer lexer("\"hello\"");
    Token token = lexer.nextToken();
    EXPECT_EQ(token.type, Token::Type::StringLiteral);
    EXPECT_EQ(token.lexeme, "hello");
}

TEST(LexerTest, TestKeyword) {
    Lexer lexer("let if else");
    Token token1 = lexer.nextToken();
    EXPECT_EQ(token1.type, Token::Type::Let);
    EXPECT_EQ(token1.lexeme, "let");

    Token token2 = lexer.nextToken();
    EXPECT_EQ(token2.type, Token::Type::If);
    EXPECT_EQ(token2.lexeme, "if");

    Token token3 = lexer.nextToken();
    EXPECT_EQ(token3.type, Token::Type::Else);
    EXPECT_EQ(token3.lexeme, "else");
}

TEST(LexerTest, TestOperator) {
    Lexer lexer("== != ->");
    Token token1 = lexer.nextToken();
    EXPECT_EQ(token1.type, Token::Type::OperatorEq);
    EXPECT_EQ(token1.lexeme, "==");

    Token token2 = lexer.nextToken();
    EXPECT_EQ(token2.type, Token::Type::OperatorNe);
    EXPECT_EQ(token2.lexeme, "!=");

    Token token3 = lexer.nextToken();
    EXPECT_EQ(token3.type, Token::Type::OperatorArrow);
    EXPECT_EQ(token3.lexeme, "->");
}

TEST(LexerTest, TestEOF) {
    Lexer lexer("");
    Token token = lexer.nextToken();
    EXPECT_EQ(token.type, Token::Type::Eof);
}
