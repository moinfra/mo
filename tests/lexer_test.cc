#include "gtest/gtest.h"
#include "src/lexer.h"
#include <sstream>

// Helper function to consume all tokens and convert them to a string
std::string lexer_to_string(Lexer& lexer) {
    std::ostringstream oss;
    while (true) {
        Token token = lexer.next_token();
        oss << token_type_to_string(token.type);
        if (token.type == TokenType::Eof) break;
        oss << " "; // Add a space between tokens
    }
    return oss.str();
}

// Test cases

TEST(LexerTest, TestNumber) {
    Lexer lexer("123 45.67");
    EXPECT_EQ(lexer_to_string(lexer), "<integer> <float> <eof>");
}

TEST(LexerTest, TestString) {
    Lexer lexer("\"hello\"");
    EXPECT_EQ(lexer_to_string(lexer), "<string> <eof>");
}

TEST(LexerTest, TestKeyword) {
    Lexer lexer("let if else while for return");
    EXPECT_EQ(lexer_to_string(lexer), "let if else while for return <eof>");
}

TEST(LexerTest, TestOperators) {
    Lexer lexer("== != -> + - * / %");
    EXPECT_EQ(lexer_to_string(lexer), "== != -> + - * / % <eof>");
}

TEST(LexerTest, TestSymbols) {
    Lexer lexer("() {} [] , ; : :: .");
    EXPECT_EQ(lexer_to_string(lexer), "( ) { } [ ] , ; : :: . <eof>");
}

TEST(LexerTest, TestIdentifiers) {
    Lexer lexer("foo bar_baz qux123");
    EXPECT_EQ(lexer_to_string(lexer), "<id> <id> <id> <eof>");
}

TEST(LexerTest, TestMixed) {
    Lexer lexer("let x = 123 + foo();");
    EXPECT_EQ(lexer_to_string(lexer), "let <id> = <integer> + <id> ( ) ; <eof>");
}

TEST(LexerTest, TestWhitespace) {
    Lexer lexer("  let   x\t=\n123;  ");
    EXPECT_EQ(lexer_to_string(lexer), "let <id> = <integer> ; <eof>");
}

TEST(LexerTest, TestComments) {
    Lexer lexer("let x = 123; // This is a comment\nreturn x;");
    EXPECT_EQ(lexer_to_string(lexer), "let <id> = <integer> ; return <id> ; <eof>");
}

TEST(LexerTest, TestEmptyInput) {
    Lexer lexer("");
    EXPECT_EQ(lexer_to_string(lexer), "<eof>");
}

TEST(LexerTest, TestComplexExpression) {
    Lexer lexer("if (x >= 10) { return x * 2; } else { return x / 2; }");
    EXPECT_EQ(lexer_to_string(lexer), "if ( <id> >= <integer> ) { return <id> * <integer> ; } else { return <id> / <integer> ; } <eof>");
}

TEST(LexerTest, TestStringWithEscapes) {
    Lexer lexer("\"hello\\nworld\"");
    EXPECT_EQ(lexer_to_string(lexer), "<string> <eof>");
}

TEST(LexerTest, TestFloatEdgeCases) {
    Lexer lexer("3.14 .123 123.0 0.0");
    EXPECT_EQ(lexer_to_string(lexer), "<float> <float> <float> <float> <eof>");
}

TEST(LexerTest, TestScientificNotation) {
    Lexer lexer("1e-3 1e+3 1.2e-3 1.2e+3");
    EXPECT_EQ(lexer_to_string(lexer), "<float> <float> <float> <float> <eof>");
}

TEST(LexerTest, TestLogicalOperators) {
    Lexer lexer("true && false || true");
    EXPECT_EQ(lexer_to_string(lexer), "<id> && <id> || <id> <eof>");
}

TEST(LexerTest, TestStructAndImpl) {
    Lexer lexer("struct Foo { x: int; }; impl Foo { fn bar() {} }");
    EXPECT_EQ(lexer_to_string(lexer), "struct <id> { <id> : int ; } ; impl <id> { fn <id> ( ) { } } <eof>");
}
