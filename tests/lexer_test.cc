#include "gtest/gtest.h"
#include "src/lexer.h"
#include <sstream>

// Helper function to consume all tokens and convert them to a string
std::string lexer_to_string(Lexer &lexer)
{
    std::ostringstream oss;
    while (true)
    {
        Token token = lexer.next_token();
        oss << token_type_to_string(token.type);
        if (token.type == TokenType::Eof)
            break;
        oss << " "; // Add a space between tokens
    }
    return oss.str();
}

bool no_errors(Lexer &lexer)
{
    if (lexer.get_errors().empty())
        return true;
    std::cerr << "Lexer errors:\n";
    for (const auto &error : lexer.get_errors())
    {
        std::cerr << error.message << " at line " << error.line << ", column " << error.col << "\n";
    }
    return false;
}

// Test cases
TEST(LexerTest, TestIntegerLiterals)
{
    Lexer lexer("123 0x1A 0b1010");
    EXPECT_EQ(lexer_to_string(lexer), "<integer> <integer> <integer> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestBooleanLiterals)
{
    Lexer lexer("true false");
    EXPECT_EQ(lexer_to_string(lexer), "<boolean> <boolean> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestFloatLiterals)
{
    Lexer lexer("3.14 .123 123.0 0.0");
    EXPECT_EQ(lexer_to_string(lexer), "<float> <float> <float> <float> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestScientificNotation)
{
    Lexer lexer("1e-3 1e+3 1.2e-3 1.2e+3");
    EXPECT_EQ(lexer_to_string(lexer), "<float> <float> <float> <float> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestHexadecimalLiterals)
{
    Lexer lexer("0x123 0xABC 0x1a2B 0x0");
    EXPECT_EQ(lexer_to_string(lexer), "<integer> <integer> <integer> <integer> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestBinaryLiterals)
{
    Lexer lexer("0b1010 0b1101 0b0");
    EXPECT_EQ(lexer_to_string(lexer), "<integer> <integer> <integer> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestInvalidNumberLiterals)
{
    Lexer lexer("0xG 0b2 123abc");
    std::string result = lexer_to_string(lexer);
    const auto &errors = lexer.get_errors();
    EXPECT_FALSE(errors.empty());
    EXPECT_EQ(errors.size(), 3);
}

TEST(LexerTest, TestStringLiterals)
{
    Lexer lexer("\"hello\" \"world\" \"escaped\\nstring\"");
    EXPECT_EQ(lexer_to_string(lexer), "<string> <string> <string> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestStringEscapes)
{
    Lexer lexer("\"hello\\nworld\\t\\\"\"");
    EXPECT_EQ(lexer_to_string(lexer), "<string> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestUnclosedStringLiteral)
{
    Lexer lexer("\"hello");
    std::string result = lexer_to_string(lexer);
    EXPECT_NE(result.find("<invalid>"), std::string::npos);
    const auto &errors = lexer.get_errors();
    EXPECT_FALSE(errors.empty());
    EXPECT_EQ(errors.size(), 1);
    EXPECT_EQ(errors[0].message, "Unclosed string literal");
}

TEST(LexerTest, TestKeywords)
{
    Lexer lexer("let if else while for return struct impl fn this type const sizeof cast");
    EXPECT_EQ(lexer_to_string(lexer), "let if else while for return struct impl fn this type const sizeof cast <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestIdentifiers)
{
    Lexer lexer("foo bar_baz qux123 _underscore");
    EXPECT_EQ(lexer_to_string(lexer), "<id> <id> <id> <id> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestOperators)
{
    Lexer lexer("+ - * / % == != < > <= >= && || ! & | ^ ~ << >> ++ --");
    EXPECT_EQ(lexer_to_string(lexer), "+ - * / % == != < > <= >= && || ! & | ^ ~ << >> ++ -- <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestAssignOperators)
{
    Lexer lexer("= += -= *= /= %= &= |= ^= <<= >>=");
    EXPECT_EQ(lexer_to_string(lexer), "= += -= *= /= %= &= |= ^= <<= >>= <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestSymbols)
{
    Lexer lexer("() {} [] , ; : :: . ->");
    EXPECT_EQ(lexer_to_string(lexer), "( ) { } [ ] , ; : :: . -> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestMixedExpressions)
{
    Lexer lexer("let x = 123 + foo(); if (x >= 10) { return x * 2; } else { return x / 2; }");
    EXPECT_EQ(lexer_to_string(lexer), "let <id> = <integer> + <id> ( ) ; if ( <id> >= <integer> ) { return <id> * <integer> ; } else { return <id> / <integer> ; } <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestWhitespace)
{
    Lexer lexer("  let   x\t=\n123;  ");
    EXPECT_EQ(lexer_to_string(lexer), "let <id> = <integer> ; <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestComments)
{
    Lexer lexer("let x = 123; // This is a comment\nreturn x; /* Block comment */");
    EXPECT_EQ(lexer_to_string(lexer), "let <id> = <integer> ; return <id> ; <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestEmptyInput)
{
    Lexer lexer("");
    EXPECT_EQ(lexer_to_string(lexer), "<eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestComplexExpression)
{
    Lexer lexer("if (x >= 10) { return x * 2; } else { return x / 2; }");
    EXPECT_EQ(lexer_to_string(lexer), "if ( <id> >= <integer> ) { return <id> * <integer> ; } else { return <id> / <integer> ; } <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestStructAndImpl)
{
    Lexer lexer("struct Foo { x: int; }; impl Foo { fn bar() {} }");
    EXPECT_EQ(lexer_to_string(lexer), "struct <id> { <id> : int ; } ; impl <id> { fn <id> ( ) { } } <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

#if defined(MO_UNICODE)

TEST(LexerTest, TestUnicodeIdentifiers)
{
    Lexer lexer("π φ ∑");
    EXPECT_EQ(lexer_to_string(lexer), "<id> <id> <id> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

#endif // MO_UNICODE


TEST(LexerTest, TestInvalidTokens)
{
    Lexer lexer("@ # $");
    std::string result = lexer_to_string(lexer);
    EXPECT_NE(result.find("<invalid>"), std::string::npos);
    const auto &errors = lexer.get_errors();
    EXPECT_FALSE(errors.empty());
    EXPECT_EQ(errors.size(), 3); // 每个无效字符记录一个错误
}

TEST(LexerTest, TestNestedComments)
{
    Lexer lexer("/* Outer comment /* Inner comment */ */");
    EXPECT_EQ(lexer_to_string(lexer), "* / <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestUnclosedBlockComment)
{
    Lexer lexer("/* Unclosed block comment");
    std::string result = lexer_to_string(lexer);
    EXPECT_EQ(result, "<eof>");
    const auto &errors = lexer.get_errors();
    EXPECT_FALSE(errors.empty());
    EXPECT_EQ(errors.size(), 1);
    EXPECT_EQ(errors[0].message, "Unclosed block comment");
}

TEST(LexerTest, TestMultipleLines)
{
    Lexer lexer("let x = 123;\nlet y = 456;\nreturn x + y;");
    EXPECT_EQ(lexer_to_string(lexer), "let <id> = <integer> ; let <id> = <integer> ; return <id> + <id> ; <eof>");
    EXPECT_TRUE(no_errors(lexer));
}

TEST(LexerTest, TestEdgeCases)
{
    Lexer lexer("0 0.0 0x0 0b0");
    EXPECT_EQ(lexer_to_string(lexer), "<integer> <float> <integer> <integer> <eof>");
    EXPECT_TRUE(no_errors(lexer));
}
