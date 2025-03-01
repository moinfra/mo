#include "src/parser.h"
#include "src/lexer.h"
#include "src/ast_printer.h"
#include <gtest/gtest.h>
#include <sstream>
#include <algorithm>
#include <cctype>

std::string normalize_whitespace(const std::string &input)
{
    auto start = input.find_first_not_of(" \t\n\r");
    auto end = input.find_last_not_of(" \t\n\r");
    if (start == std::string::npos || end == std::string::npos)
    {
        return "";
    }
    std::string trimmed = input.substr(start, end - start + 1);

    std::stringstream ss;
    bool in_space = false;
    for (char ch : trimmed)
    {
        if (std::isspace(ch))
        {
            if (!in_space)
            {
                ss << ' ';
                in_space = true;
            }
        }
        else
        {
            ss << ch;
            in_space = false;
        }
    }

    return ss.str();
}

std::string parse_and_normalize(const std::string &input, bool expr_mode = false)
{
    Lexer lexer(input);
    Parser parser(std::move(lexer));

    ASTPrinter printer;
    std::string ast_text;
    if (expr_mode)
    {
        auto ast = parser.parse_expr();
        ast_text = printer.print(*ast);
    }
    else
    {
        auto ast = parser.parse();
        ast_text = printer.print(ast);
    }
    return normalize_whitespace(ast_text);
}

TEST(ParserTest, ArithmeticExpression)
{
    std::string input = "1 + 2 * 3";
    std::string expected = "(1 + (2 * 3))";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, ParenthesizedExpression)
{
    std::string input = "(1 + 2) * 3";
    std::string expected = "((1 + 2) * 3)";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, LogicalExpression)
{
    std::string input = "true && false || true";
    std::string expected = "((true && false) || true)";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, RelationalExpression)
{
    std::string input = "x > y && y < z";
    std::string expected = "((x > y) && (y < z))";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, FunctionCallExpression)
{
    std::string input = "foo(1, 2, 3)";
    std::string expected = "(foo(1, 2, 3))";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, UnaryExpression)
{
    std::string input = "-a";
    std::string expected = "(-a)";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, NestedArithmeticExpression)
{
    std::string input = "1 + (2 * (3 + 4))";
    std::string expected = "(1 + (2 * (3 + 4)))";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, ComplexLogicalExpression)
{
    std::string input = "(true || false) && !(false && true)";
    std::string expected = "((true || false) && (!(false && true)))";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, MixedExpression)
{
    std::string input = "x + y > z && foo(a, b) < bar(c)";
    std::string expected = "(((x + y) > z) && ((foo(a, b)) < (bar(c))))";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, FunctionCallWithNestedExpressions)
{
    std::string input = "foo(1 + 2, bar(3 * 4), baz())";
    std::string expected = "(foo((1 + 2), (bar((3 * 4))), (baz())))";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, SingleVariable)
{
    std::string input = "x";
    std::string expected = "x";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, SingleNumber)
{
    std::string input = "42";
    std::string expected = "42";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, ChainedRelationalExpression)
{
    std::string input = "a < b < c";
    std::string expected = "((a < b) < c)";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, LogicalExpressionWithParentheses)
{
    std::string input = "((true && false) || true)";
    std::string expected = "((true && false) || true)";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, ArithmeticExpressionWithDivision)
{
    std::string input = "10 / 2 + 3";
    std::string expected = "((10 / 2) + 3)";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}
TEST(ParserTest, LongExpression)
{
    std::string input = "a + b * c - d / e && f || g > h";
    std::string expected = "((((a + (b * c)) - (d / e)) && f) || (g > h))";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

// TEST(ParserTest, TernaryExpression)
// {
//     std::string input = "a ? b : c";
//     std::string expected = "(a ? b : c)";
//     EXPECT_EQ(parse_and_normalize(input, true), expected);
// }

// TEST(ParserTest, NestedTernaryExpression)
// {
//     std::string input = "a ? b : c ? d : e";
//     std::string expected = "(a ? b : (c ? d : e))";
//     EXPECT_EQ(parse_and_normalize(input, true), expected);
// }

TEST(ParserTest, ArithmeticAndLogicalPrecedence)
{
    std::string input = "1 + 2 * 3 > 4 && 5 < 6";
    std::string expected = "(((1 + (2 * 3)) > 4) && (5 < 6))";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

// TEST(ParserTest, ConditionalExpression)
// {
//     std::string input = "x > 0 ? x : -x";
//     std::string expected = "((x > 0) ? x : (-x))";
//     EXPECT_EQ(parse_and_normalize(input, true), expected);
// }

TEST(ParserTest, NestedExpression)
{
    std::string input = "(a + b) * (c - d) / e";
    std::string expected = "(((a + b) * (c - d)) / e)";
    EXPECT_EQ(parse_and_normalize(input, true), expected);
}

TEST(ParserTest, BasicVarDecl)
{
    std::string input = "let x: i32 = 10;";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, PointerVarDecl)
{
    std::string input = "let ptr: *MyStruct = (malloc(sizeof(MyStruct)));";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, StaticMethodCall)
{
    std::string input = "let v: Vector2 = (Vector2::new(1, 2));";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, StructMemberAccess)
{
    std::string input = "fn f(v: Vector2) -> void { (v.x = 3); }";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, PointerMemberAccess)
{
    std::string input = "fn f(ptr: *MyStruct) -> void { (ptr->x = 3); }";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, HelloWorld)
{
    std::string input = "fn main() -> i32 { (print(\"Hello, World!\")); return 0; }";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, PointerChainDecl)
{
    std::string input = "let p: **i32 = (malloc(8));";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, ConstDecl)
{
    std::string input = "const MAX: i32 = 100;";

    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, StructDefinition)
{
    std::string input = "struct Point { x: f32, y: f32, }";

    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, MethodImpl)
{
    std::string input = normalize_whitespace(
        "impl Point {"
        "   fn new(x: f32, y: f32) -> Point { }\n"
        "}");

    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, AddressOfExpr)
{
    std::string input = "let p = &(x);";

    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, DereferenceExpr)
{
    std::string input = "let val = *(p);";

    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, PointerArithmetic)
{
    std::string input = "let p2 = (p + 1);";

    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, InitializerList)
{
    std::string input = "let arr: i32[3] = [1, 2, 3];";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, NestedInitializerList)
{
    std::string input = "let matrix: i32[2][2] = [[1, 2], [3, 4]];";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, StructInitializerList)
{
    std::string input = "let p: Point = Point { x: 1.1, y: 2.2 };";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, MixedInitializerList)
{
    std::string input = "let data: Point[2] = [Point { x: 1.1, y: 2.2 }, Point { x: 3.3, y: 4.4 }];";
    EXPECT_EQ(parse_and_normalize(input), input);
}
