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

std::string parse_and_normalize(const std::string &input)
{
    Lexer lexer(input);
    Parser parser(std::move(lexer));

    auto ast = parser.parse();
    ASTPrinter printer;
    std::string ast_text = printer.print(ast);
    return normalize_whitespace(ast_text);
}

TEST(ParserTest, BasicVarDecl)
{
    std::string input = "let x: int = 10;";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, PointerVarDecl)
{
    std::string input = "let ptr: *MyStruct = malloc(sizeof(MyStruct));";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, StaticMethodCall)
{
    std::string input = "let v: Vector2 = Vector2::new(1, 2);";
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
    std::string input = "fn main() -> int { print(\"Hello, World!\"); return 0; }";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, PointerChainDecl)
{
    std::string input = "let p: **int = malloc(8);";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, ConstDecl)
{
    std::string input = "const MAX: int = 100;";

    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, StructDefinition)
{
    std::string input = "struct Point { x: float, y: float, }";

    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, MethodImpl)
{
    std::string input = normalize_whitespace(
        "impl Point {"
        "   fn new(x: float, y: float) -> Point { }\n"
        "}");

    EXPECT_EQ(parse_and_normalize(input), input);
}

// TEST(ParserTest, NestedStruct)
// {
//     std::string input =
//         "struct Outer {"
//         "   inner: struct { a: int },\n"
//         "}";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

TEST(ParserTest, AddressOfExpr)
{
    std::string input = "let p = &x;";

    EXPECT_EQ(parse_and_normalize(input), input);
}

// TEST(ParserTest, DereferenceExpr)
// {
//     std::string input = "let val = *p;";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, PointerArithmetic)
// {
//     std::string input = "let p2 = p + 1;";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, IfElseStmt)
// {
//     std::string input =
//         "if x > 0 {"
//         "   print('pos');"
//         "} else {"
//         "   print('non-pos');"
//         "}";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, WhileLoop)
// {
//     std::string input = "while x < 10 { x += 1; }";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, ReturnWithExpr)
// {
//     std::string input = "fn add() -> int { return a + b; }";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ErrorTest, MissingSemicolon)
// {
//     std::string input = "let x = 10";
//     EXPECT_EQ(parse_and_normalize(input), input);

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ErrorTest, TypeMismatchDecl)
// {
//     std::string input = "let x: int = 3.14;";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ErrorTest, DuplicateStructDef)
// {
//     std::string input =
//         "struct A {}"
//         "struct A {}";
//     EXPECT_EQ(parse_and_normalize(input), input);

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, AutoTypeInference)
// {
//     std::string input = "let v = Vector2 { x: 1.0, y: 2.0 };";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, ExplicitCast)
// {
//     std::string input = "let p = cast(ptr, *mut int);";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, SizeofExpr)
// {
//     std::string input = "let s = sizeof(Point);";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, VariadicFunction)
// {
//     std::string input = "printf('Result: %d', 42);";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, MethodChaining)
// {
//     std::string input = "obj.get_pos()->x = 5;";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }

// TEST(ParserTest, StaticCall)
// {
//     std::string input = "File::open('test.txt');";

//     EXPECT_EQ(parse_and_normalize(input), input);
// }
