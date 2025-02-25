#include "src/parser.h"
#include "src/lexer.h"
#include "src/ast_printer.h"
#include <gtest/gtest.h>
#include <sstream>
#include <algorithm>
#include <cctype>

std::string normalize_whitespace(const std::string& input) {
    auto start = input.find_first_not_of(" \t\n\r");
    auto end = input.find_last_not_of(" \t\n\r");
    if (start == std::string::npos || end == std::string::npos) {
        return "";
    }
    std::string trimmed = input.substr(start, end - start + 1);

    std::stringstream ss;
    bool in_space = false;
    for (char ch : trimmed) {
        if (std::isspace(ch)) {
            if (!in_space) {
                ss << ' ';
                in_space = true;
            }
        } else {
            ss << ch;
            in_space = false;
        }
    }

    return ss.str();
}

std::string parse_and_normalize(const std::string& input) {
    Lexer lexer(input);
    Parser parser(std::move(lexer));
    
    auto ast = parser.parse();
    ASTPrinter printer;
    std::string ast_text = printer.print(ast);
    return normalize_whitespace(ast_text);
}

TEST(ParserTest, BasicVarDecl) {
    std::string input = "let x: int = 10;";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, PointerVarDecl) {
    std::string input = "let ptr: *MyStruct = malloc(sizeof(MyStruct));";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, StaticMethodCall) {
    std::string input = "let v: Vector2 = Vector2::new(1, 2);";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, StructMemberAccess) {
    std::string input = "fn f(v: Vector2) -> void { (v.x = 3); }";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, PointerMemberAccess) {
    std::string input = "fn f(ptr: *MyStruct) -> void { (ptr->x = 3); }";
    EXPECT_EQ(parse_and_normalize(input), input);
}

TEST(ParserTest, HelloWorld) {
    std::string input = "fn main() -> int { print(\"Hello, World!\"); return 0; }";
    EXPECT_EQ(parse_and_normalize(input), input);
}
