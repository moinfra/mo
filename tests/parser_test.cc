#include "src/parser.h"
#include "src/lexer.h"
#include "src/ast_printer.h"
#include <gtest/gtest.h>

void expect_no_errors(const Parser& parser) {
    const auto& errors = parser.errors();
    if (!errors.empty()) {
        std::cout << "Expected no errors, but got " << errors.size() << " errors: " << std::endl;
        for (const auto& error : errors) {
            std::cout << error << std::endl;
        }
        FAIL();
    }
}

TEST(ParserTest, BasicVarDecl) {
    std::string input = "let x: int = 10;";
    Lexer lexer(input);
    Parser parser(std::move(lexer));
    
    auto ast = parser.parse();
    expect_no_errors(parser);
    ASTPrinter printer;
    std::cout << "ast: " << std::endl;
    auto ast_text = printer.print(ast);
    std::cout << ast_text << std::endl;
    EXPECT_EQ(ast_text, "let x: int = 10;\n");
}

TEST(ParserTest, PointerVarDecl) {
    std::string input = "let ptr: *MyStruct = malloc(sizeof(MyStruct));";
    Lexer lexer(input);
    Parser parser(std::move(lexer));
    
    auto ast = parser.parse();
    expect_no_errors(parser);
    ASTPrinter printer;
    std::string ast_text = printer.print(ast);
    EXPECT_EQ(ast_text, "let ptr: *<type-name> = malloc(sizeof(<type-name>));\n");
}


TEST(ParserTest, StaticMethodCall) {
    std::string input = "let v: Vector2 = Vector2::new(1.0, 2.0);";
    Lexer lexer(input);
    Parser parser(std::move(lexer));
    
    auto ast = parser.parse();
    ASTPrinter printer;
    std::string ast_text = printer.print(ast);
    EXPECT_EQ(ast_text, "let v: <type-name> = Vector2::new(1, 2);\n");
}

TEST(ParserTest, StructMemberAccess) {
    std::string input = "fn f(v: Vector2) { v.x = 3.0; }";
    Lexer lexer(input);
    Parser parser(std::move(lexer));
    
    auto ast = parser.parse();
    ASTPrinter printer;
    std::string ast_text = printer.print(ast);
    EXPECT_EQ(ast_text, "fn f(v: <type-name>) -> void {\n  (v.x = 3);\n}\n\n");
}

TEST(ParserTest, PointerMemberAccess) {
    std::string input = "fn f(ptr: *MyStruct) { ptr->x = 3.0; }";
    Lexer lexer(input);
    Parser parser(std::move(lexer));
    
    auto ast = parser.parse();
    ASTPrinter printer;
    std::string ast_text = printer.print(ast);
    EXPECT_EQ(ast_text, "fn f(ptr: *<type-name>) -> void {\n  (ptr->x = 3);\n}\n\n");
}
