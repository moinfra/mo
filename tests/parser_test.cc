#include "src/parser.h"
#include "src/lexer.h"
#include "src/ast_printer.h"
#include <gtest/gtest.h>

TEST(ParserTest, BasicVarDecl) {
    std::string input = "let x: int = 10;";
    Lexer lexer(input);
    Parser parser(std::move(lexer));
    
    auto ast = parser.parse();
    ASTPrinter printer;
    std::cout << "errors: " << std::endl;
    for (const auto &error : parser.errors()) {
        std::cout << error << std::endl;
    }

    std::cout << "ast: " << std::endl;
    std::cout << printer.print(ast) << std::endl;
    // EXPECT_EQ(varDecl->type->toString(), "int");
    // EXPECT_EQ(varDecl->init_expr->toString(), "10");
}
