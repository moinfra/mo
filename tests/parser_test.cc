#include "src/parser.h"
#include "src/lexer.h"
#include <gtest/gtest.h>

TEST(ParserTest, BasicVarDecl) {
    std::string input = "let x: int = 10;";
    Lexer lexer(input);
    Parser parser(std::move(lexer));
    
    auto ast = parser.parse();
    auto varDecl = static_cast<VarDeclStmt*>(ast.globals[0].get());
    
    ASSERT_NE(varDecl, nullptr);
    EXPECT_EQ(varDecl->name, "x");
    // EXPECT_EQ(varDecl->type->toString(), "int");
    // EXPECT_EQ(varDecl->init_expr->toString(), "10");
}
