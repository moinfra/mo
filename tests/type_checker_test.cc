#include <gtest/gtest.h>
#include <algorithm>
#include "src/type_checker.h"

class TypeCheckerTest : public ::testing::Test, public TypeChecker
{
public:
    TypeCheckerTest() : TypeChecker() {}

protected:
    void SetUp() override
    {
        p = new ast::Program();
        set_program(p);
    }

    void TearDown() override
    {
        delete p;
    }

    ast::Program* p;
};

bool has_error(const std::vector<std::string> &errors, const std::string &substr)
{
    return std::any_of(errors.begin(), errors.end(),
                       [&](const auto &msg)
                       { return msg.find(substr) != std::string::npos; });
}

void expect_no_error(const TypeChecker::TypeCheckResult &result)
{
    if (!result.ok)
    {
        std::cerr << "Errors:\n";
        for (const auto &error : result.errors)
        {
            std::cerr << error << "\n";
        }
    }
    EXPECT_TRUE(result.ok);
}

TEST_F(TypeCheckerTest, TypesEqual)
{
    ast::Type intType = ast::Type::get_int_type();
    ast::Type floatType = ast::Type::get_float_type();
    EXPECT_TRUE(types_equal(intType, intType));
    EXPECT_FALSE(types_equal(intType, floatType));
}

TEST_F(TypeCheckerTest, IsConvertible)
{
    ast::Type intType = ast::Type::get_int_type();
    ast::Type floatType = ast::Type::get_float_type();

    EXPECT_TRUE(is_convertible(intType, floatType));
    EXPECT_FALSE(is_convertible(floatType, intType));
}

TEST_F(TypeCheckerTest, ResolveAlias)
{
    ast::TypeAliasDecl alias("MyInt", std::make_unique<ast::Type>(ast::Type::get_int_type()));
    program_->aliases.push_back(std::make_unique<ast::TypeAliasDecl>(std::move(alias)));
    expect_no_error(check());
    auto resolved = resolve_alias("MyInt");
    EXPECT_NE(resolved, nullptr);
    EXPECT_TRUE(types_equal(*resolved, ast::Type::get_int_type()));
}

TEST_F(TypeCheckerTest, FindStruct)
{
    ast::StructDecl structDecl("MyStruct", {});
    program_->structs.push_back(std::make_unique<ast::StructDecl>(std::move(structDecl)));

    EXPECT_NE(find_struct("MyStruct"), nullptr);
    EXPECT_EQ(find_struct("NonExistentStruct"), nullptr);
}

TEST_F(TypeCheckerTest, FindAndInsert)
{
    Scope globalScope(nullptr);
    ast::Type intType = ast::Type::get_int_type();

    EXPECT_TRUE(globalScope.insert("x", std::make_unique<ast::Type>(intType)));
    EXPECT_TRUE(types_equal(*globalScope.find("x"), intType));
    EXPECT_EQ(globalScope.find("y"), nullptr);
}

TEST_F(TypeCheckerTest, NestedScope)
{
    Scope globalScope(nullptr);
    Scope localScope(&globalScope);

    ast::Type intType = ast::Type::get_int_type();
    ast::Type floatType = ast::Type::get_float_type();

    globalScope.insert("x", std::make_unique<ast::Type>(intType));
    localScope.insert("y", std::make_unique<ast::Type>(floatType));

    EXPECT_TRUE(types_equal(*localScope.find("x"), intType));
    EXPECT_TRUE(types_equal(*localScope.find("y"), floatType));
    EXPECT_EQ(localScope.find("z"), nullptr);
}

TEST_F(TypeCheckerTest, PushAndPopScope)
{
    push_scope();
    current_scope_->insert("x", std::make_unique<ast::Type>(ast::Type::get_int_type()));
    EXPECT_NE(current_scope_->find("x"), nullptr);

    pop_scope();
    EXPECT_EQ(current_scope_->find("x"), nullptr);
}

TEST_F(TypeCheckerTest, ValidBreakContinue)
{
    auto while_loop = std::make_unique<ast::WhileStmt>(
        std::make_unique<ast::IntegerLiteralExpr>(1),
        std::make_unique<ast::BlockStmt>());

    auto &body = static_cast<ast::BlockStmt &>(*while_loop->body);
    body.statements.push_back(std::make_unique<ast::BreakStmt>());
    body.statements.push_back(std::make_unique<ast::ContinueStmt>());

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>());
    program_->functions.back()->body.push_back(std::move(while_loop));

    expect_no_error(check());
}

TEST_F(TypeCheckerTest, InvalidBreakOutsideLoop)
{
    auto if_stmt = std::make_unique<ast::IfStmt>(
        std::make_unique<ast::IntegerLiteralExpr>(1),
        std::make_unique<ast::BlockStmt>(),
        nullptr);

    auto &then_block = static_cast<ast::BlockStmt &>(*if_stmt->then_branch);
    then_block.statements.push_back(std::make_unique<ast::BreakStmt>());

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>());
    program_->functions.back()->body.push_back(std::move(if_stmt));

    auto result = check();
    EXPECT_FALSE(result.ok);
    EXPECT_TRUE(has_error(result.errors, "Break statement outside loop context"));
}

TEST_F(TypeCheckerTest, ValidNumericConversion)
{
    auto var_decl = std::make_unique<ast::VarDeclStmt>();
    var_decl->name = "x";
    var_decl->type = ast::Type::get_float_type().clone();
    var_decl->init_expr = std::make_unique<ast::IntegerLiteralExpr>(42);

    program_->globals.push_back(std::make_unique<ast::GlobalDecl>(std::move(*var_decl)));

    expect_no_error(check());
}

TEST_F(TypeCheckerTest, InvalidPointerConversion)
{
    auto cast_expr = std::make_unique<ast::CastExpr>();
    cast_expr->target_type = ast::Type::get_int_type().clone();
    cast_expr->expr = std::make_unique<ast::FloatLiteralExpr>(3.14f);

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>());
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(cast_expr)));

    auto result = check();
    EXPECT_FALSE(result.ok);
    EXPECT_TRUE(has_error(result.errors, "Invalid type conversion"));
}

TEST_F(TypeCheckerTest, UndefinedVariable)
{

    auto struct_decl = std::make_unique<ast::StructDecl>("Point",
                                                         std::vector<ast::TypedField>{
                                                             {ast::Type::get_int_type().clone(), "x"},
                                                             {ast::Type::get_int_type().clone(), "y"}});

    auto var_expr = std::make_unique<ast::VariableExpr>("p");
    auto member_access = std::make_unique<ast::MemberAccessExpr>(
        std::move(var_expr), "x", TokenType::Dot);

    program_->structs.push_back(std::move(struct_decl));
    program_->functions.push_back(std::make_unique<ast::FunctionDecl>());
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(member_access)));

    EXPECT_TRUE(has_error(check().errors, "Undefined variable"));
}

TEST_F(TypeCheckerTest, StructMemberAccess)
{
    auto struct_decl = std::make_unique<ast::StructDecl>("Point",
                                                         std::vector<ast::TypedField>{
                                                             {ast::Type::get_int_type().clone(), "x"},
                                                             {ast::Type::get_int_type().clone(), "y"}});

    auto var_decl = std::make_unique<ast::VarDeclStmt>();
    var_decl->name = "p";
    var_decl->type = std::make_unique<ast::Type>();
    var_decl->type->kind = ast::Type::Kind::Struct;
    var_decl->type->name = "Point";

    auto var_expr = std::make_unique<ast::VariableExpr>("p");
    auto member_access = std::make_unique<ast::MemberAccessExpr>(
        std::move(var_expr), "x", TokenType::Dot);

    program_->structs.push_back(std::move(struct_decl));
    program_->globals.push_back(std::make_unique<ast::GlobalDecl>(std::move(*var_decl)));
    program_->functions.push_back(std::make_unique<ast::FunctionDecl>());
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(member_access)));

    expect_no_error(check());
}

TEST_F(TypeCheckerTest, UndefinedStructAccess)
{
    auto var_expr = std::make_unique<ast::VariableExpr>("p");
    auto member_access = std::make_unique<ast::MemberAccessExpr>(
        std::move(var_expr), "x", TokenType::Dot);

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>());
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(member_access)));

    auto result = check();
    EXPECT_FALSE(result.ok);
    EXPECT_TRUE(has_error(result.errors, "Member access on non-struct type"));
}

TEST_F(TypeCheckerTest, FunctionReturnTypeMismatch)
{
    auto func_decl = std::make_unique<ast::FunctionDecl>();
    func_decl->name = "test";
    func_decl->return_type = ast::Type::get_int_type().clone();
    func_decl->body.push_back(
        std::make_unique<ast::ReturnStmt>(std::make_unique<ast::FloatLiteralExpr>(3.14f)));

    program_->functions.push_back(std::move(func_decl));

    auto result = check();
    EXPECT_FALSE(result.ok);
    EXPECT_TRUE(has_error(result.errors, "Return type mismatch"));
}

TEST_F(TypeCheckerTest, TypeAliasResolution)
{
    program_->aliases.push_back(std::make_unique<ast::TypeAliasDecl>(
        "ID", ast::Type::get_int_type().clone()));

    auto var_decl = std::make_unique<ast::VarDeclStmt>();
    var_decl->name = "user_id";
    var_decl->type = std::make_unique<ast::Type>();
    var_decl->type->kind = ast::Type::Kind::Alias;
    var_decl->type->name = "ID";

    program_->globals.push_back(std::make_unique<ast::GlobalDecl>(std::move(*var_decl)));

    expect_no_error(check());
}
