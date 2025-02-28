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

    ast::Program *p;
};

bool has_error(const std::vector<std::string> &errors, const std::string &substr)
{
    return std::any_of(errors.begin(), errors.end(),
                       [&](const auto &msg)
                       { return msg.find(substr) != std::string::npos; });
}

bool no_error(const TypeChecker::TypeCheckResult &result)
{
    if (!result.ok)
    {
        std::cerr << "Errors:\n";
        auto indent = "  ";
        for (const auto &error : result.errors)
        {
            std::cerr << indent << error << "\n";
        }
    }
    return result.ok;
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
    EXPECT_TRUE(no_error(check()));
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

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(std::move(while_loop));

    EXPECT_TRUE(no_error(check()));
}

TEST_F(TypeCheckerTest, InvalidBreakOutsideLoop)
{
    auto if_stmt = std::make_unique<ast::IfStmt>(
        std::make_unique<ast::IntegerLiteralExpr>(1),
        std::make_unique<ast::BlockStmt>(),
        nullptr);

    auto &then_block = static_cast<ast::BlockStmt &>(*if_stmt->then_branch);
    then_block.statements.push_back(std::make_unique<ast::BreakStmt>());

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
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

    EXPECT_TRUE(no_error(check()));
}

TEST_F(TypeCheckerTest, InvalidPointerConversion)
{
    auto cast_expr = std::make_unique<ast::CastExpr>();
    cast_expr->target_type = ast::Type::get_int_type().clone();
    cast_expr->expr = std::make_unique<ast::FloatLiteralExpr>(3.14f);

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
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
    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(member_access)));

    EXPECT_TRUE(has_error(check().errors, "Undefined symbol"));
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
    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(member_access)));

    EXPECT_TRUE(no_error(check()));
}

TEST_F(TypeCheckerTest, UndefinedStructAccess)
{
    auto var_expr = std::make_unique<ast::VariableExpr>("p");
    auto member_access = std::make_unique<ast::MemberAccessExpr>(
        std::move(var_expr), "x", TokenType::Dot);

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
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

    EXPECT_TRUE(no_error(check()));
}
TEST_F(TypeCheckerTest, VariableExprTypeInference)
{

    auto var_decl = std::make_unique<ast::VarDeclStmt>();
    var_decl->name = "x";
    var_decl->type = ast::Type::get_int_type().clone();
    program_->globals.push_back(std::make_unique<ast::GlobalDecl>(std::move(*var_decl)));

    auto var_expr = std::make_unique<ast::VariableExpr>("x");

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(var_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, ast::Type::get_int_type()));
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::LValue);
}

TEST_F(TypeCheckerTest, IntegerLiteralExprTypeInference)
{

    auto int_literal = std::make_unique<ast::IntegerLiteralExpr>(42);

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(int_literal)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, ast::Type::get_int_type()));
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, FloatLiteralExprTypeInference)
{

    auto float_literal = std::make_unique<ast::FloatLiteralExpr>(3.14f);

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(float_literal)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, ast::Type::get_float_type()));
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, StringLiteralExprTypeInference)
{

    auto string_literal = std::make_unique<ast::StringLiteralExpr>("hello");

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(string_literal)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, ast::Type::get_string_type()));
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, BinaryExprTypeInference)
{

    auto binary_expr = std::make_unique<ast::BinaryExpr>(
        TokenType::Plus,
        std::make_unique<ast::IntegerLiteralExpr>(1),
        std::make_unique<ast::IntegerLiteralExpr>(2));

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(binary_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, ast::Type::get_int_type()));
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, UnaryExprTypeInference)
{

    auto var_decl = std::make_unique<ast::VarDeclStmt>();
    var_decl->name = "x";
    var_decl->type = ast::Type::get_int_type().clone();
    program_->globals.push_back(std::make_unique<ast::GlobalDecl>(std::move(*var_decl)));

    auto var_expr = std::make_unique<ast::VariableExpr>("x");
    auto unary_expr = std::make_unique<ast::UnaryExpr>(
        TokenType::Ampersand, std::move(var_expr));

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(unary_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_EQ(stmt.expr->type->kind, ast::Type::Kind::Pointer);
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, CallExprTypeInference)
{

    auto func_decl = std::make_unique<ast::FunctionDecl>();
    func_decl->name = "foo";
    func_decl->return_type = ast::Type::get_int_type().clone();
    func_decl->params.push_back({ast::Type::get_float_type().clone(), "x"});
    program_->functions.push_back(std::move(func_decl));

    std::vector<ast::ExprPtr> args;
    args.push_back(std::make_unique<ast::FloatLiteralExpr>(3.14f));
    auto call_expr = std::make_unique<ast::CallExpr>(
        "foo",
        std::move(args));

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(call_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, ast::Type::get_int_type()));
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, MemberAccessExprTypeInference)
{

    auto fields = std::vector<ast::TypedField>{
        {ast::Type::get_int_type().clone(), "x"},
        {ast::Type::get_float_type().clone(), "y"}};

    auto struct_decl = std::make_unique<ast::StructDecl>("Point", fields);
    program_->structs.push_back(std::move(struct_decl));

    auto var_decl = std::make_unique<ast::VarDeclStmt>();
    var_decl->name = "p";
    var_decl->type = std::make_unique<ast::Type>(ast::Type::get_struct_type("Point", fields));
    program_->globals.push_back(std::make_unique<ast::GlobalDecl>(std::move(*var_decl)));

    auto var_expr = std::make_unique<ast::VariableExpr>("p");
    auto member_access = std::make_unique<ast::MemberAccessExpr>(
        std::move(var_expr), "x", TokenType::Dot);

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(member_access)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, ast::Type::get_int_type()));
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, ArrayAccessExprTypeInference)
{

    // fn main() {
    //     let arr: int[];
    //     arr[0]; // standalone array access considered as a left value
    // }
    auto var_decl = std::make_unique<ast::VarDeclStmt>();
    var_decl->name = "arr";
    var_decl->type = std::make_unique<ast::Type>(ast::Type::get_array_type(ast::Type::get_int_type().clone(), -1));
    program_->globals.push_back(std::make_unique<ast::GlobalDecl>(std::move(*var_decl)));

    auto var_expr = std::make_unique<ast::VariableExpr>("arr");
    auto array_access = std::make_unique<ast::ArrayAccessExpr>(
        std::move(var_expr), std::make_unique<ast::IntegerLiteralExpr>(0));

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(array_access)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, ast::Type::get_int_type()));
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::LValue);
}

TEST_F(TypeCheckerTest, ArrayAccessAssignment)
{
    // fn main() {
    //     let arr: int[];
    //     arr[0] = 1;
    // }

    {
        auto var_decl = std::make_unique<ast::VarDeclStmt>();
        var_decl->name = "arr";
        var_decl->type = std::make_unique<ast::Type>(ast::Type::get_array_type(ast::Type::get_int_type().clone(), -1));
        program_->globals.push_back(std::make_unique<ast::GlobalDecl>(std::move(*var_decl)));

        auto var_expr = std::make_unique<ast::VariableExpr>("arr");
        auto array_access = std::make_unique<ast::ArrayAccessExpr>(
            std::move(var_expr), std::make_unique<ast::IntegerLiteralExpr>(0));

        auto assignment_expr = std::make_unique<ast::BinaryExpr>(
            TokenType::Assign, std::move(array_access), std::make_unique<ast::IntegerLiteralExpr>(1));

        program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
            ast::FunctionDecl::create_main_function()));
        program_->functions.back()->body.push_back(
            std::make_unique<ast::ExprStmt>(std::move(assignment_expr)));
    }

    auto result = check();
    EXPECT_TRUE(no_error(result));

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_EQ(stmt.expr->type->basic_kind, ast::Type::BasicKind::Int);
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::RValue);

    auto assignment_expr = dynamic_cast<ast::BinaryExpr *>(stmt.expr.get());
    EXPECT_TRUE(assignment_expr);
    auto left = dynamic_cast<ast::ArrayAccessExpr *>(assignment_expr->left.get());
    EXPECT_TRUE(left);
    EXPECT_EQ(left->expr_category, ast::Expr::Category::LValue);
    auto right = dynamic_cast<ast::IntegerLiteralExpr *>(assignment_expr->right.get());
    EXPECT_TRUE(right);
    EXPECT_EQ(right->expr_category, ast::Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, SizeofExprTypeInference)
{

    auto sizeof_expr = std::make_unique<ast::SizeofExpr>(
        std::make_unique<ast::Type>(ast::Type::get_int_type()));

    program_->functions.push_back(std::make_unique<ast::FunctionDecl>(
        ast::FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ast::ExprStmt>(std::move(sizeof_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ast::ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, ast::Type::get_int_type()));
    EXPECT_EQ(stmt.expr->expr_category, ast::Expr::Category::RValue);
}
