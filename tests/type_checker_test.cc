#include <gtest/gtest.h>
#include <algorithm>
#include "src/type_checker.h"

using namespace ast;

class TypeCheckerTest : public ::testing::Test, public TypeChecker
{
public:
    TypeCheckerTest() : TypeChecker() {}

protected:
    void SetUp() override
    {
        p = new Program();
        set_program(p);
    }

    void TearDown() override
    {
        delete p;
    }

    Program *p;
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
    auto IntegerType = Type::create_int();
    auto floatType = Type::create_float();
    EXPECT_TRUE(types_equal(*IntegerType, *IntegerType));
    EXPECT_FALSE(types_equal(*IntegerType, *floatType));
}

TEST_F(TypeCheckerTest, IsConvertible)
{
    auto IntegerType = Type::create_int();
    auto floatType = Type::create_float();

    EXPECT_TRUE(is_convertible(*IntegerType, *floatType));
    EXPECT_FALSE(is_convertible(*floatType, *IntegerType));
}

TEST_F(TypeCheckerTest, ResolveAlias)
{
    TypeAliasDecl alias("MyInt", (Type::create_int()));
    program_->aliases.push_back(std::make_unique<TypeAliasDecl>(std::move(alias)));
    EXPECT_TRUE(no_error(check()));
    auto resolved = resolve_alias(*Type::create_alias("MyInt"));
    EXPECT_NE(resolved, nullptr);
    EXPECT_TRUE(types_equal(*resolved, *Type::create_int()));
}

TEST_F(TypeCheckerTest, FindStruct)
{
    StructDecl structDecl("MyStruct", {});
    program_->structs.push_back(std::make_unique<StructDecl>(std::move(structDecl)));

    EXPECT_NE(find_struct("MyStruct"), nullptr);
    EXPECT_EQ(find_struct("NonExistentStruct"), nullptr);
}

TEST_F(TypeCheckerTest, FindAndInsert)
{
    Scope globalScope(nullptr);
    auto IntegerType = Type::create_int();

    EXPECT_TRUE(globalScope.insert_variable("x", IntegerType->clone()));
    EXPECT_TRUE(types_equal(*globalScope.resolve_variable("x"), *IntegerType));
    EXPECT_EQ(globalScope.resolve_variable("y"), nullptr);
}

TEST_F(TypeCheckerTest, NestedScope)
{
    Scope globalScope(nullptr);
    Scope localScope(&globalScope);

    auto IntegerType = Type::create_int();
    auto floatType = Type::create_float();

    globalScope.insert_variable("x", IntegerType->clone());
    localScope.insert_variable("y", floatType->clone());

    EXPECT_TRUE(types_equal(*localScope.resolve_variable("x"), *IntegerType));
    EXPECT_TRUE(types_equal(*localScope.resolve_variable("y"), *floatType));
    EXPECT_EQ(localScope.resolve_variable("z"), nullptr);
}

TEST_F(TypeCheckerTest, PushAndPopScope)
{
    push_scope();
    current_scope_->insert_variable("x", (Type::create_int()));
    EXPECT_NE(current_scope_->resolve_variable("x"), nullptr);

    pop_scope();
    EXPECT_EQ(current_scope_->resolve_variable("x"), nullptr);
}

TEST_F(TypeCheckerTest, ValidBreakContinue)
{
    auto while_loop = std::make_unique<WhileStmt>(
        std::make_unique<BooleanLiteralExpr>(true),
        std::make_unique<BlockStmt>());

    auto &body = static_cast<BlockStmt &>(*while_loop->body);
    body.statements.push_back(std::make_unique<BreakStmt>());
    body.statements.push_back(std::make_unique<ContinueStmt>());

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(std::move(while_loop));

    EXPECT_TRUE(no_error(check()));
}

TEST_F(TypeCheckerTest, InvalidBreakOutsideLoop)
{
    auto if_stmt = std::make_unique<IfStmt>(
        std::make_unique<IntegerLiteralExpr>(1),
        std::make_unique<BlockStmt>(),
        nullptr);

    auto &then_block = static_cast<BlockStmt &>(*if_stmt->then_branch);
    then_block.statements.push_back(std::make_unique<BreakStmt>());

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(std::move(if_stmt));

    auto result = check();
    EXPECT_FALSE(result.ok);
    EXPECT_TRUE(has_error(result.errors, "Break statement outside loop context"));
}

TEST_F(TypeCheckerTest, ValidNumericConversion)
{
    auto var_decl = std::make_unique<VarDeclStmt>();
    var_decl->name = "x";
    var_decl->type = Type::create_float();
    var_decl->init_expr = std::make_unique<IntegerLiteralExpr>(42);

    program_->globals.push_back(std::make_unique<GlobalDecl>(std::move(*var_decl)));

    EXPECT_TRUE(no_error(check()));
}

TEST_F(TypeCheckerTest, InvalidPointerConversion)
{
    auto target_type = Type::create_int();
    auto expr = std::make_unique<FloatLiteralExpr>(3.14f);
    auto cast_expr = std::make_unique<CastExpr>(std::move(target_type), std::move(expr));

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(cast_expr)));

    auto result = check();
    EXPECT_FALSE(result.ok);
    EXPECT_TRUE(has_error(result.errors, "Invalid type conversion"));
}

TEST_F(TypeCheckerTest, UndefinedVariable)
{

    auto struct_decl = std::make_unique<StructDecl>("Point",
                                                    std::vector<TypedField>{
                                                        {"x", Type::create_int()},
                                                        {"y", Type::create_int()}});

    auto var_expr = std::make_unique<VariableExpr>("p");
    auto member_access = std::make_unique<MemberAccessExpr>(
        std::move(var_expr), "x", TokenType::Dot);

    program_->structs.push_back(std::move(struct_decl));
    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(member_access)));

    EXPECT_TRUE(has_error(check().errors, "Undefined symbol"));
}

TEST_F(TypeCheckerTest, StructMemberAccess)
{
    /*
        struct Point {
            x: i32,
            y: i32
        }

        var p: Point

        fn main() -> i32 {
            p.x;
        }
    */
    auto struct_decl = std::make_unique<StructDecl>("Point",
                                                    std::vector<TypedField>{
                                                        {"x", Type::create_int()},
                                                        {"y", Type::create_int()}});
    auto struct_type = Type::create_struct(struct_decl->name, struct_decl->fields);

    auto var_decl = std::make_unique<VarDeclStmt>();
    var_decl->name = "p";
    var_decl->type = Type::create_alias("Point");

    auto var_expr = std::make_unique<VariableExpr>("p");
    auto member_access = std::make_unique<MemberAccessExpr>(
        std::move(var_expr), "x", TokenType::Dot);

    program_->structs.push_back(std::move(struct_decl));
    program_->globals.push_back(std::make_unique<GlobalDecl>(std::move(*var_decl)));
    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(member_access)));

    EXPECT_TRUE(no_error(check()));

    EXPECT_TRUE(types_equal(*program_->globals.back()->type, *struct_type));
    auto &member_access_latest = dynamic_cast<ExprStmt &>(
                                     *program_->functions.back()->body.back())
                                     .expr;
    EXPECT_TRUE(types_equal(*member_access_latest->type, *Type::create_int()));
}

TEST_F(TypeCheckerTest, UndefinedStructAccess)
{
    auto var_expr = std::make_unique<VariableExpr>("p");
    auto member_access = std::make_unique<MemberAccessExpr>(
        std::move(var_expr), "x", TokenType::Dot);

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(member_access)));

    auto result = check();
    EXPECT_FALSE(result.ok);
    EXPECT_TRUE(has_error(result.errors, "Member access on non-struct type"));
}

TEST_F(TypeCheckerTest, FunctionReturnTypeMismatch)
{
    auto func_decl = std::make_unique<FunctionDecl>();
    func_decl->name = "test";
    func_decl->return_type = Type::create_int();
    func_decl->body.push_back(
        std::make_unique<ReturnStmt>(std::make_unique<FloatLiteralExpr>(3.14f)));

    program_->functions.push_back(std::move(func_decl));

    auto result = check();
    EXPECT_FALSE(result.ok);
    EXPECT_TRUE(has_error(result.errors, "Return type mismatch"));
}

TEST_F(TypeCheckerTest, TypeAliasResolution)
{
    program_->aliases.push_back(std::make_unique<TypeAliasDecl>(
        "ID", Type::create_int()));

    auto var_decl = std::make_unique<VarDeclStmt>();
    var_decl->name = "user_id";
    var_decl->type = Type::create_alias("ID");

    program_->globals.push_back(std::make_unique<GlobalDecl>(std::move(*var_decl)));

    EXPECT_TRUE(no_error(check()));
}
TEST_F(TypeCheckerTest, VariableExprTypeInference)
{

    auto var_decl = std::make_unique<VarDeclStmt>();
    var_decl->name = "x";
    var_decl->type = Type::create_int();
    program_->globals.push_back(std::make_unique<GlobalDecl>(std::move(*var_decl)));

    auto var_expr = std::make_unique<VariableExpr>("x");

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(var_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, *Type::create_int()));
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::LValue);
}

TEST_F(TypeCheckerTest, IntegerLiteralExprTypeInference)
{

    auto int_literal = std::make_unique<IntegerLiteralExpr>(42);

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(int_literal)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, *Type::create_int()));
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, FloatLiteralExprTypeInference)
{

    auto float_literal = std::make_unique<FloatLiteralExpr>(3.14f);

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(float_literal)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, *Type::create_float()));
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, StringLiteralExprTypeInference)
{

    auto string_literal = std::make_unique<StringLiteralExpr>("hello");

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(string_literal)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, *Type::create_string()));
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, BinaryExprTypeInference)
{

    auto binary_expr = std::make_unique<BinaryExpr>(
        TokenType::Plus,
        std::make_unique<IntegerLiteralExpr>(1),
        std::make_unique<IntegerLiteralExpr>(2));

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(binary_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, *Type::create_int()));
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, UnaryExprTypeInference)
{

    auto var_decl = std::make_unique<VarDeclStmt>();
    var_decl->name = "x";
    var_decl->type = Type::create_int();
    program_->globals.push_back(std::make_unique<GlobalDecl>(std::move(*var_decl)));

    auto var_expr = std::make_unique<VariableExpr>("x");
    auto unary_expr = std::make_unique<UnaryExpr>(
        TokenType::Ampersand, std::move(var_expr));

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(unary_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_EQ(stmt.expr->type->kind(), Type::Kind::Pointer);
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, CallExprTypeInference)
{

    auto func_decl = std::make_unique<FunctionDecl>();
    func_decl->name = "foo";
    func_decl->return_type = Type::create_int();
    func_decl->params.push_back({"x", Type::create_float()});
    program_->functions.push_back(std::move(func_decl));

    std::vector<ExprPtr> args;
    args.push_back(std::make_unique<FloatLiteralExpr>(3.14f));
    auto call_expr = std::make_unique<CallExpr>(
        "foo",
        std::move(args));

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(call_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, *Type::create_int()));
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, MemberAccessExprTypeInference)
{

    auto fields = std::vector<TypedField>{
        {"x", Type::create_int()},
        {"y", Type::create_float()}};

    auto struct_decl = std::make_unique<StructDecl>("Point", fields);
    program_->structs.push_back(std::move(struct_decl));

    auto var_decl = std::make_unique<VarDeclStmt>();
    var_decl->name = "p";
    var_decl->type = (Type::create_struct("Point", fields));
    program_->globals.push_back(std::make_unique<GlobalDecl>(std::move(*var_decl)));

    auto var_expr = std::make_unique<VariableExpr>("p");
    auto member_access = std::make_unique<MemberAccessExpr>(
        std::move(var_expr), "x", TokenType::Dot);

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(member_access)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, *Type::create_int()));
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, ArrayAccessExprTypeInference)
{

    // fn main() {
    //     let arr: int[];
    //     arr[0]; // standalone array access considered as a left value
    // }
    auto var_decl = std::make_unique<VarDeclStmt>();
    var_decl->name = "arr";
    var_decl->type = (Type::create_array(Type::create_int(), -1));
    program_->globals.push_back(std::make_unique<GlobalDecl>(std::move(*var_decl)));

    auto var_expr = std::make_unique<VariableExpr>("arr");
    auto array_access = std::make_unique<ArrayAccessExpr>(
        std::move(var_expr), std::make_unique<IntegerLiteralExpr>(0));

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(array_access)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, *Type::create_int()));
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::LValue);
}

TEST_F(TypeCheckerTest, ArrayAccessAssignment)
{
    // fn main() {
    //     let arr: int[];
    //     arr[0] = 1;
    // }

    {
        auto var_decl = std::make_unique<VarDeclStmt>();
        var_decl->name = "arr";
        var_decl->type = (Type::create_array(Type::create_int(), -1));
        program_->globals.push_back(std::make_unique<GlobalDecl>(std::move(*var_decl)));

        auto var_expr = std::make_unique<VariableExpr>("arr");
        auto array_access = std::make_unique<ArrayAccessExpr>(
            std::move(var_expr), std::make_unique<IntegerLiteralExpr>(0));

        auto assignment_expr = std::make_unique<BinaryExpr>(
            TokenType::Assign, std::move(array_access), std::make_unique<IntegerLiteralExpr>(1));

        program_->functions.push_back(std::make_unique<FunctionDecl>(
            FunctionDecl::create_main_function()));
        program_->functions.back()->body.push_back(
            std::make_unique<ExprStmt>(std::move(assignment_expr)));
    }

    auto result = check();
    EXPECT_TRUE(no_error(result));

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_EQ(stmt.expr->type->kind(), Type::Kind::Int);
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::RValue);

    auto assignment_expr = dynamic_cast<BinaryExpr *>(stmt.expr.get());
    EXPECT_TRUE(assignment_expr);
    auto left = dynamic_cast<ArrayAccessExpr *>(assignment_expr->left.get());
    EXPECT_TRUE(left);
    EXPECT_EQ(left->expr_category, Expr::Category::LValue);
    auto right = dynamic_cast<IntegerLiteralExpr *>(assignment_expr->right.get());
    EXPECT_TRUE(right);
    EXPECT_EQ(right->expr_category, Expr::Category::RValue);
}

TEST_F(TypeCheckerTest, SizeofExprTypeInference)
{

    auto sizeof_expr = std::make_unique<SizeofExpr>(
        (Type::create_int()));

    program_->functions.push_back(std::make_unique<FunctionDecl>(
        FunctionDecl::create_main_function()));
    program_->functions.back()->body.push_back(
        std::make_unique<ExprStmt>(std::move(sizeof_expr)));

    auto result = check();
    no_error(result);

    auto &stmt = static_cast<ExprStmt &>(*program_->functions.back()->body.back());
    EXPECT_TRUE(types_equal(*stmt.expr->type, *Type::create_int()));
    EXPECT_EQ(stmt.expr->expr_category, Expr::Category::RValue);
}
