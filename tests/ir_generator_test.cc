#include "src/ir_generator.h"
#include "src/ir_printer.h"
#include "src/type_checker.h"
#include "src/ast.h"

#include <gtest/gtest.h>

using FnPtr = std::unique_ptr<ast::FunctionDecl>;

class IrGeneratorTest : public ::testing::Test
{
protected:
    Module module;
    IRGenerator generator{&module};

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

    void generate_simple_program(FnPtr func, bool type_check = true)
    {
        ast::Program program;
        program.functions.push_back(std::move(func));
        generate(program, type_check);
    }

    void generate(ast::Program &program, bool type_check = true)
    {
        if (type_check)
        {
            TypeChecker checker(&program);
            auto ret = checker.check();
            EXPECT_TRUE(no_error(ret));
            MO_DEBUG("Type checking passed\n");
        }
        generator.generate(program);

        IRPrinter printer;
        std::ostringstream os;
        printer.print_module(module, os);
        std::cout << os.str() << std::endl;
    }

    FnPtr create_test_function(ast::TypePtr return_type = nullptr)
    {
        FnPtr fn = std::make_unique<ast::FunctionDecl>();
        fn->name = "test";
        fn->return_type = return_type ? std::move(return_type) : ast::Type::create_int();
        return fn;
    }

    ReturnInst *find_return(Function *func)
    {
        for (auto &bb : *func)
        {
            for (auto &inst : *bb)
            {
                if (auto *ret = dynamic_cast<ReturnInst *>(&inst))
                {
                    return ret;
                }
            }
        }
        return nullptr;
    }
};

//===----------------------------------------------------------------------===//
//                           Literal Expression Tests
//===----------------------------------------------------------------------===//

TEST_F(IrGeneratorTest, IntegerLiteral)
{
    auto fn = create_test_function();
    fn->body.push_back(std::make_unique<ast::ReturnStmt>(
        std::make_unique<ast::IntegerLiteralExpr>(42)));

    generate_simple_program(std::move(fn));

    Function *test_fn = module.get_function("test");
    ASSERT_NE(test_fn, nullptr);

    ReturnInst *ret = find_return(test_fn);
    ASSERT_NE(ret, nullptr);

    auto *constInt = dynamic_cast<ConstantInt *>(ret->value());
    ASSERT_NE(constInt, nullptr);
    EXPECT_EQ(constInt->value(), 42);
}

TEST_F(IrGeneratorTest, BooleanLiteral)
{
    auto fn = create_test_function(ast::Type::create_bool());
    fn->body.push_back(std::make_unique<ast::ReturnStmt>(
        std::make_unique<ast::BooleanLiteralExpr>(true)));

    generate_simple_program(std::move(fn));

    Function *test_fn = module.get_function("test");
    ASSERT_NE(test_fn, nullptr);

    ReturnInst *ret = find_return(test_fn);
    ASSERT_NE(ret, nullptr);

    auto *constInt = dynamic_cast<ConstantInt *>(ret->value());
    ASSERT_NE(constInt, nullptr);
    EXPECT_EQ(constInt->value(), 1);
}

TEST_F(IrGeneratorTest, FloatLiteral)
{
    auto fn = create_test_function(ast::Type::create_float());
    fn->body.push_back(std::make_unique<ast::ReturnStmt>(
        std::make_unique<ast::FloatLiteralExpr>(3.14f)));

    generate_simple_program(std::move(fn));

    Function *test_func = module.get_function("test");
    ASSERT_NE(test_func, nullptr);

    ReturnInst *ret = find_return(test_func);
    ASSERT_NE(ret, nullptr);

    auto *const_fp = dynamic_cast<ConstantFP *>(ret->value());
    ASSERT_NE(const_fp, nullptr);
    EXPECT_FLOAT_EQ(const_fp->value(), 3.14f);
}

TEST_F(IrGeneratorTest, StringLiteral)
{
    /*
        const u8* hello = "hello";

        fn test() -> *u8 {
            return hello;
        }
    */
    auto fn = create_test_function(ast::Type::create_string());
    fn->body.push_back(std::make_unique<ast::ReturnStmt>(
        std::make_unique<ast::StringLiteralExpr>("hello")));

    generate_simple_program(std::move(fn));

    // Verify global string
    bool found_global = false;
    for (const auto &global : module.global_variables())
    {
        auto *initializer = global->initializer();
        EXPECT_NE(initializer, nullptr);
        if (auto *init = dynamic_cast<ConstantString *>(initializer))
        {
            EXPECT_EQ(init->value(), "hello");
            found_global = true;
        }
    }
    EXPECT_TRUE(found_global);

    // Verify return value
    Function *test_fn = module.get_function("test");
    ASSERT_NE(test_fn, nullptr);

    ReturnInst *ret = find_return(test_fn);
    ASSERT_NE(ret, nullptr);

    auto *inst = dynamic_cast<BitCastInst *>(ret->value());
    ASSERT_NE(inst, nullptr);
}

TEST_F(IrGeneratorTest, BoolImplicitConversion)
{
    /*
        fn test() -> i32 {
            let a : i32 = 5;
            if a {
                return 1;
            }
            return 0;
        }
    */
    auto fn = create_test_function();

    // int a = 5;
    auto var_decl = std::make_unique<ast::VarDeclStmt>();
    var_decl->name = "a";
    var_decl->type = ast::Type::create_int();
    var_decl->init_expr = std::make_unique<ast::IntegerLiteralExpr>(5);
    fn->body.push_back(std::move(var_decl));

    // if (a) return 1;
    auto cond = std::make_unique<ast::VariableExpr>("a");
    auto then_block = std::make_unique<ast::BlockStmt>();
    then_block->statements.push_back(std::make_unique<ast::ReturnStmt>(
        std::make_unique<ast::IntegerLiteralExpr>(1)));

    fn->body.push_back(std::make_unique<ast::IfStmt>(
        std::move(cond),
        std::move(then_block),
        nullptr // No else block
        ));

    fn->body.push_back(std::make_unique<ast::ReturnStmt>(
        std::make_unique<ast::IntegerLiteralExpr>(0)));

    generate_simple_program(std::move(fn));

    Function *test_fn = module.get_function("test");
    ASSERT_NE(test_fn, nullptr);

    bool found_icmp = false;
    for (const auto &bb : *test_fn)
    {
        for (const auto &inst : *bb)
        {
            if (auto *icmp = dynamic_cast<const ICmpInst *>(&inst))
            {
                EXPECT_EQ(icmp->predicate(), ICmpInst::NE);
                auto *load = dynamic_cast<const LoadInst *>(icmp->operand(0));
                ASSERT_NE(load, nullptr);
                found_icmp = true;
            }
        }
    }
    EXPECT_TRUE(found_icmp);
}

TEST_F(IrGeneratorTest, BoolLiteral)
{
    /*
        fn test() -> bool {
            return true;
        }
    */
    auto fn = create_test_function(ast::Type::create_bool());
    fn->body.push_back(std::make_unique<ast::ReturnStmt>(
        std::make_unique<ast::BooleanLiteralExpr>(true)));

    generate_simple_program(std::move(fn));

    Function *test_fn = module.get_function("test");
    ASSERT_NE(test_fn, nullptr);

    ReturnInst *ret = find_return(test_fn);
    ASSERT_NE(ret, nullptr);

    auto *const_bool = dynamic_cast<ConstantInt *>(ret->value());
    ASSERT_NE(const_bool, nullptr);
    EXPECT_EQ(const_bool->type(), module.get_integer_type(1));
    EXPECT_EQ(const_bool->value(), 1);
}

TEST_F(IrGeneratorTest, StructLiteral)
{
    /*
        struct Point {
            x: i32,
            y: i32
        }

        fn test() -> Point {
            return Point { x: 5, y: 10 };
        }
    */
    auto point_struct = std::make_unique<ast::StructDecl>();
    point_struct->name = "Point";
    point_struct->add_field(ast::TypedField{"x", ast::Type::create_int()});
    point_struct->add_field(ast::TypedField{"y", ast::Type::create_int()});

    auto struct_literal = std::make_unique<ast::StructLiteralExpr>("Point");
    struct_literal->add_member("x", std::make_unique<ast::IntegerLiteralExpr>(5));
    struct_literal->add_member("y", std::make_unique<ast::IntegerLiteralExpr>(10));
    struct_literal->type = point_struct->type()->clone();

    auto fn = create_test_function(point_struct->type()->clone());
    fn->body.push_back(std::make_unique<ast::ReturnStmt>(std::move(struct_literal)));

    ast::Program program;
    program.structs.push_back(std::move(point_struct));
    program.functions.push_back(std::move(fn));
    generate(program);

    Function *test_fn = module.get_function("test");
    ASSERT_NE(test_fn, nullptr);

    bool found_alloca = false;
    bool found_geps = false;
    bool found_stores = false;
    for (const auto &bb : *test_fn)
    {
        for (const auto &inst : *bb)
        {
            if (auto *alloca = dynamic_cast<const AllocaInst *>(&inst))
            {
                if (alloca->allocated_type()->is_struct())
                {
                    found_alloca = true;
                }
            }
            else if (auto *gep = dynamic_cast<const GetElementPtrInst *>(&inst))
            {
                if (gep->base_pointer()->type()->is_pointer() &&
                    gep->base_pointer()->type()->element_type()->is_struct())
                {
                    found_geps = true;
                }
            }
            else if (auto *store = dynamic_cast<const StoreInst *>(&inst))
            {
                if (store->stored_value()->type()->is_integer())
                {
                    found_stores = true;
                }
            }
        }
    }
    EXPECT_TRUE(found_alloca);
    EXPECT_TRUE(found_geps);
    EXPECT_TRUE(found_stores);
}

// TEST_F(IrGeneratorTest, InitListExpr)
// {
//     /*
//         fn test() -> [3 x i32] {
//             return [1, 2, 3];
//         }
//     */
//     std::vector<ast::ExprPtr> elems;
//     elems.reserve(3);
//     elems.push_back(std::make_unique<ast::IntegerLiteralExpr>(1));
//     elems.push_back(std::make_unique<ast::IntegerLiteralExpr>(2));
//     elems.push_back(std::make_unique<ast::IntegerLiteralExpr>(3));
//     auto init_list = std::make_unique<ast::InitListExpr>(std::move(elems));

//     auto array_type = ast::Type::create_array(ast::Type::create_int(), 3);
//     auto fn = create_test_function(std::move(array_type));
//     fn->body.push_back(std::make_unique<ast::ReturnStmt>(std::move(init_list)));

//     generate_simple_program(std::move(fn));

//     Function *test_fn = module.get_function("test");
//     ASSERT_NE(test_fn, nullptr);

//     bool found_array_alloca = false;
//     bool found_constant_agg = false;
//     for (const auto &bb : *test_fn)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *alloca = dynamic_cast<const AllocaInst *>(&inst))
//             {
//                 if (alloca->allocated_type()->is_array())
//                 {
//                     found_array_alloca = true;
//                 }
//             }
//             if (auto *const_agg = dynamic_cast<const ConstantAggregate *>(&inst))
//             {
//                 if (const_agg->type()->is_array() &&
//                     const_agg->type()->element_type()->is_integer())
//                 {
//                     found_constant_agg = true;
//                     EXPECT_EQ(const_agg->elements().size(), 3);
//                 }
//             }
//         }
//     }
//     EXPECT_TRUE(found_array_alloca);
//     EXPECT_TRUE(found_constant_agg);
// }
// //===----------------------------------------------------------------------===//
// //                         运算符和表达式测试
// //===----------------------------------------------------------------------===//
// //===----------------------------------------------------------------------===//
// //                          Binary Arithmetic Ops Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, BinaryArithmeticOps)
// {
//     /*
//         fn test(a: i32, b: i32) -> i32 {
//             let c = a + b;
//             let d = c - a;
//             let e = d * b;
//             let f = e / d;
//             return f;
//         }
//     */
//     auto fn = create_test_function(ast::Type::create_int());

//     // Parameters
//     fn->add_param("a", ast::Type::create_int());
//     fn->add_param("b", ast::Type::create_int());

//     // Body
//     auto block = std::make_unique<ast::BlockStmt>();

//     // c = a + b
//     auto add_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Plus,
//         std::make_unique<ast::VariableExpr>("a"),
//         std::make_unique<ast::VariableExpr>("b"));
//     auto add_decl = std::make_unique<ast::VarDeclStmt>();
//     add_decl->name = "c";
//     add_decl->type = ast::Type::create_int();
//     add_decl->init_expr = std::move(add_expr);
//     block->statements.push_back(std::move(add_decl));

//     // d = c - a
//     auto sub_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Minus,
//         std::make_unique<ast::VariableExpr>("c"),
//         std::make_unique<ast::VariableExpr>("a"));
//     auto sub_decl = std::make_unique<ast::VarDeclStmt>();
//     sub_decl->name = "d";
//     sub_decl->type = ast::Type::create_int();
//     sub_decl->init_expr = std::move(sub_expr);
//     block->statements.push_back(std::move(sub_decl));

//     // e = d * b
//     auto mul_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Star,
//         std::make_unique<ast::VariableExpr>("d"),
//         std::make_unique<ast::VariableExpr>("b"));
//     auto mul_decl = std::make_unique<ast::VarDeclStmt>();
//     mul_decl->name = "e";
//     mul_decl->type = ast::Type::create_int();
//     mul_decl->init_expr = std::move(mul_expr);
//     block->statements.push_back(std::move(mul_decl));

//     // f = e / d
//     auto div_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Slash,
//         std::make_unique<ast::VariableExpr>("e"),
//         std::make_unique<ast::VariableExpr>("d"));
//     auto div_decl = std::make_unique<ast::VarDeclStmt>();
//     div_decl->name = "f";
//     div_decl->type = ast::Type::create_int();
//     div_decl->init_expr = std::move(div_expr);
//     block->statements.push_back(std::move(div_decl));

//     // Return
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::VariableExpr>("f")));

//     fn->body.push_back(std::move(block));
//     generate_simple_program(std::move(fn));

//     // Verify instructions
//     Function *test_fn = module.get_function("test");
//     ASSERT_NE(test_fn, nullptr);

//     bool found_add = false;
//     bool found_sub = false;
//     bool found_mul = false;
//     bool found_div = false;

//     for (const auto &bb : *test_fn)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *bin = dynamic_cast<const BinaryInst *>(&inst))
//             {
//                 switch (bin->opcode())
//                 {
//                 case Opcode::Add:
//                     found_add = true;
//                     break;
//                 case Opcode::Sub:
//                     found_sub = true;
//                     break;
//                 case Opcode::Mul:
//                     found_mul = true;
//                     break;
//                 case Opcode::SDiv:
//                     found_div = true;
//                     break;
//                 default:
//                     break;
//                 }
//             }
//         }
//     }

//     EXPECT_TRUE(found_add) << "Add instruction not found";
//     EXPECT_TRUE(found_sub) << "Sub instruction not found";
//     EXPECT_TRUE(found_mul) << "Mul instruction not found";
//     EXPECT_TRUE(found_div) << "SDiv instruction not found";
// }

// //===----------------------------------------------------------------------===//
// //                          Comparison Ops Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, ComparisonOps)
// {
//     /*
//         fn test(a: i32, b: f32) -> i1 {
//             let c = (a == 5);
//             let d = (b != 3.14);
//             let e = (a < 10);
//             let f = (b > 2.0);
//             return c && d && e && f;
//         }
//     */
//     auto fn = create_test_function(ast::Type::create_bool());

//     // Parameters
//     fn->add_param("a", ast::Type::create_int());
//     fn->add_param("b", ast::Type::create_float());

//     // Body
//     auto block = std::make_unique<ast::BlockStmt>();

//     // c = (a == 5)
//     auto eq_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Eq,
//         std::make_unique<ast::VariableExpr>("a"),
//         std::make_unique<ast::IntegerLiteralExpr>(5));
//     auto eq_decl = std::make_unique<ast::VarDeclStmt>();
//     eq_decl->name = "c";
//     eq_decl->type = ast::Type::create_bool();
//     eq_decl->init_expr = std::move(eq_expr);
//     block->statements.push_back(std::move(eq_decl));

//     // d = (b != 3.14)
//     auto ne_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Ne,
//         std::make_unique<ast::VariableExpr>("b"),
//         std::make_unique<ast::FloatLiteralExpr>(3.14f));
//     auto ne_decl = std::make_unique<ast::VarDeclStmt>();
//     ne_decl->name = "d";
//     ne_decl->type = ast::Type::create_bool();
//     ne_decl->init_expr = std::move(ne_expr);
//     block->statements.push_back(std::move(ne_decl));

//     // e = (a < 10)
//     auto lt_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Lt,
//         std::make_unique<ast::VariableExpr>("a"),
//         std::make_unique<ast::IntegerLiteralExpr>(10));
//     auto lt_decl = std::make_unique<ast::VarDeclStmt>();
//     lt_decl->name = "e";
//     lt_decl->type = ast::Type::create_bool();
//     lt_decl->init_expr = std::move(lt_expr);
//     block->statements.push_back(std::move(lt_decl));

//     // f = (b > 2.0)
//     auto gt_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Gt,
//         std::make_unique<ast::VariableExpr>("b"),
//         std::make_unique<ast::FloatLiteralExpr>(2.0f));
//     auto gt_decl = std::make_unique<ast::VarDeclStmt>();
//     gt_decl->name = "f";
//     gt_decl->type = ast::Type::create_bool();
//     gt_decl->init_expr = std::move(gt_expr);
//     block->statements.push_back(std::move(gt_decl));

//     // Final return with logical ANDs
//     auto final_and = std::make_unique<ast::BinaryExpr>(
//         TokenType::And,
//         std::make_unique<ast::BinaryExpr>(
//             TokenType::And,
//             std::make_unique<ast::BinaryExpr>(
//                 TokenType::And,
//                 std::make_unique<ast::VariableExpr>("c"),
//                 std::make_unique<ast::VariableExpr>("d")),
//             std::make_unique<ast::VariableExpr>("e")),
//         std::make_unique<ast::VariableExpr>("f"));
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(final_and)));

//     fn->body.push_back(std::move(block));
//     generate_simple_program(std::move(fn));

//     // Verify instructions
//     Function *test_fn = module.get_function("test");
//     ASSERT_NE(test_fn, nullptr);

//     bool found_icmp_eq = false;
//     bool found_fcmp_ne = false;
//     bool found_icmp_slt = false;
//     bool found_fcmp_ogt = false;

//     for (const auto &bb : *test_fn)
//     {
//         for (const auto &inst : *bb)
//         {
//             // Check integer comparisons
//             if (auto *icmp = dynamic_cast<const ICmpInst *>(&inst))
//             {
//                 if (icmp->predicate() == ICmpInst::EQ)
//                 {
//                     found_icmp_eq = true;
//                 }
//                 else if (icmp->predicate() == ICmpInst::SLT)
//                 {
//                     found_icmp_slt = true;
//                 }
//             }
//             // Check float comparisons
//             else if (auto *fcmp = dynamic_cast<const FCmpInst *>(&inst))
//             {
//                 if (fcmp->predicate() == FCmpInst::ONE)
//                 {
//                     found_fcmp_ne = true;
//                 }
//                 else if (fcmp->predicate() == FCmpInst::OGT)
//                 {
//                     found_fcmp_ogt = true;
//                 }
//             }
//         }
//     }

//     EXPECT_TRUE(found_icmp_eq) << "i32 EQ comparison not found";
//     EXPECT_TRUE(found_fcmp_ne) << "f32 NE comparison not found";
//     EXPECT_TRUE(found_icmp_slt) << "i32 SLT comparison not found";
//     EXPECT_TRUE(found_fcmp_ogt) << "f32 OGT comparison not found";
// }

// //===----------------------------------------------------------------------===//
// //                          Logical Operators Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, LogicalOps)
// {
//     /*
//         fn test(a: bool, b: bool) -> bool {
//             return (a && b) || (!a)
//         }
//     */
//     auto fn = create_test_function(ast::Type::create_bool());
//     fn->add_param("a", ast::Type::create_bool());
//     fn->add_param("b", ast::Type::create_bool());

//     // Build logical expression
//     auto logical_or = std::make_unique<ast::BinaryExpr>(
//         TokenType::Or,
//         std::make_unique<ast::BinaryExpr>(
//             TokenType::And,
//             std::make_unique<ast::VariableExpr>("a"),
//             std::make_unique<ast::VariableExpr>("b")),
//         std::make_unique<ast::UnaryExpr>(
//             TokenType::Not,
//             std::make_unique<ast::VariableExpr>("a")));
//     fn->body.push_back(std::make_unique<ast::ReturnStmt>(std::move(logical_or)));

//     generate_simple_program(std::move(fn));

//     // Verify short-circuit logic
//     Function *test_fn = module.get_function("test");
//     ASSERT_NE(test_fn, nullptr);

//     bool found_phi = false;
//     bool has_multiple_blocks = false;
//     int cond_br_count = 0;

//     // Should have at least 3 basic blocks for short-circuit:
//     // entry, and_block, or_block
//     if (test_fn->basic_blocks().size() >= 3)
//     {
//         has_multiple_blocks = true;
//     }

//     for (const auto &bb : *test_fn)
//     {
//         for (const auto &inst : *bb)
//         {
//             // Check for conditional branches
//             if (auto *br = dynamic_cast<const BranchInst *>(&inst))
//             {
//                 if (br->is_conditional())
//                 {
//                     cond_br_count++;
//                 }
//             }
//             // Check final phi node for merged result
//             if (dynamic_cast<const PhiInst *>(&inst))
//             {
//                 found_phi = true;
//             }
//         }
//     }

//     EXPECT_TRUE(has_multiple_blocks) << "Expected multiple basic blocks for short-circuit";
//     EXPECT_GE(cond_br_count, 2) << "Expected at least 2 conditional branches";
//     EXPECT_TRUE(found_phi) << "Phi node for merging logic results not found";
// }

// //===----------------------------------------------------------------------===//
// //                           Unary Operators Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, UnaryOps)
// {
//     /*
//         fn test(a: i32, b: bool, c: i32, d: *i32) -> i32 {
//             let neg = -a;       // Negation
//             let not = !b;        // Logical NOT
//             let bit_not = ~c;    // Bitwise NOT
//             let addr = &neg;     // Address-of
//             let deref = *d;      // Dereference
//             return neg + deref;
//         }
//     */
//     auto fn = create_test_function(ast::Type::create_int());
//     fn->add_param("a", ast::Type::create_int());
//     fn->add_param("b", ast::Type::create_bool());
//     fn->add_param("c", ast::Type::create_int());
//     fn->add_param("d", ast::Type::create_pointer(ast::Type::create_int()));

//     auto block = std::make_unique<ast::BlockStmt>();

//     // -a
//     auto neg_expr = std::make_unique<ast::UnaryExpr>(
//         TokenType::Minus,
//         std::make_unique<ast::VariableExpr>("a"));
//     auto neg_decl = std::make_unique<ast::VarDeclStmt>();
//     neg_decl->name = "neg";
//     neg_decl->type = ast::Type::create_int();
//     neg_decl->init_expr = std::move(neg_expr);
//     block->statements.push_back(std::move(neg_decl));

//     // !b
//     auto not_expr = std::make_unique<ast::UnaryExpr>(
//         TokenType::Not,
//         std::make_unique<ast::VariableExpr>("b"));
//     auto not_decl = std::make_unique<ast::VarDeclStmt>();
//     not_decl->name = "not";
//     not_decl->type = ast::Type::create_bool();
//     not_decl->init_expr = std::move(not_expr);
//     block->statements.push_back(std::move(not_decl));

//     // ~c
//     auto bit_not_expr = std::make_unique<ast::UnaryExpr>(
//         TokenType::Tilde,
//         std::make_unique<ast::VariableExpr>("c"));
//     auto bit_not_decl = std::make_unique<ast::VarDeclStmt>();
//     bit_not_decl->name = "bit_not";
//     bit_not_decl->type = ast::Type::create_int();
//     bit_not_decl->init_expr = std::move(bit_not_expr);
//     block->statements.push_back(std::move(bit_not_decl));

//     // &neg
//     auto addr_expr = std::make_unique<ast::AddressOfExpr>(
//         std::make_unique<ast::VariableExpr>("neg"));
//     auto addr_decl = std::make_unique<ast::VarDeclStmt>();
//     addr_decl->name = "addr";
//     addr_decl->type = ast::Type::create_pointer(ast::Type::create_int());
//     addr_decl->init_expr = std::move(addr_expr);
//     block->statements.push_back(std::move(addr_decl));

//     // *d
//     auto deref_expr = std::make_unique<ast::DerefExpr>(
//         std::make_unique<ast::VariableExpr>("d"));
//     auto deref_decl = std::make_unique<ast::VarDeclStmt>();
//     deref_decl->name = "deref";
//     deref_decl->type = ast::Type::create_int();
//     deref_decl->init_expr = std::move(deref_expr);
//     block->statements.push_back(std::move(deref_decl));

//     // Return sum
//     auto ret_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Plus,
//         std::make_unique<ast::VariableExpr>("neg"),
//         std::make_unique<ast::VariableExpr>("deref"));
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(ret_expr)));

//     fn->body.push_back(std::move(block));
//     generate_simple_program(std::move(fn));

//     // Verify unary operations
//     Function *test_fn = module.get_function("test");
//     ASSERT_NE(test_fn, nullptr);

//     bool found_neg = false;
//     bool found_not = false;
//     bool found_bit_not = false;
//     bool found_addr = false;
//     bool found_deref = false;

//     for (const auto &bb : *test_fn)
//     {
//         for (const auto &inst : *bb)
//         {
//             // Check negation (0 - a)
//             if (auto *bin = dynamic_cast<const BinaryInst *>(&inst))
//             {
//                 if (bin->opcode() == Opcode::Sub)
//                 {
//                     if (auto *lhs = dynamic_cast<ConstantInt *>(bin->get_lhs()))
//                     {
//                         if (lhs->value() == 0)
//                         {
//                             found_neg = true;
//                         }
//                     }
//                 }
//             }
//             // Check logical NOT (icmp ne)
//             else if (auto *icmp = dynamic_cast<const ICmpInst *>(&inst))
//             {
//                 if (icmp->predicate() == ICmpInst::NE)
//                 {
//                     found_not = true;
//                 }
//             }
//             // Check bitwise NOT (xor -1)
//             else if (auto *bin = dynamic_cast<const BinaryInst *>(&inst))
//             {
//                 if (bin->opcode() == Opcode::BitXor)
//                 {
//                     if (auto *rhs = dynamic_cast<ConstantInt *>(bin->get_rhs()))
//                     {
//                         if (rhs->value() == ~0ULL)
//                         {
//                             found_bit_not = true;
//                         }
//                     }
//                 }
//             }
//             // Check address-of (alloca + store)
//             else if (dynamic_cast<const AllocaInst *>(&inst))
//             {
//                 found_addr = true;
//             }
//             // Check dereference (load)
//             else if (dynamic_cast<const LoadInst *>(&inst))
//             {
//                 found_deref = true;
//             }
//         }
//     }

//     EXPECT_TRUE(found_neg) << "Negation operation (-a) not found";
//     EXPECT_TRUE(found_not) << "Logical NOT (!b) not found";
//     EXPECT_TRUE(found_bit_not) << "Bitwise NOT (~c) not found";
//     EXPECT_TRUE(found_addr) << "Address-of operator (&neg) not found";
//     EXPECT_TRUE(found_deref) << "Dereference operator (*d) not found";
// }

// //===----------------------------------------------------------------------===//
// //                          控制流测试
// //===----------------------------------------------------------------------===//
// //===----------------------------------------------------------------------===//
// //                          If-Else Statement Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, IfElseStatement)
// {
//     /*
//         fn test(a: i32) -> i32 {
//             if a > 0 {
//                 return 1;
//             } else if a == 0 {
//                 return 0;
//             } else {
//                 return -1;
//             }
//         }
//     */
//     auto fn = create_test_function(ast::Type::create_int());
//     fn->add_param("a", ast::Type::create_int());

//     // Build nested if-else
//     auto else_block = std::make_unique<ast::BlockStmt>();
//     else_block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::IntegerLiteralExpr>(-1)));

//     auto else_if_cond = std::make_unique<ast::BinaryExpr>(
//         TokenType::Eq,
//         std::make_unique<ast::VariableExpr>("a"),
//         std::make_unique<ast::IntegerLiteralExpr>(0));
//     auto else_if_block = std::make_unique<ast::BlockStmt>();
//     else_if_block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::IntegerLiteralExpr>(0)));

//     auto main_cond = std::make_unique<ast::BinaryExpr>(
//         TokenType::Ge,
//         std::make_unique<ast::VariableExpr>("a"),
//         std::make_unique<ast::IntegerLiteralExpr>(0));
//     auto then_block = std::make_unique<ast::BlockStmt>();
//     then_block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::IntegerLiteralExpr>(1)));

//     auto else_if_stmt = std::make_unique<ast::IfStmt>(
//         std::move(else_if_cond),
//         std::move(else_if_block),
//         std::move(else_block));

//     auto main_if = std::make_unique<ast::IfStmt>(
//         std::move(main_cond),
//         std::move(then_block),
//         std::move(else_if_stmt));

//     fn->body.push_back(std::move(main_if));
//     generate_simple_program(std::move(fn));

//     // Verify control flow
//     Function *test_fn = module.get_function("test");
//     ASSERT_NE(test_fn, nullptr);

//     bool found_phi = false;
//     int cond_br_count = 0;
//     size_t return_count = 0;

//     for (const auto &bb : *test_fn)
//     {
//         // Count terminators
//         if (auto *term = bb->get_terminator())
//         {
//             if (dynamic_cast<ReturnInst *>(term))
//             {
//                 return_count++;
//             }
//             else if (auto *br = dynamic_cast<BranchInst *>(term))
//             {
//                 if (br->is_conditional())
//                 {
//                     cond_br_count++;
//                 }
//             }
//         }

//         // Check for phi nodes
//         for (const auto &inst : *bb)
//         {
//             if (dynamic_cast<const PhiInst *>(&inst))
//             {
//                 found_phi = true;
//             }
//         }
//     }

//     EXPECT_GE(cond_br_count, 2) << "Expected at least 2 conditional branches";
//     EXPECT_EQ(return_count, 3) << "Expected 3 return points";
//     EXPECT_TRUE(found_phi) << "Phi node for value merging not found";
// }

// //===----------------------------------------------------------------------===//
// //                          While Loop Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, WhileLoop)
// {
//     /*
//         fn test(n: i32) -> i32 {
//             let mut i = 0;
//             let mut sum = 0;
//             while i < n {
//                 sum += i;
//                 i += 1;
//             }
//             return sum;
//         }
//     */
//     auto fn = create_test_function(ast::Type::create_int());
//     fn->add_param("n", ast::Type::create_int());

//     auto block = std::make_unique<ast::BlockStmt>();

//     // Variable declarations
//     auto i_decl = std::make_unique<ast::VarDeclStmt>();
//     i_decl->name = "i";
//     i_decl->type = ast::Type::create_int();
//     i_decl->init_expr = std::make_unique<ast::IntegerLiteralExpr>(0);
//     block->statements.push_back(std::move(i_decl));

//     auto sum_decl = std::make_unique<ast::VarDeclStmt>();
//     sum_decl->name = "sum";
//     sum_decl->type = ast::Type::create_int();
//     sum_decl->init_expr = std::make_unique<ast::IntegerLiteralExpr>(0);
//     block->statements.push_back(std::move(sum_decl));

//     // While loop body
//     auto loop_body = std::make_unique<ast::BlockStmt>();

//     // sum += i
//     auto sum_add = std::make_unique<ast::BinaryExpr>(
//         TokenType::AddAssign,
//         std::make_unique<ast::VariableExpr>("sum"),
//         std::make_unique<ast::VariableExpr>("i"));
//     loop_body->statements.push_back(std::make_unique<ast::ExprStmt>(std::move(sum_add)));

//     // i += 1
//     auto i_inc = std::make_unique<ast::BinaryExpr>(
//         TokenType::AddAssign,
//         std::make_unique<ast::VariableExpr>("i"),
//         std::make_unique<ast::IntegerLiteralExpr>(1));
//     loop_body->statements.push_back(std::make_unique<ast::ExprStmt>(std::move(i_inc)));

//     // Condition: i < n
//     auto cond = std::make_unique<ast::BinaryExpr>(
//         TokenType::Lt,
//         std::make_unique<ast::VariableExpr>("i"),
//         std::make_unique<ast::VariableExpr>("n"));

//     auto while_stmt = std::make_unique<ast::WhileStmt>(
//         std::move(cond),
//         std::move(loop_body));
//     block->statements.push_back(std::move(while_stmt));

//     // Final return
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::VariableExpr>("sum")));

//     fn->body.push_back(std::move(block));
//     generate_simple_program(std::move(fn));

//     // Verify loop structure
//     Function *test_fn = module.get_function("test");
//     ASSERT_NE(test_fn, nullptr);

//     bool has_loop_backedge = false;
//     BasicBlock *cond_block = nullptr;
//     BasicBlock *loop_block = nullptr;

//     // Check CFG structure
//     for (const auto &bb : *test_fn)
//     {
//         if (auto *term = bb->get_terminator())
//         {
//             if (auto *br = dynamic_cast<BranchInst *>(term))
//             {
//                 // Check for back edge (loop block -> cond block)
//                 if (br->get_true_successor() == cond_block && loop_block == &*bb)
//                 {
//                     has_loop_backedge = true;
//                 }

//                 // Record condition block
//                 if (br->is_conditional() && !cond_block)
//                 {
//                     cond_block = &*bb;
//                 }
//             }
//         }

//         // Find loop body block
//         if (cond_block && bb.get() != cond_block && !loop_block)
//         {
//             loop_block = &*bb;
//         }
//     }

//     EXPECT_TRUE(cond_block) << "Condition block not found";
//     EXPECT_TRUE(loop_block) << "Loop body block not found";
//     EXPECT_TRUE(has_loop_backedge) << "Missing loop back edge";
// }

// //===----------------------------------------------------------------------===//
// //                          Break/Continue Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, BreakContinue)
// {
//     /*
//         fn test() -> i32 {
//             let mut i = 0;
//             while true {
//                 i += 1;
//                 if i > 5 {
//                     break;
//                 }
//                 if i % 2 == 0 {
//                     continue;
//                 }
//                 // Do something
//             }
//             return i;
//         }
//     */
//     auto fn = create_test_function(ast::Type::create_int());

//     auto block = std::make_unique<ast::BlockStmt>();

//     // let mut i = 0;
//     auto i_decl = std::make_unique<ast::VarDeclStmt>();
//     i_decl->name = "i";
//     i_decl->type = ast::Type::create_int();
//     i_decl->init_expr = std::make_unique<ast::IntegerLiteralExpr>(0);
//     block->statements.push_back(std::move(i_decl));

//     // Loop body
//     auto loop_body = std::make_unique<ast::BlockStmt>();

//     // i += 1
//     auto i_inc = std::make_unique<ast::BinaryExpr>(
//         TokenType::AddAssign,
//         std::make_unique<ast::VariableExpr>("i"),
//         std::make_unique<ast::IntegerLiteralExpr>(1));
//     loop_body->statements.push_back(std::make_unique<ast::ExprStmt>(std::move(i_inc)));

//     // if i > 5 { break }
//     auto break_cond = std::make_unique<ast::BinaryExpr>(
//         TokenType::Ge,
//         std::make_unique<ast::VariableExpr>("i"),
//         std::make_unique<ast::IntegerLiteralExpr>(5));
//     auto break_then = std::make_unique<ast::BlockStmt>();
//     break_then->statements.push_back(std::make_unique<ast::BreakStmt>());
//     auto break_if = std::make_unique<ast::IfStmt>(
//         std::move(break_cond),
//         std::move(break_then),
//         nullptr);
//     loop_body->statements.push_back(std::move(break_if));

//     // if i % 2 == 0 { continue }
//     auto continue_cond = std::make_unique<ast::BinaryExpr>(
//         TokenType::Eq,
//         std::make_unique<ast::BinaryExpr>(
//             TokenType::Modulo,
//             std::make_unique<ast::VariableExpr>("i"),
//             std::make_unique<ast::IntegerLiteralExpr>(2)),
//         std::make_unique<ast::IntegerLiteralExpr>(0));
//     auto continue_then = std::make_unique<ast::BlockStmt>();
//     continue_then->statements.push_back(std::make_unique<ast::ContinueStmt>());
//     auto continue_if = std::make_unique<ast::IfStmt>(
//         std::move(continue_cond),
//         std::move(continue_then),
//         nullptr);
//     loop_body->statements.push_back(std::move(continue_if));

//     // While loop with true condition
//     auto while_stmt = std::make_unique<ast::WhileStmt>(
//         std::make_unique<ast::IntegerLiteralExpr>(1), // true condition
//         std::move(loop_body));
//     block->statements.push_back(std::move(while_stmt));

//     // Final return
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::VariableExpr>("i")));

//     fn->body.push_back(std::move(block));
//     generate_simple_program(std::move(fn));

//     // Verify break/continue targets
//     Function *test_fn = module.get_function("test");
//     ASSERT_NE(test_fn, nullptr);

//     BasicBlock *loop_exit = nullptr;
//     BasicBlock *loop_header = nullptr;
//     bool has_break_jump = false;
//     bool has_continue_jump = false;

//     for (const auto &bb : *test_fn)
//     {
//         if (auto *term = bb->get_terminator())
//         {
//             // Check break (branch to loop exit)
//             if (auto *br = dynamic_cast<BranchInst *>(term))
//             {
//                 if (!br->is_conditional() && br->get_true_successor() == loop_exit)
//                 {
//                     has_break_jump = true;
//                 }
//             }

//             // Check continue (branch to loop header)
//             if (auto *br = dynamic_cast<BranchInst *>(term))
//             {
//                 if (!br->is_conditional() && br->get_true_successor() == loop_header)
//                 {
//                     has_continue_jump = true;
//                 }
//             }

//             // Find loop exit block (contains return)
//             if (dynamic_cast<ReturnInst *>(term))
//             {
//                 loop_exit = &*bb;
//             }

//             // Find loop header (condition block)
//             if (bb.get() != loop_exit && bb->get_terminator() &&
//                 dynamic_cast<BranchInst *>(bb->get_terminator())->is_conditional())
//             {
//                 loop_header = &*bb;
//             }
//         }
//     }

//     EXPECT_TRUE(loop_exit) << "Loop exit block not found";
//     EXPECT_TRUE(loop_header) << "Loop header block not found";
//     EXPECT_TRUE(has_break_jump) << "Break jump to exit not found";
//     EXPECT_TRUE(has_continue_jump) << "Continue jump to header not found";
// }

// //===----------------------------------------------------------------------===//
// //                          函数和调用测试
// //===----------------------------------------------------------------------===//
// //===----------------------------------------------------------------------===//
// //                          Function Call Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, FunctionCall)
// {
//     /*
//         fn add(a: i32, b: i32) -> i32 {
//             return a + b;
//         }

//         fn test() -> i32 {
//             return add(5, 3);
//         }
//     */
//     ast::Program program;

//     // Add function
//     auto add_fn = std::make_unique<ast::FunctionDecl>();
//     add_fn->name = "add";
//     add_fn->return_type = ast::Type::create_int();
//     add_fn->add_param("a", ast::Type::create_int());
//     add_fn->add_param("b", ast::Type::create_int());
//     add_fn->body.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::BinaryExpr>(
//             TokenType::Plus,
//             std::move(std::make_unique<ast::VariableExpr>("a")),
//             std::move(std::make_unique<ast::VariableExpr>("b")))));
//     program.functions.push_back(std::move(add_fn));

//     // Test function
//     auto test_fn = create_test_function(ast::Type::create_int());

//     std::vector<ast::ExprPtr> vec_1;
//     vec_1.reserve(2);
//     vec_1.push_back(std::make_unique<ast::IntegerLiteralExpr>(5));
//     vec_1.push_back(std::make_unique<ast::IntegerLiteralExpr>(3));

//     test_fn->body.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::CallExpr>(
//             "add",
//             std::move(vec_1))));
//     program.functions.push_back(std::move(test_fn));

//     generate(program);

//     // Verify call instruction
//     Function *add_func = module.get_function("add");
//     Function *test_func = module.get_function("test");
//     ASSERT_NE(add_func, nullptr);
//     ASSERT_NE(test_func, nullptr);

//     bool found_call = false;
//     for (const auto &bb : *test_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *call = dynamic_cast<const CallInst *>(&inst))
//             {
//                 EXPECT_EQ(call->called_function(), add_func);
//                 ASSERT_EQ(call->arguments().size(), 2);
//                 EXPECT_EQ(call->arguments()[0], test_func->arg(0));
//                 EXPECT_EQ(call->arguments()[1], test_func->arg(1));
//                 found_call = true;
//             }
//         }
//     }
//     EXPECT_TRUE(found_call) << "Function call instruction not found";
// }

// //===----------------------------------------------------------------------===//
// //                          Method Call Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, MethodCall)
// {
//     /*
//         struct Point {
//             x: i32,
//             y: i32,
//         }

//         fn test() -> i32 {
//             let p = Point { x: 3, y: 5 };
//             return p.sum();
//         }

//         impl Point {
//             fn sum(self) -> i32 {
//                 return self.x + self.y;
//             }
//         }
//     */
//     ast::Program program;

//     // Define Point struct
//     auto point_struct = std::make_unique<ast::StructDecl>();
//     point_struct->name = "Point";
//     point_struct->add_field({"x", ast::Type::create_int()});
//     point_struct->add_field({"y", ast::Type::create_int()});
//     auto struct_type = point_struct->type();

//     // sum() method
//     auto sum_method = std::make_unique<ast::FunctionDecl>();
//     sum_method->name = "sum";
//     sum_method->return_type = ast::Type::create_int();
//     sum_method->is_method = true;
//     sum_method->receiver_type = ast::Type::create_alias("Point");
//     sum_method->body.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::BinaryExpr>(
//             TokenType::Plus,
//             std::make_unique<ast::MemberAccessExpr>(
//                 std::make_unique<ast::VariableExpr>("self"),
//                 "x",
//                 TokenType::Dot),
//             std::make_unique<ast::MemberAccessExpr>(
//                 std::make_unique<ast::VariableExpr>("self"),
//                 "y",
//                 TokenType::Dot))));
//     point_struct->add_method(std::move(*sum_method));
//     program.structs.push_back(std::move(point_struct));

//     // Test function
//     auto test_fn = create_test_function(ast::Type::create_int());
//     auto p_decl = std::make_unique<ast::VarDeclStmt>();
//     p_decl->name = "p";
//     p_decl->type = ast::Type::create_alias("Point");
//     p_decl->init_expr = std::make_unique<ast::StructLiteralExpr>("Point");
//     auto *struct_lit = static_cast<ast::StructLiteralExpr *>(p_decl->init_expr.get());
//     struct_lit->add_member("x", std::make_unique<ast::IntegerLiteralExpr>(3));
//     struct_lit->add_member("y", std::make_unique<ast::IntegerLiteralExpr>(5));
//     struct_lit->type = ast::Type::create_alias("Point");

//     auto block = std::make_unique<ast::BlockStmt>();
//     block->statements.push_back(std::move(p_decl));
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::MemberAccessExpr>(
//             std::make_unique<ast::VariableExpr>("p"),
//             "sum",
//             TokenType::Dot)));
//     test_fn->body = std::move(block->statements);
//     program.functions.push_back(std::move(test_fn));

//     generate(program);

//     // Verify method call
//     Function *sum_func = module.get_function("Point.sum");
//     Function *test_func = module.get_function("test");
//     ASSERT_NE(sum_func, nullptr);
//     ASSERT_NE(test_func, nullptr);

//     bool found_method_call = false;
//     for (const auto &bb : *test_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *call = dynamic_cast<const CallInst *>(&inst))
//             {
//                 EXPECT_EQ(call->called_function(), sum_func);
//                 ASSERT_GE(call->arguments().size(), 1);

//                 // Verify 'self' pointer is first argument
//                 if (auto *gep = dynamic_cast<GetElementPtrInst *>(call->arguments()[0]))
//                 {
//                     EXPECT_TRUE(gep->base_pointer()->name().find("%p") != std::string::npos);
//                     found_method_call = true;
//                 }
//             }
//         }
//     }
//     EXPECT_TRUE(found_method_call) << "Method call instruction not found";
// }

// //===----------------------------------------------------------------------===//
// //                          Recursive Call Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, RecursiveCall)
// {
//     /*
//         fn factorial(n: i32) -> i32 {
//             if n <= 1 {
//                 return 1;
//             }
//             return n * factorial(n - 1);
//         }
//     */
//     ast::Program program;

//     auto fact_fn = std::make_unique<ast::FunctionDecl>();
//     fact_fn->name = "factorial";
//     fact_fn->return_type = ast::Type::create_int();
//     fact_fn->add_param("n", ast::Type::create_int());

//     // if n <= 1
//     auto cond = std::make_unique<ast::BinaryExpr>(
//         TokenType::Le,
//         std::make_unique<ast::VariableExpr>("n"),
//         std::make_unique<ast::IntegerLiteralExpr>(1));

//     // then return 1
//     auto then_block = std::make_unique<ast::BlockStmt>();
//     then_block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::IntegerLiteralExpr>(1)));

//     // else return n * factorial(n-1)
//     auto else_block = std::make_unique<ast::BlockStmt>();

//     std::vector<ast::ExprPtr> vec_2;
//     vec_2.push_back(std::make_unique<ast::BinaryExpr>(
//         TokenType::Minus,
//         std::make_unique<ast::VariableExpr>("n"),
//         std::make_unique<ast::IntegerLiteralExpr>(1)));

//     else_block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::BinaryExpr>(
//             TokenType::Star,
//             std::make_unique<ast::VariableExpr>("n"),
//             std::make_unique<ast::CallExpr>(
//                 "factorial",
//                 std::move(vec_2)))));

//     auto if_stmt = std::make_unique<ast::IfStmt>(
//         std::move(cond),
//         std::move(then_block),
//         std::move(else_block));
//     fact_fn->body.push_back(std::move(if_stmt));
//     program.functions.push_back(std::move(fact_fn));

//     generate(program);

//     // Verify recursive call
//     Function *fact_func = module.get_function("factorial");
//     ASSERT_NE(fact_func, nullptr);

//     bool found_recursive_call = false;
//     for (const auto &bb : *fact_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *call = dynamic_cast<const CallInst *>(&inst))
//             {
//                 if (call->called_function() == fact_func)
//                 {
//                     ASSERT_EQ(call->arguments().size(), 1);

//                     // Verify argument is n-1
//                     if (auto *sub = dynamic_cast<const BinaryInst *>(call->arguments()[0]))
//                     {
//                         EXPECT_EQ(sub->opcode(), Opcode::Sub);
//                         found_recursive_call = true;
//                     }
//                 }
//             }
//         }
//     }
//     EXPECT_TRUE(found_recursive_call) << "Recursive call instruction not found";
// }

// //===----------------------------------------------------------------------===//
// //                      Struct Member Access Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, StructMemberAccess)
// {
//     /*
//         struct Point {
//             x: i32,
//             y: i32
//         }

//         fn test() -> i32 {
//             let p = Point { x: 3, y: 5 };
//             return p.x + p.y;
//         }
//     */
//     ast::Program program;

//     // Define Point struct
//     auto point_struct = std::make_unique<ast::StructDecl>();
//     point_struct->name = "Point";
//     point_struct->add_field({"x", ast::Type::create_int()});
//     point_struct->add_field({"y", ast::Type::create_int()});
//     auto struct_type = point_struct->type();

//     program.structs.push_back(std::move(point_struct));

//     // Test function
//     auto test_fn = create_test_function(ast::Type::create_int());

//     auto p_decl = std::make_unique<ast::VarDeclStmt>();
//     p_decl->name = "p";
//     p_decl->type = ast::Type::create_alias("Point");
//     auto struct_lit = std::make_unique<ast::StructLiteralExpr>("Point");
//     struct_lit->add_member("x", std::make_unique<ast::IntegerLiteralExpr>(3));
//     struct_lit->add_member("y", std::make_unique<ast::IntegerLiteralExpr>(5));
//     p_decl->init_expr = std::move(struct_lit);

//     auto return_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Plus,
//         std::make_unique<ast::MemberAccessExpr>(
//             std::make_unique<ast::VariableExpr>("p"),
//             "x",
//             TokenType::Dot),
//         std::make_unique<ast::MemberAccessExpr>(
//             std::make_unique<ast::VariableExpr>("p"),
//             "y",
//             TokenType::Dot));

//     auto block = std::make_unique<ast::BlockStmt>();
//     block->statements.push_back(std::move(p_decl));
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(return_expr)));
//     test_fn->body = std::move(block->statements);
//     program.functions.push_back(std::move(test_fn));

//     generate(program);

//     // Verify GEP instructions
//     Function *test_func = module.get_function("test");
//     ASSERT_NE(test_func, nullptr);

//     int gep_count = 0;
//     for (const auto &bb : *test_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *gep = dynamic_cast<const GetElementPtrInst *>(&inst))
//             {
//                 // Verify struct type
//                 if (gep->base_pointer()->type()->element_type()->is_struct())
//                 {
//                     gep_count++;

//                     // Check index for x (0) and y (1)
//                     auto *idx = gep->indices().back();
//                     if (auto *const_idx = dynamic_cast<ConstantInt *>(idx))
//                     {
//                         EXPECT_TRUE(const_idx->value() == 0 || const_idx->value() == 1)
//                             << "Invalid struct member index: " << const_idx->value();
//                     }
//                 }
//             }
//         }
//     }
//     EXPECT_EQ(gep_count, 2) << "Expected 2 GEP instructions for x and y access";
// }

// //===----------------------------------------------------------------------===//
// //                          Array Access Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, ArrayAccess)
// {
//     /*
//         fn test() -> i32 {
//             let arr: [3 x i32] = [1, 2, 3];
//             let idx = 1;
//             return arr[idx] + arr[2];
//         }
//     */
//     ast::Program program;

//     // Test function
//     auto test_fn = create_test_function(ast::Type::create_int());

//     // Array declaration
//     auto arr_decl = std::make_unique<ast::VarDeclStmt>();
//     arr_decl->name = "arr";
//     arr_decl->type = ast::Type::create_array(ast::Type::create_int(), 3);
//     std::vector<ast::ExprPtr> elems;
//     elems.reserve(3);
//     elems.push_back(std::make_unique<ast::IntegerLiteralExpr>(1));
//     elems.push_back(std::make_unique<ast::IntegerLiteralExpr>(2));
//     elems.push_back(std::make_unique<ast::IntegerLiteralExpr>(3));
//     auto init_list = std::make_unique<ast::InitListExpr>(std::move(elems));

//     arr_decl->init_expr = std::move(init_list);

//     // Index variable
//     auto idx_decl = std::make_unique<ast::VarDeclStmt>();
//     idx_decl->name = "idx";
//     idx_decl->type = ast::Type::create_int();
//     idx_decl->init_expr = std::make_unique<ast::IntegerLiteralExpr>(1);

//     // Array accesses
//     auto dynamic_access = std::make_unique<ast::ArrayAccessExpr>(
//         std::make_unique<ast::VariableExpr>("arr"),
//         std::make_unique<ast::VariableExpr>("idx"));
//     auto const_access = std::make_unique<ast::ArrayAccessExpr>(
//         std::make_unique<ast::VariableExpr>("arr"),
//         std::make_unique<ast::IntegerLiteralExpr>(2));

//     auto return_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Plus,
//         std::move(dynamic_access),
//         std::move(const_access));

//     auto block = std::make_unique<ast::BlockStmt>();
//     block->statements.push_back(std::move(arr_decl));
//     block->statements.push_back(std::move(idx_decl));
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(return_expr)));
//     test_fn->body = std::move(block->statements);
//     program.functions.push_back(std::move(test_fn));

//     generate(program);

//     // Verify array access patterns
//     Function *test_func = module.get_function("test");
//     ASSERT_NE(test_func, nullptr);

//     int array_gep_count = 0;
//     int const_index_gep = 0;
//     int dynamic_index_gep = 0;

//     for (const auto &bb : *test_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *gep = dynamic_cast<const GetElementPtrInst *>(&inst))
//             {
//                 if (gep->base_pointer()->type()->element_type()->is_array())
//                 {
//                     array_gep_count++;

//                     // Check array indices
//                     auto indices = gep->indices();
//                     ASSERT_GE(indices.size(), 2) << "Array GEP should have at least 2 indices";

//                     // First index (array pointer)
//                     EXPECT_EQ(dynamic_cast<ConstantInt *>(indices[0])->value(), 0)
//                         << "First array index should be 0";

//                     // Second index (element index)
//                     if (auto *const_idx = dynamic_cast<ConstantInt *>(indices[1]))
//                     {
//                         EXPECT_EQ(const_idx->value(), 2) << "Constant index should be 2";
//                         const_index_gep++;
//                     }
//                     else
//                     {
//                         // Dynamic index (from variable)
//                         dynamic_index_gep++;
//                     }
//                 }
//             }
//         }
//     }

//     EXPECT_EQ(array_gep_count, 2) << "Expected 2 array access GEPs";
//     EXPECT_EQ(const_index_gep, 1) << "Should have 1 constant index access";
//     EXPECT_EQ(dynamic_index_gep, 1) << "Should have 1 dynamic index access";
// }
// //===----------------------------------------------------------------------===//
// //                          类型系统测试
// //===----------------------------------------------------------------------===//
// //===----------------------------------------------------------------------===//
// //                          Explicit Type Cast Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, ExplicitTypeCast)
// {
//     /*
//         fn test(a: i32, b: f32, c: *i32) -> (f32, i32, *u8) {
//             let f = f32(a);     // int to float
//             let i = i32(b);     // float to int
//             let p = *u8(c);     // pointer type cast
//             return (f, i, p);
//         }
//     */
//     ast::Program program;

//     std::vector<ast::TypePtr> elem_types;
//     elem_types.reserve(3);
//     elem_types.push_back(ast::Type::create_float());
//     elem_types.push_back(ast::Type::create_int());
//     elem_types.push_back(ast::Type::create_pointer(ast::Type::create_int(8, true)));
//     auto ret_ty = ast::Type::create_tuple(std::move(elem_types));
//     auto fn = create_test_function(std::move(ret_ty));
//     fn->add_param("a", ast::Type::create_int());
//     fn->add_param("b", ast::Type::create_float());
//     fn->add_param("c", ast::Type::create_pointer(ast::Type::create_int()));

//     auto block = std::make_unique<ast::BlockStmt>();

//     // f32(a)
//     auto cast1 = std::make_unique<ast::CastExpr>(
//         ast::Type::create_float(),
//         std::make_unique<ast::VariableExpr>("a"));
//     auto decl1 = std::make_unique<ast::VarDeclStmt>();
//     decl1->name = "f";
//     // decl1->type = ast::Type::create_float();
//     decl1->init_expr = std::move(cast1);
//     block->statements.push_back(std::move(decl1));

//     // i32(b)
//     auto cast2 = std::make_unique<ast::CastExpr>(
//         ast::Type::create_int(),
//         std::make_unique<ast::VariableExpr>("b"));
//     auto decl2 = std::make_unique<ast::VarDeclStmt>();
//     decl2->name = "i";
//     // decl2->type = ast::Type::create_int();
//     decl2->init_expr = std::move(cast2);
//     block->statements.push_back(std::move(decl2));

//     // *u8(c)
//     auto cast3 = std::make_unique<ast::CastExpr>(
//         ast::Type::create_pointer(ast::Type::create_int(8, true)),
//         std::make_unique<ast::VariableExpr>("c"));
//     auto decl3 = std::make_unique<ast::VarDeclStmt>();
//     decl3->name = "p";
//     // decl3->type = ast::Type::create_pointer(ast::Type::create_int(8, true));
//     decl3->init_expr = std::move(cast3);
//     block->statements.push_back(std::move(decl3));

//     // Return tuple
//     std::vector<ast::ExprPtr> elems;
//     elems.reserve(3);
//     elems.push_back(std::make_unique<ast::VariableExpr>("f"));
//     elems.push_back(std::make_unique<ast::VariableExpr>("i"));
//     elems.push_back(std::make_unique<ast::VariableExpr>("p"));
//     auto ret_expr = std::make_unique<ast::TupleExpr>(std::move(elems));

//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(ret_expr)));

//     fn->body = std::move(block->statements);
//     program.functions.push_back(std::move(fn));
//     generate(program);

//     // Verify cast instructions
//     Function *test_fn = module.get_function("test");
//     ASSERT_NE(test_fn, nullptr);

//     bool found_sitofp = false;
//     bool found_fptosi = false;
//     bool found_bitcast = false;

//     for (const auto &bb : *test_fn)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *cast = dynamic_cast<const SIToFPInst *>(&inst))
//             {
//                 EXPECT_EQ(cast->source()->type(), module.get_integer_type(32));
//                 EXPECT_EQ(cast->type(), module.get_float_type(FloatType::Single));
//                 found_sitofp = true;
//             }
//             else if (auto *cast = dynamic_cast<const FPToSIInst *>(&inst))
//             {
//                 EXPECT_EQ(cast->source()->type(), module.get_float_type(FloatType::Single));
//                 EXPECT_EQ(cast->type(), module.get_integer_type(32));
//                 found_fptosi = true;
//             }
//             else if (auto *cast = dynamic_cast<const BitCastInst *>(&inst))
//             {
//                 EXPECT_TRUE(cast->source()->type()->is_pointer());
//                 EXPECT_TRUE(cast->type()->is_pointer());
//                 found_bitcast = true;
//             }
//         }
//     }

//     EXPECT_TRUE(found_sitofp) << "SIToFP instruction not found";
//     EXPECT_TRUE(found_fptosi) << "FPToSI instruction not found";
//     EXPECT_TRUE(found_bitcast) << "Pointer BitCast instruction not found";
// }

// //===----------------------------------------------------------------------===//
// //                          Sizeof Operator Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, SizeofOperator)
// {
//     /*
//         struct Point { x: f32, y: f32 }

//         fn test() -> (i32, i32) {
//             let s1 = sizeof(Point);
//             let s2 = sizeof(5);
//             return (s1, s2);
//         }
//     */
//     ast::Program program;

//     // Define Point struct
//     auto point_struct = std::make_unique<ast::StructDecl>();
//     point_struct->name = "Point";
//     point_struct->add_field({"x", ast::Type::create_float()});
//     point_struct->add_field({"y", ast::Type::create_float()});
//     program.structs.push_back(std::move(point_struct));

//     // Test function
//     auto elem_types = std::vector<ast::TypePtr>();
//     elem_types.reserve(2);
//     elem_types.push_back(ast::Type::create_int());
//     elem_types.push_back(ast::Type::create_int());
//     auto ret_ty = ast::Type::create_tuple(std::move(elem_types));
//     auto test_fn = create_test_function(std::move(ret_ty));

//     auto block = std::make_unique<ast::BlockStmt>();

//     // sizeof(Point)
//     auto sizeof_type = std::make_unique<ast::SizeofExpr>(ast::Type::create_alias("Point"));
//     auto decl1 = std::make_unique<ast::VarDeclStmt>();
//     decl1->name = "s1";
//     // decl1->type = ast::Type::create_int();
//     decl1->init_expr = std::move(sizeof_type);
//     block->statements.push_back(std::move(decl1));

//     // sizeof(5)
//     auto sizeof_expr = std::make_unique<ast::SizeofExpr>(std::make_unique<ast::IntegerLiteralExpr>(5));
//     auto decl2 = std::make_unique<ast::VarDeclStmt>();
//     decl2->name = "s2";
//     // decl2->type = ast::Type::create_int();
//     decl2->init_expr = std::move(sizeof_expr);
//     block->statements.push_back(std::move(decl2));

//     // Return tuple
//     std::vector<ast::ExprPtr> elems;
//     elems.reserve(2);
//     elems.push_back(std::make_unique<ast::VariableExpr>("s1"));
//     elems.push_back(std::make_unique<ast::VariableExpr>("s2"));
//     auto ret_expr = std::make_unique<ast::TupleExpr>(std::move(elems));
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(ret_expr)));

//     test_fn->body = std::move(block->statements);
//     program.functions.push_back(std::move(test_fn));
//     generate(program);

//     // Verify sizeof constants
//     Function *test_func = module.get_function("test");
//     ASSERT_NE(test_func, nullptr);

//     const int point_size = 8; // 2 x f32
//     const int int_size = 4;

//     bool found_type_size = false;
//     bool found_expr_size = false;

//     for (const auto &bb : *test_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *cnst = dynamic_cast<const ConstantInt *>(&inst))
//             {
//                 if (cnst->value() == point_size)
//                 {
//                     found_type_size = true;
//                 }
//                 else if (cnst->value() == int_size)
//                 {
//                     found_expr_size = true;
//                 }
//             }
//         }
//     }

//     EXPECT_TRUE(found_type_size) << "Struct sizeof constant incorrect";
//     EXPECT_TRUE(found_expr_size) << "Expression sizeof constant incorrect";
// }

// //===----------------------------------------------------------------------===//
// //                          Type Alias Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, TypeAlias)
// {
//     /*
//         type MyInt = i32;
//         type IntPtr = *MyInt;

//         fn test(a: MyInt) -> IntPtr {
//             let b: IntPtr = &a;
//             return b;
//         }
//     */
//     ast::Program program;

//     // Type aliases
//     auto alias1 = std::make_unique<ast::TypeAliasDecl>();
//     alias1->name = "MyInt";
//     alias1->type = ast::Type::create_int();
//     program.aliases.push_back(std::move(alias1));

//     auto alias2 = std::make_unique<ast::TypeAliasDecl>();
//     alias2->name = "IntPtr";
//     alias2->type = ast::Type::create_pointer(ast::Type::create_alias("MyInt"));
//     program.aliases.push_back(std::move(alias2));

//     // Test function
//     auto test_fn = create_test_function(ast::Type::create_alias("IntPtr"));
//     test_fn->add_param("a", ast::Type::create_alias("MyInt"));

//     auto block = std::make_unique<ast::BlockStmt>();

//     // let b: IntPtr = &a
//     auto addr_expr = std::make_unique<ast::AddressOfExpr>(
//         std::make_unique<ast::VariableExpr>("a"));
//     auto decl = std::make_unique<ast::VarDeclStmt>();
//     decl->name = "b";
//     decl->type = ast::Type::create_alias("IntPtr");
//     decl->init_expr = std::move(addr_expr);
//     block->statements.push_back(std::move(decl));

//     // return b
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(
//         std::make_unique<ast::VariableExpr>("b")));

//     test_fn->body = std::move(block->statements);
//     program.functions.push_back(std::move(test_fn));
//     generate(program);

//     // Verify type equivalence
//     Function *test_func = module.get_function("test");
//     ASSERT_NE(test_func, nullptr);

//     // Check parameter type
//     EXPECT_EQ(test_func->arg(0)->type(), module.get_integer_type(32))
//         << "MyInt should resolve to i32";

//     // Check return type
//     Type *ret_type = test_func->return_type();
//     ASSERT_TRUE(ret_type->is_pointer());
//     EXPECT_EQ(ret_type->as_pointer()->element_type(), module.get_integer_type(32))
//         << "IntPtr should resolve to *i32";

//     // Check variable type
//     bool found_alloca = false;
//     for (const auto &bb : *test_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *alloca = dynamic_cast<const AllocaInst *>(&inst))
//             {
//                 if (alloca->allocated_type()->is_pointer() &&
//                     *alloca->allocated_type()->as_pointer()->element_type() == *module.get_integer_type(32))
//                 {
//                     found_alloca = true;
//                 }
//             }
//         }
//     }
//     EXPECT_TRUE(found_alloca) << "Pointer alloca with aliased type not found";
// }

// //===----------------------------------------------------------------------===//
// //                          Memory
// //===----------------------------------------------------------------------===//
// //===----------------------------------------------------------------------===//
// //                          Pointer Operations Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, PointerOperations)
// {
//     /*
//         fn test() -> i32 {
//             let mut data: [3 x i32] = [10, 20, 30];
//             let ptr = &data[0];
//             let p2 = ptr + 1;  // Pointer arithmetic
//             *p2 = 99;         // Store through pointer
//             return *ptr + *p2; // Load through pointers
//         }
//     */
//     ast::Program program;

//     auto test_fn = create_test_function(ast::Type::create_int());
//     auto block = std::make_unique<ast::BlockStmt>();

//     // Array declaration
//     auto arr_decl = std::make_unique<ast::VarDeclStmt>();
//     arr_decl->name = "data";
//     arr_decl->type = ast::Type::create_array(ast::Type::create_int(), 3);
//     std::vector<ast::ExprPtr> elems;
//     elems.reserve(3);
//     elems.emplace_back(std::make_unique<ast::IntegerLiteralExpr>(10));
//     elems.emplace_back(std::make_unique<ast::IntegerLiteralExpr>(20));
//     elems.emplace_back(std::make_unique<ast::IntegerLiteralExpr>(30));
//     auto init_list = std::make_unique<ast::InitListExpr>(std::move(elems));

//     arr_decl->init_expr = std::move(init_list);
//     block->statements.push_back(std::move(arr_decl));

//     // Get address of first element
//     auto ptr_decl = std::make_unique<ast::VarDeclStmt>();
//     ptr_decl->name = "ptr";
//     ptr_decl->type = ast::Type::create_pointer(ast::Type::create_int());
//     ptr_decl->init_expr = std::make_unique<ast::AddressOfExpr>(
//         std::make_unique<ast::ArrayAccessExpr>(
//             std::make_unique<ast::VariableExpr>("data"),
//             std::make_unique<ast::IntegerLiteralExpr>(0)));
//     block->statements.push_back(std::move(ptr_decl));

//     // Pointer arithmetic (ptr + 1)
//     auto p2_decl = std::make_unique<ast::VarDeclStmt>();
//     p2_decl->name = "p2";
//     p2_decl->type = ast::Type::create_pointer(ast::Type::create_int());
//     p2_decl->init_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Plus,
//         std::make_unique<ast::VariableExpr>("ptr"),
//         std::make_unique<ast::IntegerLiteralExpr>(1));
//     block->statements.push_back(std::move(p2_decl));

//     // Store through pointer
//     auto store_stmt = std::make_unique<ast::ExprStmt>(
//         std::make_unique<ast::BinaryExpr>(
//             TokenType::Assign,
//             std::make_unique<ast::DerefExpr>(std::make_unique<ast::VariableExpr>("p2")),
//             std::make_unique<ast::IntegerLiteralExpr>(99)));
//     block->statements.push_back(std::move(store_stmt));

//     // Return sum of dereferenced pointers
//     auto ret_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Plus,
//         std::make_unique<ast::DerefExpr>(std::make_unique<ast::VariableExpr>("ptr")),
//         std::make_unique<ast::DerefExpr>(std::make_unique<ast::VariableExpr>("p2")));
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(ret_expr)));

//     test_fn->body = std::move(block->statements);
//     program.functions.push_back(std::move(test_fn));
//     generate(program);

//     // Verify pointer operations
//     Function *test_func = module.get_function("test");
//     ASSERT_NE(test_func, nullptr);

//     bool found_gep = false;
//     bool found_ptr_store = false;
//     bool found_ptr_load = false;

//     for (const auto &bb : *test_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             // Check GEP for array index and pointer arithmetic
//             if (auto *gep = dynamic_cast<const GetElementPtrInst *>(&inst))
//             {
//                 found_gep = true;
//                 // Verify array index or pointer offset
//                 auto indices = gep->indices();
//                 if (indices.size() > 1 && dynamic_cast<ConstantInt *>(indices[1])->value() == 1)
//                 {
//                     EXPECT_EQ(gep->type()->element_type(), module.get_integer_type(32));
//                 }
//             }
//             // Check store through pointer
//             else if (auto *store = dynamic_cast<const StoreInst *>(&inst))
//             {
//                 if (store->stored_value()->type()->is_integer() &&
//                     dynamic_cast<ConstantInt *>(store->stored_value())->value() == 99)
//                 {
//                     found_ptr_store = true;
//                 }
//             }
//             // Check load through pointers
//             else if (dynamic_cast<const LoadInst *>(&inst))
//             {
//                 found_ptr_load = true;
//             }
//         }
//     }

//     EXPECT_TRUE(found_gep) << "GEP instruction for pointer arithmetic not found";
//     EXPECT_TRUE(found_ptr_store) << "Store through pointer not found";
//     EXPECT_TRUE(found_ptr_load) << "Load through pointers not found";
// }

// //===----------------------------------------------------------------------===//
// //                          Global Variable Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, GlobalVariable_)
// {
//     /*
//         global GLOBAL_VALUE: i32 = 100;

//         fn test() -> i32 {
//             let mut x = GLOBAL_VALUE;
//             GLOBAL_VALUE = 200;
//             return x + GLOBAL_VALUE;
//         }
//     */
//     ast::Program program;

//     // Global variable declaration
//     auto global_decl = std::make_unique<ast::GlobalDecl>();
//     global_decl->name = "GLOBAL_VALUE";
//     global_decl->type = ast::Type::create_int();
//     global_decl->init_expr = std::make_unique<ast::IntegerLiteralExpr>(100);
//     program.globals.push_back(std::move(global_decl));

//     // Test function
//     auto test_fn = create_test_function(ast::Type::create_int());
//     auto block = std::make_unique<ast::BlockStmt>();

//     // Load global
//     auto x_decl = std::make_unique<ast::VarDeclStmt>();
//     x_decl->name = "x";
//     x_decl->type = ast::Type::create_int();
//     x_decl->init_expr = std::make_unique<ast::VariableExpr>("GLOBAL_VALUE");
//     block->statements.push_back(std::move(x_decl));

//     // Store to global
//     auto store_global = std::make_unique<ast::ExprStmt>(
//         std::make_unique<ast::BinaryExpr>(
//             TokenType::Assign,
//             std::make_unique<ast::VariableExpr>("GLOBAL_VALUE"),
//             std::make_unique<ast::IntegerLiteralExpr>(200)));
//     block->statements.push_back(std::move(store_global));

//     // Return sum
//     auto ret_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Plus,
//         std::make_unique<ast::VariableExpr>("x"),
//         std::make_unique<ast::VariableExpr>("GLOBAL_VALUE"));
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(ret_expr)));

//     test_fn->body = std::move(block->statements);
//     program.functions.push_back(std::move(test_fn));
//     generate(program);

//     // Verify global variable handling
//     GlobalVariable *global_var = nullptr;
//     for (const auto &gv : module.global_variables())
//     {
//         if (gv->name() == "GLOBAL_VALUE")
//         {
//             global_var = gv;
//             break;
//         }
//     }
//     ASSERT_NE(global_var, nullptr) << "Global variable not found";
//     EXPECT_EQ(global_var->initializer()->as_string(), "100");

//     Function *test_func = module.get_function("test");
//     ASSERT_NE(test_func, nullptr);

//     bool found_global_load = false;
//     bool found_global_store = false;

//     for (const auto &bb : *test_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             // Check global loads
//             if (auto *load = dynamic_cast<const LoadInst *>(&inst))
//             {
//                 if (load->pointer() == global_var)
//                 {
//                     found_global_load = true;
//                 }
//             }
//             // Check global stores
//             else if (auto *store = dynamic_cast<const StoreInst *>(&inst))
//             {
//                 if (store->pointer() == global_var)
//                 {
//                     found_global_store = true;
//                     EXPECT_EQ(dynamic_cast<ConstantInt *>(store->stored_value())->value(), 200);
//                 }
//             }
//         }
//     }

//     EXPECT_TRUE(found_global_load) << "Global variable load not found";
//     EXPECT_TRUE(found_global_store) << "Global variable store not found";
// }

// //===----------------------------------------------------------------------===//
// //                          边界条件测试
// //===----------------------------------------------------------------------===//
// //===----------------------------------------------------------------------===//
// //                          Empty Struct Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, EmptyStruct)
// {
//     /*
//         struct Empty {}

//         fn test() -> i32 {
//             let e = Empty{};
//             return sizeof(Empty);
//         }
//     */
//     ast::Program program;

//     auto empty_struct = std::make_unique<ast::StructDecl>();
//     empty_struct->name = "Empty";
//     program.structs.push_back(std::move(empty_struct));

//     auto test_fn = create_test_function(ast::Type::create_int());

//     auto var_decl = std::make_unique<ast::VarDeclStmt>();
//     var_decl->name = "e";
//     var_decl->type = ast::Type::create_alias("Empty");
//     var_decl->init_expr = std::make_unique<ast::StructLiteralExpr>("Empty");

//     auto sizeof_expr = std::make_unique<ast::SizeofExpr>(ast::Type::create_alias("Empty"));

//     auto block = std::make_unique<ast::BlockStmt>();
//     block->statements.push_back(std::move(var_decl));
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(sizeof_expr)));
//     test_fn->body = std::move(block->statements);

//     program.functions.push_back(std::move(test_fn));
//     generate(program);

//     // Verify empty struct has 1 byte size (minimum allowed)
//     StructType *empty_type = module.try_get_named_struct_type("Empty");
//     ASSERT_NE(empty_type, nullptr);
//     EXPECT_EQ(empty_type->size(), 1) << "Empty struct should have 1 byte size";
// }

// //===----------------------------------------------------------------------===//
// //                          Zero-Size Array Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, ZeroSizeArray)
// {
//     /*
//         struct Container {
//             data: [0 x i32]
//         }

//         fn test() -> i32 {
//             return sizeof(Container);
//         }
//     */
//     ast::Program program;

//     auto container_struct = std::make_unique<ast::StructDecl>();
//     container_struct->name = "Container";
//     container_struct->add_field({"data", ast::Type::create_array(ast::Type::create_int(), 0)});
//     program.structs.push_back(std::move(container_struct));

//     auto test_fn = create_test_function(ast::Type::create_int());

//     auto sizeof_expr = std::make_unique<ast::SizeofExpr>(ast::Type::create_alias("Container"));

//     auto block = std::make_unique<ast::BlockStmt>();
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(sizeof_expr)));
//     test_fn->body = std::move(block->statements);

//     program.functions.push_back(std::move(test_fn));
//     generate(program);

//     // Verify zero-size array handling
//     StructType *container_type = module.try_get_named_struct_type("Container");
//     ASSERT_NE(container_type, nullptr);

//     // Should have 0 size but alignment may vary
//     EXPECT_EQ(container_type->size(), 0) << "Zero-size array struct should have 0 size";
//     EXPECT_TRUE(container_type->members().empty())
//         << "Zero-size array should not contribute to struct layout";
// }

// //===----------------------------------------------------------------------===//
// //                          Nested Structs Tests
// //===----------------------------------------------------------------------===//
// TEST_F(IrGeneratorTest, NestedStructs)
// {
//     /*
//         struct Inner {
//             a: i32,
//             b: f32
//         }

//         struct Outer {
//             x: Inner,
//             y: Inner
//         }

//         fn test(p: Outer) -> f32 {
//             return p.x.b + p.y.a;
//         }
//     */
//     ast::Program program;

//     // Define inner struct
//     auto inner_struct = std::make_unique<ast::StructDecl>();
//     inner_struct->name = "Inner";
//     inner_struct->add_field({"a", ast::Type::create_int()});
//     inner_struct->add_field({"b", ast::Type::create_float()});
//     program.structs.push_back(std::move(inner_struct));

//     // Define outer struct
//     auto outer_struct = std::make_unique<ast::StructDecl>();
//     outer_struct->name = "Outer";
//     outer_struct->add_field({"x", ast::Type::create_alias("Inner")});
//     outer_struct->add_field({"y", ast::Type::create_alias("Inner")});
//     program.structs.push_back(std::move(outer_struct));

//     // Test function
//     auto test_fn = create_test_function(ast::Type::create_float());
//     test_fn->add_param("p", ast::Type::create_alias("Outer"));

//     // Access nested members
//     auto x_b = std::make_unique<ast::MemberAccessExpr>(
//         std::make_unique<ast::MemberAccessExpr>(
//             std::make_unique<ast::VariableExpr>("p"),
//             "x",
//             TokenType::Dot),
//         "b",
//         TokenType::Dot);

//     auto y_a = std::make_unique<ast::MemberAccessExpr>(
//         std::make_unique<ast::MemberAccessExpr>(
//             std::make_unique<ast::VariableExpr>("p"),
//             "y",
//             TokenType::Dot),
//         "a",
//         TokenType::Dot);

//     auto return_expr = std::make_unique<ast::BinaryExpr>(
//         TokenType::Plus,
//         std::move(x_b),
//         std::move(y_a));

//     auto block = std::make_unique<ast::BlockStmt>();
//     block->statements.push_back(std::make_unique<ast::ReturnStmt>(std::move(return_expr)));
//     test_fn->body = std::move(block->statements);

//     program.functions.push_back(std::move(test_fn));
//     generate(program);

//     // Verify nested GEP instructions
//     Function *test_func = module.get_function("test");
//     ASSERT_NE(test_func, nullptr);

//     int gep_count = 0;
//     for (const auto &bb : *test_func)
//     {
//         for (const auto &inst : *bb)
//         {
//             if (auto *gep = dynamic_cast<const GetElementPtrInst *>(&inst))
//             {
//                 if (gep->base_pointer()->name() == "%p")
//                 {
//                     // Verify nested indices
//                     auto indices = gep->indices();
//                     ASSERT_GE(indices.size(), 2);

//                     // First index selects struct field (0 for x, 1 for y)
//                     EXPECT_EQ(dynamic_cast<ConstantInt *>(indices[0])->value(), 0);

//                     // Second index selects inner struct member
//                     if (indices.size() > 1)
//                     {
//                         auto *idx = dynamic_cast<ConstantInt *>(indices[1]);
//                         EXPECT_TRUE(idx->value() == 0 || idx->value() == 1)
//                             << "Inner struct member index should be 0(a) or 1(b)";
//                     }
//                     gep_count++;
//                 }
//             }
//         }
//     }
//     EXPECT_EQ(gep_count, 2) << "Should have 2 GEPs for nested member access";
// }
