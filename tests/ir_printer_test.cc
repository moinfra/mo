#include "src/ir_builder.h"
#include "src/ir_printer.h"
#include <gtest/gtest.h>

#define NOP(x) ((void)(x))

TEST(IRPrinterTest, SimpleFunction)
{
    Module module;
    IRBuilder builder(&module);

    // Create a function with a single basic block
    auto *func = module.create_function("main", module.get_void_type(), {});
    auto *bb = func->create_basic_block("entry");
    builder.set_insert_point(bb);

    // Create a return instruction
    builder.create_ret_void();

    // Print the module
    std::ostringstream os;
    IRPrinter::print_module(module, os);
    std::cout << os.str();

    std::string expected = R"(define void @main() {
entry:
  ret void
}
)";
    EXPECT_EQ(os.str(), expected);
}

TEST(IRPrinterTest, BinaryOperations)
{
    Module module;
    IRBuilder builder(&module);

    // Create a function with a single basic block
    auto *func = module.create_function("add", module.get_integer_type(32), {});
    auto *bb = func->create_basic_block("entry");
    builder.set_insert_point(bb);

    // Create binary operations
    auto *lhs = builder.get_int32(10);
    auto *rhs = builder.get_int32(20);
    auto *add = builder.create_add(lhs, rhs, "add_result");
    auto *sub = builder.create_sub(lhs, rhs, "sub_result");
    NOP(sub);
    auto *mul = builder.create_mul(lhs, rhs, "mul_result");
    NOP(mul);
    auto *udiv = builder.create_udiv(lhs, rhs, "udiv_result");
    NOP(udiv);
    auto *sdiv = builder.create_sdiv(lhs, rhs, "sdiv_result");
    NOP(sdiv);

    // Create a return instruction
    builder.create_ret(add);

    // Print the module
    std::ostringstream os;
    IRPrinter::print_module(module, os);
    std::cout << os.str();

    std::string expected = R"(define i32 @add() {
entry:
  %add_result = add i32 10, 20
  %sub_result = sub i32 10, 20
  %mul_result = mul i32 10, 20
  %udiv_result = udiv i32 10, 20
  %sdiv_result = sdiv i32 10, 20
  ret i32 %add_result
}
)";
    EXPECT_EQ(os.str(), expected);
}

TEST(IRPrinterTest, ControlFlow)
{
    Module module;
    IRBuilder builder(&module);

    // Create a function with multiple basic blocks
    auto *func = module.create_function("control_flow", module.get_void_type(), {});
    auto *entry = func->create_basic_block("entry");
    auto *true_bb = func->create_basic_block("true");
    auto *false_bb = func->create_basic_block("false");
    auto *merge_bb = func->create_basic_block("merge");

    builder.set_insert_point(entry);
    auto *cond = builder.get_int1(true);
    builder.create_cond_br(cond, true_bb, false_bb);

    builder.set_insert_point(true_bb);
    builder.create_br(merge_bb);

    builder.set_insert_point(false_bb);
    builder.create_br(merge_bb);

    builder.set_insert_point(merge_bb);
    builder.create_ret_void();

    // Print the module
    std::ostringstream os;
    IRPrinter::print_module(module, os);
    std::cout << os.str();

    std::string expected = R"(define void @control_flow() {
entry:
  br i1 true, label %true, label %false
true:
  br label %merge
false:
  br label %merge
merge:
  ret void
}
)";
    EXPECT_EQ(os.str(), expected);
}

TEST(IRPrinterTest, MemoryOperations)
{
    Module module;
    IRBuilder builder(&module);

    // Create a function with a single basic block
    auto *func = module.create_function("memory_ops", module.get_void_type(), {});
    auto *bb = func->create_basic_block("entry");
    builder.set_insert_point(bb);

    // Create memory operations
    auto *alloca = builder.create_alloca(module.get_integer_type(32), "var");
    auto *load = builder.create_load(alloca, "load_result");
    NOP(load);
    auto *store_val = builder.get_int32(42);
    builder.create_store(store_val, alloca);

    // Create a return instruction
    builder.create_ret_void();

    // Print the module
    std::ostringstream os;
    IRPrinter::print_module(module, os);
    std::cout << os.str();

    std::string expected = R"(define void @memory_ops() {
entry:
  %var = alloca i32
  %load_result = load i32, i32* %var
  store i32 42, i32* %var
  ret void
}
)";
    EXPECT_EQ(os.str(), expected);
}

TEST(IRPrinterTest, GEPAndStruct)
{
    Module module;
    IRBuilder builder(&module);

    // Create a struct type
    auto *struct_type = builder.create_struct_type("MyStruct");
    struct_type->set_body({module.get_integer_type(32), module.get_float_type(FloatType::Single)});

    // Create a function with a single basic block
    auto *func = module.create_function("struct_ops", module.get_void_type(), {});
    auto *bb = func->create_basic_block("entry");
    builder.set_insert_point(bb);

    // Create a struct alloca and GEP
    auto *alloca = builder.create_alloca(struct_type, "struct_var");
    auto *gep = builder.create_struct_gep(alloca, 0, "gep_result");
    NOP(gep);

    // Create a return instruction
    builder.create_ret_void();

    // Print the module
    std::ostringstream os;
    IRPrinter::print_module(module, os);
    std::cout << os.str();

    std::string expected = R"(define void @struct_ops() {
entry:
  %struct_var = alloca { i32, f32 }
  %gep_result = getelementptr { i32, f32 }*, { i32, f32 }* %struct_var, i32 0, i32 0
  ret void
}
)";
    EXPECT_EQ(os.str(), expected);
}
