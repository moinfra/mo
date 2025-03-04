#include "gtest/gtest.h"
#include "src/ir_builder.h"
#define NOP(x) ((void)(x))

TEST(IRBuilder, InsertPoint)
{
    Module m;
    IntegerType *i32 = m.get_integer_type(32);
    Function *f = m.create_function("func", i32, {});
    BasicBlock *bb = f->create_basic_block("bb");

    IRBuilder builder(&m);
    builder.set_insert_point(bb);

    Value *v1 = builder.create_binary(Opcode::Add, m.get_constant_int(i32, 1), m.get_constant_int(i32, 2));
    EXPECT_EQ(bb->last_instruction()->opcode(), Opcode::Add);
    NOP(v1);
}

TEST(IRBuilder, InstructionGeneration)
{
    Module m;
    IntegerType *i32 = m.get_integer_type(32);
    Function *f = m.create_function("func", i32, {});
    BasicBlock *bb1 = f->create_basic_block("bb1");
    BasicBlock *bb2 = f->create_basic_block("bb2");

    IRBuilder builder(&m);
    builder.set_insert_point(bb1);

    Value *cond = builder.create_icmp(ICmpInst::Predicate::SLT, m.get_constant_int(i32, 1), m.get_constant_int(i32, 2));
    builder.create_cond_br(cond, bb1, bb2);

    EXPECT_EQ(bb1->successors().size(), 2);
    EXPECT_EQ(bb1->successors()[0], bb1);
    EXPECT_EQ(bb1->successors()[1], bb2);
}

TEST(IRBuilder, ComplexTypeSupport)
{
    Module m;
    IntegerType *i32 = m.get_integer_type(32);
    std::vector<MemberInfo> members = {MemberInfo("a", i32), MemberInfo("b", i32)};
    StructType *structType = m.get_struct_type("MyStruct", members);

    Function *f = m.create_function("func", i32, {});
    BasicBlock *bb = f->create_basic_block("bb");

    IRBuilder builder(&m);
    builder.set_insert_point(bb);

    AllocaInst *alloca = builder.create_alloca(structType);
    Value *gep = builder.create_struct_gep(alloca, 1);

    EXPECT_EQ(gep->type(), m.get_pointer_type(i32));
}

// TEST(BoundaryConditions, NullOperand) {
//     Module m;
//     IntegerType *i32 = m.get_integer_type( 32);
//     User *u = new User(i32, "u");

//     EXPECT_DEATH(u->set_operand(0, nullptr), ".*"); // 断言失败
// }

// TEST(BoundaryConditions, IllegalTypeCombination) {
//     Module m;
//     VoidType *voidType = m.get_void_type(;

//     EXPECT_DEATH(m.get_array_type( voidType, 10), ".*"); // 断言失败
// }

TEST(Integration, FibonacciFunction)
{
    Module m;
    IntegerType *i32 = m.get_integer_type(32);
    Function *fib = m.create_function("fib", i32, {{"n", i32}});

    // 创建基本块
    BasicBlock *entry = fib->create_basic_block("entry");
    BasicBlock *loop = fib->create_basic_block("loop");
    BasicBlock *exit = fib->create_basic_block("exit");

    IRBuilder builder(&m);
    builder.set_insert_point(entry);

    Value *n = fib->arg(0);

    // （可选）处理 n == 0 的情况
    BasicBlock *return_zero = fib->create_basic_block("return_zero");
    BasicBlock *continue_block = fib->create_basic_block("continue");

    Value *cond_zero = builder.create_icmp(ICmpInst::Predicate::EQ, n, m.get_constant_int(i32, 0));
    builder.create_cond_br(cond_zero, return_zero, continue_block);

    builder.set_insert_point(return_zero);
    builder.create_ret(m.get_constant_int(i32, 0));

    builder.set_insert_point(continue_block);

    // 分配本地变量 a, b 和 counter
    Value *a = builder.create_alloca(i32);
    Value *b = builder.create_alloca(i32);
    Value *counter = builder.create_alloca(i32); // 新增本地计数器

    // 初始化 a = 0, b = 1, counter = n
    builder.create_store(m.get_constant_int(i32, 0), a);
    builder.create_store(m.get_constant_int(i32, 1), b);
    builder.create_store(n, counter); // 将 n 的值存储到 counter

    builder.create_br(loop);

    // 循环块
    builder.set_insert_point(loop);
    Value *a_val = builder.create_load(a);
    Value *b_val = builder.create_load(b);
    Value *sum = builder.create_add(a_val, b_val);
    builder.create_store(b_val, a);
    builder.create_store(sum, b);

    // 使用 counter 而不是 n
    Value *counter_val = builder.create_load(counter);
    Value *counter_minus_1 = builder.create_sub(counter_val, m.get_constant_int(i32, 1));
    builder.create_store(counter_minus_1, counter); // 更新 counter

    Value *cond = builder.create_icmp(ICmpInst::Predicate::SGT, counter_minus_1, m.get_constant_int(i32, 0));
    builder.create_cond_br(cond, loop, exit);

    // 退出块
    builder.set_insert_point(exit);
    Value *result = builder.create_load(b);
    builder.create_ret(result);

    // 测试基本块结构
    EXPECT_EQ(fib->basic_blocks().size(), 5); // 增加了 return_zero 和 continue_block
    EXPECT_EQ(entry->successors().size(), 2); // entry 现在有两个后继
    EXPECT_EQ(loop->successors().size(), 2);
    EXPECT_EQ(exit->predecessors().size(), 1);
}
