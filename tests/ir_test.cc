#include "gtest/gtest.h"
#include "src/ir.h"

TEST(TypeSystem, VoidType)
{
    Module m;
    VoidType *voidType = VoidType::get(&m);
    EXPECT_EQ(voidType->size(), 0);
    EXPECT_EQ(voidType, VoidType::get(&m));
}

TEST(TypeSystem, IntegerType)
{
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    EXPECT_EQ(i32->bits(), 32);
    EXPECT_EQ(i32->size(), 4); // 32 bits = 4 bytes

    IntegerType *i8 = IntegerType::get(&m, 8);
    EXPECT_EQ(i8->bits(), 8);
    EXPECT_EQ(i8->size(), 1); // 8 bits = 1 byte
}

TEST(TypeSystem, PointerType)
{
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    PointerType *ptrType = PointerType::get(&m, i32);
    EXPECT_EQ(ptrType->element_type(), i32);
}

TEST(TypeSystem, ArrayType)
{
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    ArrayType *arrayType = ArrayType::get(&m, i32, 10);
    EXPECT_EQ(arrayType->num_elements(), 10);
    EXPECT_EQ(arrayType->element_type(), i32);
    EXPECT_EQ(arrayType->size(), 40); // 10 * 4 bytes
}

TEST(TypeSystem, StructType)
{
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    StructType *structType = StructType::create(&m, "MyStruct");
    structType->set_body({i32, i32});
    EXPECT_EQ(structType->get_member_type(0), i32);
    EXPECT_EQ(structType->get_member_offset(0), 0);
    EXPECT_EQ(structType->get_member_offset(1), 4);
    EXPECT_EQ(structType->size(), 8);
}

// TEST(ValueUser, ValueLifecycle) {
//     Module m;
//     IntegerType *i32 = IntegerType::get(&m, 32);
//     Value *v1 = new Value(i32, "v1");
//     Value *v2 = new Value(i32, "v2");

//     v1->add_user(reinterpret_cast<User *>(v2));
//     EXPECT_EQ(v1->users().size(), 1);

//     delete v2;
//     EXPECT_EQ(v1->users().size(), 0); // 验证 User 被销毁时自动更新
// }

// TEST(ValueUser, UserOperandManagement) {
//     Module m;
//     IntegerType *i32 = IntegerType::get(&m, 32);
//     User *u = new User(i32, "u");
//     Value *v = new Value(i32, "v");

//     u->set_operand(0, v);
//     EXPECT_EQ(u->operand(0), v);
//     EXPECT_EQ(v->users().size(), 1);

//     u->remove_use_of(v);
//     EXPECT_EQ(v->users().size(), 0);
// }

TEST(BasicBlockInstruction, InstructionList)
{
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    Function *f = m.create_function("func", i32, {});
    BasicBlock *bb = f->create_basic_block("bb");

    Instruction *inst1 = Instruction::create(Opcode::add, i32, {}, bb);
    Instruction *inst2 = Instruction::create(Opcode::sub, i32, {}, bb);

    EXPECT_EQ(bb->first_instruction(), inst1);
    EXPECT_EQ(bb->last_instruction(), inst2);
    EXPECT_EQ(inst1->next(), inst2);
    EXPECT_EQ(inst2->prev(), inst1);
}

TEST(BasicBlockInstruction, ControlFlow)
{
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    Function *f = m.create_function("func", i32, {});
    BasicBlock *bb1 = f->create_basic_block("bb1");
    BasicBlock *bb2 = f->create_basic_block("bb2");

    BranchInst *br = BranchInst::create(bb2, bb1);
    EXPECT_EQ(bb1->successors().size(), 1);
    EXPECT_EQ(bb1->successors()[0], bb2);
    EXPECT_EQ(bb2->predecessors().size(), 1);
    EXPECT_EQ(bb2->predecessors()[0], bb1);
}

TEST(Function, FunctionParameters)
{
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    PointerType *ptrType = PointerType::get(&m, i32);
    Function *f = m.create_function("func", i32, {i32, ptrType});

    EXPECT_EQ(f->param_types().size(), 2);
    EXPECT_EQ(f->param_types()[0], i32);
    EXPECT_EQ(f->param_types()[1], ptrType);
}

TEST(Module, TypeUniqueness)
{
    Module m;
    IntegerType *i32_1 = m.get_integer_type(32);
    IntegerType *i32_2 = m.get_integer_type(32);
    EXPECT_EQ(i32_1, i32_2);

    PointerType *ptr1 = m.get_pointer_type(i32_1);
    PointerType *ptr2 = m.get_pointer_type(i32_2);
    EXPECT_EQ(ptr1, ptr2);
}

TEST(Module, FunctionManagement)
{
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    Function *f1 = m.create_function("func1", i32, {});
    Function *f2 = m.create_function("func2", i32, {});

    EXPECT_EQ(m.functions().size(), 2);
    EXPECT_EQ(m.functions()[0], f1);
    EXPECT_EQ(m.functions()[1], f2);
}

TEST(Constant, ConstantInt) {
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    ConstantInt *c1 = ConstantInt::get(&m, i32, 42);
    ConstantInt *c2 = ConstantInt::get(&m, i32, 42);

    EXPECT_EQ(c1->value(), 42);
    EXPECT_EQ(c1, c2); // should be the same object
}

TEST(InstructionSubclasses, PhiInst) {
    Module m;
    IntegerType *i32 = IntegerType::get(&m, 32);
    Function *f = m.create_function("func", i32, {});
    BasicBlock *bb1 = f->create_basic_block("bb1");
    BasicBlock *bb2 = f->create_basic_block("bb2");

    PhiInst *phi = PhiInst::create(i32, bb1);
    phi->add_incoming(ConstantInt::get(&m, i32, 1), bb1);
    phi->add_incoming(ConstantInt::get(&m, i32, 2), bb2);

    EXPECT_EQ(phi->num_incoming(), 2);
    auto v0 = phi->get_incoming_value(0);
    ConstantInt *c0 = dynamic_cast<ConstantInt *>(v0);
    EXPECT_EQ(c0->value(), 1);
    auto v1 = phi->get_incoming_value(1);
    ConstantInt *c1 = dynamic_cast<ConstantInt *>(v1);
    EXPECT_EQ(c1->value(), 2);
    
    EXPECT_EQ(phi->get_incoming_block(0), bb1);
    EXPECT_EQ(phi->get_incoming_block(1), bb2);
}
