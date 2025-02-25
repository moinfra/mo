#include "ir_builder.h"
IRBuilder::IRBuilder(Module *module)
    : module_(module), insert_block_(nullptr), insert_pos_(nullptr) {}

void IRBuilder::set_insert_point(BasicBlock *bb)
{
    insert_block_ = bb;
    insert_pos_ = nullptr;
}

void IRBuilder::set_insert_point(Instruction *inst)
{
    insert_block_ = inst->parent();
    insert_pos_ = inst;
}

BinaryInst *IRBuilder::create_binary(Opcode opc, Value *lhs, Value *rhs,
                                     const std::string &name)
{
    // 增强类型检查
    assert(lhs->type() == rhs->type() && "Operand type mismatch");
    assert((lhs->type()->type_id() == Type::IntTy ||
            lhs->type()->type_id() == Type::FpTy) &&
           "Binary operation requires integer or float operands");

    // 特定操作类型检查
    switch (opc)
    {
    case Opcode::Add:
    case Opcode::Sub:
    case Opcode::Mul:
        break; // 允许整数和浮点
    case Opcode::UDiv:
    case Opcode::SDiv:
        assert(lhs->type()->type_id() == Type::IntTy &&
               "Division requires integer types");
        break;
    case Opcode::FCmp:
        assert(lhs->type()->type_id() == Type::FpTy &&
               "FCmp requires float types");
        break;
    default:
        assert(false && "Unsupported binary operation");
    }

    auto *inst = BinaryInst::create(opc, lhs, rhs, insert_block_, name);
    insert(inst);
    return inst;
}

ICmpInst *IRBuilder::create_icmp(ICmpInst::Predicate pred, Value *lhs, Value *rhs,
                                 const std::string &name)
{
    // 确保整数类型
    assert(lhs->type()->type_id() == Type::IntTy &&
           rhs->type()->type_id() == Type::IntTy &&
           "ICmp requires integer operands");
    assert(lhs->type() == rhs->type() && "Operand type mismatch");

    auto *inst = ICmpInst::create(pred, lhs, rhs, insert_block_);
    inst->set_name(name);
    insert(inst);
    return inst;
}

FCmpInst *IRBuilder::create_fcmp(FCmpInst::Predicate pred, Value *lhs, Value *rhs,
                                 const std::string &name)
{
    // 确保浮点类型
    assert(lhs->type()->type_id() == Type::FpTy &&
           rhs->type()->type_id() == Type::FpTy &&
           "FCmp requires float operands");
    assert(lhs->type() == rhs->type() && "Operand type mismatch");

    auto *inst = FCmpInst::create(pred, lhs, rhs, insert_block_, name);
    insert(inst);
    return inst;
}

BranchInst *IRBuilder::create_br(BasicBlock *target)
{
    auto *inst = BranchInst::create(target, insert_block_);
    insert(inst);
    return inst;
}

BranchInst *IRBuilder::create_cond_br(Value *cond, BasicBlock *true_bb,
                                      BasicBlock *false_bb)
{
    // 条件必须是i1类型
    assert(cond->type() == module_->get_integer_type(1) &&
           "Condition must be i1 type");

    auto *inst = BranchInst::create_cond(cond, true_bb, false_bb, insert_block_);
    insert(inst);
    return inst;
}

ReturnInst *IRBuilder::create_ret(Value *value)
{
    // 获取当前函数
    Function *cur_func = insert_block_->parent();

    if (value)
    {
        assert(cur_func->return_type() == value->type() &&
               "Return type mismatch");
    }
    else
    {
        assert(cur_func->return_type()->type_id() == Type::VoidTy &&
               "Void function cannot return value");
    }

    auto *inst = ReturnInst::create(value, insert_block_);
    insert(inst);
    return inst;
}

ReturnInst *IRBuilder::create_ret_void()
{
    return create_ret(nullptr);
}

PhiInst *IRBuilder::create_phi(Type *type, const std::string &name)
{
    assert(type->type_id() != Type::VoidTy && "Phi cannot have void type");

    auto *inst = PhiInst::create(type, insert_block_);
    inst->set_name(name);
    insert(inst);
    return inst;
}

ConstantInt *IRBuilder::get_int32(int32_t val)
{
    return module_->get_constant_int(32, val);
}

ConstantInt *IRBuilder::get_int64(int64_t val)
{
    return module_->get_constant_int(64, val);
}

ConstantInt *IRBuilder::get_int1(bool val)
{
    return module_->get_constant_int(1, val ? 1 : 0);
}

ConstantFP *IRBuilder::get_float(double val)
{
    // 假设使用单精度浮点类型
    auto *f_type = FloatType::get(module_, FloatType::Single);
    return ConstantFP::get(module_, f_type, val);
}

AllocaInst *IRBuilder::create_alloca(Type *type, const std::string &name)
{
    assert(type->size() > 0 && "Cannot allocate zero-sized type");

    auto *inst = AllocaInst::create(type, insert_block_);
    inst->set_name(name);
    insert(inst);
    return inst;
}

LoadInst *IRBuilder::create_load(Value *ptr, const std::string &name)
{
    // 确保指针类型
    assert(ptr->type()->type_id() == Type::PtrTy &&
           "Load operand must be pointer");

    auto *loaded_type = static_cast<PointerType *>(ptr->type())->element_type();
    assert(loaded_type->size() > 0 && "Cannot load zero-sized type");
    auto *inst = LoadInst::create(ptr, insert_block_);
    inst->set_name(name);
    insert(inst);
    return inst;
}

StoreInst *IRBuilder::create_store(Value *value, Value *ptr)
{
    std::cout << "create_store target to " << Type::id_to_str(ptr->type()->type_id()) << std::endl;
    assert(ptr->type()->type_id() == Type::PtrTy &&
           "Store operand must be pointer");
    auto *ptr_type = static_cast<PointerType *>(ptr->type());
    assert(value->type() == ptr_type->element_type() &&
           "Stored value type mismatch");

    auto *inst = StoreInst::create(value, ptr, insert_block_);
    insert(inst);
    return inst;
}

GetElementPtrInst *IRBuilder::create_gep(Value *ptr, std::vector<Value *> indices,
                                         const std::string &name)
{
    assert(ptr->type()->type_id() == Type::PtrTy &&
           "GEP base must be pointer");

    for (auto *index : indices)
    {
        assert(index->type()->type_id() == Type::IntTy &&
               "GEP indices must be integers");
    }

    auto *inst = GetElementPtrInst::create(ptr, indices, insert_block_, name);
    insert(inst);
    return inst;
}

Value *IRBuilder::create_struct_gep(Value *struct_ptr, unsigned idx,
                                    const std::string &name)
{
    // 结构体指针类型验证
    assert(struct_ptr->type()->type_id() == Type::PtrTy &&
           "struct_gep requires pointer operand");
    auto *ptr_type = static_cast<PointerType *>(struct_ptr->type());
    assert(ptr_type->element_type()->type_id() == Type::StructTy &&
           "struct_gep must operate on struct pointer");

    // 成员索引验证
    auto *struct_type = static_cast<StructType *>(ptr_type->element_type());
    assert(idx < struct_type->members().size() &&
           "Struct index out of bounds");

    // auto *index_type = module_->get_integer_type(32);
    auto *zero = module_->get_constant_int(32, 0);
    auto *idx_val = module_->get_constant_int(32, idx);
    return create_gep(struct_ptr, {zero, idx_val}, name);
}

ArrayType *IRBuilder::get_array_type(Type *elem_ty, uint64_t num)
{
    return module_->get_array_type(elem_ty, num);
}

StructType *IRBuilder::create_struct_type(const std::string &name)
{
    return module_->create_struct_type(name);
}

StructType *IRBuilder::get_struct_type(const std::vector<Type *> &members)
{
    return module_->get_struct_type(members);
}

void IRBuilder::insert(Instruction *inst)
{
    if (insert_pos_)
    {
        insert_block_->insert_before(insert_pos_, std::unique_ptr<Instruction>(inst));
    }
    else
    {
        insert_block_->append(inst);
    }
}
