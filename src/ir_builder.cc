#include "ir_builder.h"
#include "utils.h"

IRBuilder::IRBuilder(Module *module)
    : module_(module), insert_block_(nullptr), insert_pos_(nullptr)
{
}

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

void IRBuilder::clear_insert_point()
{
    insert_block_ = nullptr;
    insert_pos_ = nullptr;
}

UnaryInst *IRBuilder::create_unary(Opcode opc, Value *operand, const std::string &name)
{
    auto *inst = UnaryInst::create(opc, operand, insert_block_, name);
    insert(inst);
    return inst;
}

UnaryInst *IRBuilder::create_neg(Value *val, const std::string &name)
{
    return create_unary(Opcode::Neg, val, name);
}

UnaryInst *IRBuilder::create_fneg(Value *val, const std::string &name)
{
    return create_unary(Opcode::FNeg, val, name);
}

UnaryInst *IRBuilder::create_not(Value *val, const std::string &name)
{
    return create_unary(Opcode::Not, val, name);
}

BinaryInst *IRBuilder::create_binary(Opcode opc, Value *lhs, Value *rhs,
                                     const std::string &name)
{
    // Enhanced type checking
    MO_DEBUG("Creating binary instruction: %d %s %s", int(opc),
             lhs->type()->to_string().c_str(), rhs->type()->to_string().c_str());

    assert(*lhs->type() == *rhs->type() && "Operand type mismatch");
    assert((lhs->type()->type_id() == Type::IntTy ||
            lhs->type()->type_id() == Type::FpTy) &&
           "Binary operation requires integer or float operands");

    auto *inst = BinaryInst::create(opc, lhs, rhs, insert_block_, name);
    insert(inst);
    return inst;
}

BinaryInst *IRBuilder::create_add(Value *lhs, Value *rhs,
                                  const std::string &name)
{
    return create_binary(Opcode::Add, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_sub(Value *lhs, Value *rhs,
                                  const std::string &name)
{
    return create_binary(Opcode::Sub, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_mul(Value *lhs, Value *rhs,
                                  const std::string &name)
{
    return create_binary(Opcode::Mul, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_udiv(Value *lhs, Value *rhs,
                                   const std::string &name)
{
    return create_binary(Opcode::UDiv, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_sdiv(Value *lhs, Value *rhs,
                                   const std::string &name)
{
    return create_binary(Opcode::SDiv, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_bitand(Value *lhs, Value *rhs,
                                     const std::string &name)
{
    return create_binary(Opcode::BitAnd, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_bitor(Value *lhs, Value *rhs,
                                    const std::string &name)
{
    return create_binary(Opcode::BitOr, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_bitxor(Value *lhs, Value *rhs,
                                     const std::string &name)
{
    return create_binary(Opcode::BitXor, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_srem(Value *lhs, Value *rhs, const std::string &name)
{
    return create_binary(Opcode::SRem, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_urem(Value *lhs, Value *rhs, const std::string &name)
{
    return create_binary(Opcode::URem, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_shl(Value *lhs, Value *rhs, const std::string &name)
{
    return create_binary(Opcode::Shl, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_ashr(Value *lhs, Value *rhs, const std::string &name)
{
    return create_binary(Opcode::AShr, lhs, rhs, name);
}

BinaryInst *IRBuilder::create_lshr(Value *lhs, Value *rhs, const std::string &name)
{
    return create_binary(Opcode::LShr, lhs, rhs, name);
}

ICmpInst *IRBuilder::create_icmp(ICmpInst::Predicate pred, Value *lhs,
                                 Value *rhs, const std::string &name)
{
    // Ensure integer types
    assert(lhs->type()->type_id() == Type::IntTy &&
           rhs->type()->type_id() == Type::IntTy &&
           "ICmp requires integer operands");
    assert(*lhs->type() == *rhs->type() && "Operand type mismatch");

    auto *inst = ICmpInst::create(pred, lhs, rhs, insert_block_);
    inst->set_name(name);
    insert(inst);
    return inst;
}

FCmpInst *IRBuilder::create_fcmp(FCmpInst::Predicate pred, Value *lhs,
                                 Value *rhs, const std::string &name)
{
    // Ensure floating-point types
    assert(lhs->type()->type_id() == Type::FpTy &&
           rhs->type()->type_id() == Type::FpTy &&
           "FCmp requires float operands");
    assert(*lhs->type() == *rhs->type() && "Operand type mismatch");

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
    // Condition must be i1 type
    assert(*cond->type() == *module_->get_integer_type(1) &&
           "Condition must be i1 type");

    auto *inst = BranchInst::create_cond(cond, true_bb, false_bb, insert_block_);
    insert(inst);
    return inst;
}

UnreachableInst *IRBuilder::create_unreachable()
{
    auto *inst = UnreachableInst::create(insert_block_);
    insert(inst);
    return inst;
}

ReturnInst *IRBuilder::create_ret(Value *value)
{
    // Get the current function
    assert(insert_block_);
    Function *cur_func = insert_block_->parent_function();
    assert(cur_func);
    assert(cur_func->return_type());

    if (value)
    {
        MO_ASSERT(*cur_func->return_type() == *value->type(), "Return type mismatch, expect `%s`, got `%s`",
                  cur_func->return_type()->to_string().c_str(),
                  value->type()->to_string().c_str());
    }
    else
    {
        MO_ASSERT(cur_func->return_type()->type_id() == Type::VoidTy,
                  "Void function cannot return value");
    }

    auto *inst = ReturnInst::create(value, insert_block_);

    insert(inst);
    return inst;
}

ReturnInst *IRBuilder::create_ret_void() { return create_ret(nullptr); }

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
    // Assume single-bit_width floating-point type
    auto *f_type = module_->get_float_type(32);
    return module_->get_constant_fp(f_type, val);
}

AllocaInst *IRBuilder::create_alloca(Type *type, const std::string &name)
{
    if (type->size() == 0)
    {
        MO_WARN("Trying to allocat zero size memory");
    }

    auto *inst = AllocaInst::create(type, insert_block_);
    inst->set_name(name);
    insert(inst);
    return inst;
}

AllocaInst *IRBuilder::create_entry_alloca(Type *type, const std::string &name)
{
    if (type->size() == 0)
    {
        MO_WARN("Trying to allocat zero size memory");
    }
    assert(insert_block_ && "Insert block must be set for entry alloca");

    Function *cur_func = insert_block_->parent_function();
    BasicBlock *entry_block = cur_func->entry_block();

    BasicBlock *saved_block = insert_block_;
    Instruction *saved_pos = insert_pos_;

    auto first_non_phi = entry_block->first_non_phi();
    if (first_non_phi != entry_block->last_instruction())
    {
        set_insert_point(first_non_phi);
    }
    else
    {
        set_insert_point(entry_block);
    }

    AllocaInst *inst = create_alloca(type, name);

    insert_block_ = saved_block;
    insert_pos_ = saved_pos;

    return inst;
}

LoadInst *IRBuilder::create_load(Value *ptr, const std::string &name)
{
    // Ensure pointer type
    MO_ASSERT(ptr->type()->is_pointer(),
              "Load operand must be pointer, actually `%s`", ptr->type()->to_string().c_str());

    auto *loaded_type = static_cast<PointerType *>(ptr->type())->element_type();
    if (loaded_type->size() == 0)
    {
        MO_WARN("Trying to load zero size memory");
    }
    auto *inst = LoadInst::create(ptr, insert_block_);
    inst->set_name(name);
    insert(inst);
    return inst;
}

StoreInst *IRBuilder::create_store(Value *value, Value *ptr)
{
    MO_DEBUG("Store value: %s to ptr: %s", value->type()->to_string().c_str(), ptr->type()->to_string().c_str());
    MO_ASSERT(ptr->type()->is_pointer(), "Store operand must be pointer");
    auto *ptr_type = static_cast<PointerType *>(ptr->type());
    MO_ASSERT(*value->type() == *ptr_type->element_type(),
              "Stored value type mismatch, expect `%s`, got `%s`", ptr_type->element_type()->to_string().c_str(), value->type()->to_string().c_str());

    auto *inst = StoreInst::create(value, ptr, insert_block_);
    insert(inst);
    return inst;
}

GetElementPtrInst *IRBuilder::create_gep(Value *ptr,
                                         std::vector<Value *> indices,
                                         const std::string &name)
{
    MO_ASSERT(ptr->type()->is_pointer(), "GEP base must be pointer, actually `%s`", ptr->type()->to_string().c_str());

    for (auto *index : indices)
    {
        MO_ASSERT(index->type()->is_integer(), "GEP indices must be integers, actually `%s`", index->type()->to_string().c_str());
    }

    auto *inst = GetElementPtrInst::create(ptr, indices, insert_block_, name);
    insert(inst);
    return inst;
}

Value *IRBuilder::create_struct_gep(Value *struct_ptr, unsigned idx,
                                    const std::string &name)
{
    // Structure pointer type verification
    assert(struct_ptr->type()->is_pointer() &&
           "struct_gep requires pointer operand");
    auto *ptr_type = static_cast<PointerType *>(struct_ptr->type());
    assert(ptr_type->element_type()->type_id() == Type::StructTy &&
           "struct_gep must operate on struct pointer");

    // Member index verification
    auto *struct_type = static_cast<StructType *>(ptr_type->element_type());
    MO_ASSERT(idx < struct_type->members().size(), "Struct index %u out of range which is %zu", idx, struct_type->members().size());

    // auto *index_type = module_->get_integer_type(32);
    auto *zero = module_->get_constant_int(32, 0);
    auto *idx_val = module_->get_constant_int(32, idx);
    return create_gep(struct_ptr, {zero, idx_val}, name);
}

Value *IRBuilder::create_extract_value(Value *agg_val,
                                       const std::vector<size_t> &indices,
                                       const std::string &name)
{
    // Verify aggregate type
    assert(agg_val->type()->is_aggregate() &&
           "extractvalue operand must be aggregate type");

    Type *cur_type = agg_val->type();
    Value *result = agg_val;

    // Traverse through each index
    for (unsigned idx : indices)
    {
        if (cur_type->type_id() == Type::StructTy)
        {
            auto *struct_type = static_cast<StructType *>(cur_type);
            MO_ASSERT(idx < struct_type->members().size(),
                      "Struct index %u out of range which is %zu",
                      idx, struct_type->members().size());
            cur_type = struct_type->members()[idx].type;

            // Create GEP for struct field access
            auto *zero = module_->get_constant_int(32, 0);
            auto *idx_val = module_->get_constant_int(32, idx);
            result = create_gep(result, {zero, idx_val}, name);
        }
        else if (cur_type->type_id() == Type::ArrayTy)
        {
            auto *array_type = static_cast<ArrayType *>(cur_type);
            MO_ASSERT(idx < array_type->num_elements(),
                      "Array index %u out of range which is %zu",
                      idx, array_type->num_elements());
            cur_type = array_type->element_type();

            // Create GEP for array element access
            auto *zero = module_->get_constant_int(32, 0);
            auto *idx_val = module_->get_constant_int(32, idx);
            result = create_gep(result, {zero, idx_val}, name);
        }
        else
        {
            assert(false && "Invalid aggregate type for extractvalue");
        }
    }

    // Load the final value
    return create_load(result, name);
}

ArrayType *IRBuilder::get_array_type(Type *elem_ty, uint64_t num)
{
    return module_->get_array_type(elem_ty, num);
}

StructType *IRBuilder::get_struct_type_anonymous(const std::vector<MemberInfo> &members)
{
    return module_->get_struct_type_anonymous(members);
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

// Call Instruction
CallInst *IRBuilder::create_call(Function *callee,
                                 const std::vector<Value *> &args,
                                 const std::string &name)
{
    assert(callee->num_args() == args.size() && "Argument count mismatch");
    for (size_t i = 0; i < args.size(); ++i)
    {
        assert(*args[i]->type() == *callee->arg_type(i) && "Argument type mismatch");
    }
    auto *inst = CallInst::create(callee, args, insert_block_, name);
    insert(inst);
    return inst;
}

CallInst *IRBuilder::create_call(Value *callee, const std::vector<Value *> &args,
                                 const std::string &name)
{
    // Handle direct function calls
    if (auto *func = dynamic_cast<Function *>(callee))
    {
        return create_call(func, args, name);
    }
    // TODO: use create_indirect_call
    // Handle indirect calls through function pointers
    assert(callee->type()->is_pointer() &&
           "Callee must be a function pointer");
    PointerType *ptr_type = static_cast<PointerType *>(callee->type());
    assert(ptr_type->element_type()->type_id() == Type::FuncTy &&
           "Callee must point to a function type");

    FunctionType *func_type = static_cast<FunctionType *>(ptr_type->element_type());
    assert(func_type->num_params() == args.size() &&
           "Argument count mismatch");

    for (size_t i = 0; i < args.size(); ++i)
    {
        assert(*args[i]->type() == *func_type->param_type(i) &&
               "Argument type mismatch");
    }

    auto *inst = CallInst::create(callee, func_type->return_type(), args,
                                  insert_block_, name);
    insert(inst);
    return inst;
}

// Indirect Call Instruction (via function pointer)
CallInst *IRBuilder::create_indirect_call(Value *callee,
                                          const std::vector<Value *> &args,
                                          const std::string &name)
{
    // Verify function pointer type
    assert(callee->type()->is_pointer() &&
           "Callee must be pointer type");
    auto *ptr_type = static_cast<PointerType *>(callee->type());
    assert(ptr_type->element_type()->type_id() == Type::TypeID::FuncTy &&
           "Callee must point to function type");

    // Verify argument compatibility
    auto *func_type = static_cast<FunctionType *>(ptr_type->element_type());
    assert(func_type->num_params() == args.size() && "Argument count mismatch");
    for (size_t i = 0; i < args.size(); ++i)
    {
        assert(*args[i]->type() == *func_type->param_type(i) &&
               "Argument type mismatch for indirect call");
    }

    auto *inst = CallInst::create(callee, func_type->return_type(), args,
                                  insert_block_, name);
    insert(inst);
    return inst;
}

bool is_legal_bitcast(Type *src, Type *dst)
{
    assert(src && "Source type must not be null");
    assert(dst && "Target type must not be null");

    if (src->size() == dst->size())
        return true;

    if (src->is_array() && dst->is_pointer())
    {
        Type *element_type = src->element_type();
        Type *ptr_target_type = dst->element_type();
        return element_type == ptr_target_type;
    }

    std::cerr << "Unsupported bitcast operation: " << Type::id_to_str(src->type_id())
              << " to " << Type::id_to_str(dst->type_id()) << std::endl;

    // FIXME: other cases such as struct to pointer / pointer length extension casts are not supported yet.
    return false;
}

// BitCast Instruction
BitCastInst *IRBuilder::create_bitcast(Value *val, Type *target_type,
                                       const std::string &name)
{
    assert(is_legal_bitcast(val->type(), target_type) &&
           "Bitcast types must have same size");
    auto *inst = BitCastInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

PtrToIntInst *IRBuilder::create_ptrtoint(Value *ptr, Type *target_type,
                                         const std::string &name)
{
    assert(ptr->type()->is_pointer() && "PtrToInt operand must be pointer");
    assert(target_type->type_id() == Type::IntTy && "PtrToInt target must be integer");
    auto *inst = PtrToIntInst::create(ptr, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

// Sign Extension Instruction
SExtInst *IRBuilder::create_sext(Value *val, Type *target_type,
                                 const std::string &name)
{
    assert(val->type()->type_id() == Type::IntTy && "SExt source must be integer");
    assert(target_type->type_id() == Type::IntTy &&
           "SExt target must be integer");
    assert(target_type->size() > val->type()->size() &&
           "SExt must expand to larger type");
    auto *inst = SExtInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

// Zero Extension Instruction
ZExtInst *IRBuilder::create_zext(Value *val, Type *target_type,
                                 const std::string &name)
{
    assert(val->type()->type_id() == Type::IntTy && "ZExt source must be integer");
    assert(target_type->type_id() == Type::IntTy &&
           "ZExt target must be integer");
    assert(target_type->size() > val->type()->size() &&
           "ZExt must expand to larger type");
    auto *inst = ZExtInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

// Floating-point Extension Instruction
FPExtInst *IRBuilder::create_fpext(Value *val, Type *target_type,
                                   const std::string &name)
{
    assert(val->type()->type_id() == Type::FpTy && "FPExt source must be float");
    assert(target_type->type_id() == Type::FpTy &&
           "FPExt target must be float");
    assert(target_type->size() > val->type()->size() &&
           "FPExt must expand to larger floating-point type");
    auto *inst = FPExtInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

// Floating-point Truncation Instruction
FPTruncInst *IRBuilder::create_fptrunc(Value *val, Type *target_type,
                                       const std::string &name)
{
    assert(val->type()->type_id() == Type::FpTy && "FPTrunc source must be float");
    assert(target_type->type_id() == Type::FpTy &&
           "FPTrunc target must be float");
    assert(target_type->size() < val->type()->size() &&
           "FPTrunc must reduce to smaller floating-point type");
    auto *inst = FPTruncInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

IntToPtrInst *IRBuilder::create_inttoptr(Value *val, Type *target_type,
                                         const std::string &name)
{
    assert(val->type()->type_id() == Type::IntTy &&
           "IntToPtr source must be integer");
    assert(target_type->is_pointer() &&
           "IntToPtr target must be pointer");
    auto *inst = IntToPtrInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

FPToSIInst *IRBuilder::create_fptosi(Value *val, Type *target_type,
                                     const std::string &name)
{
    assert(val->type()->type_id() == Type::FpTy &&
           "FPToSI source must be float");
    assert(target_type->type_id() == Type::IntTy &&
           "FPToSI target must be integer");
    auto *inst = FPToSIInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

FPToUIInst *IRBuilder::create_fptoui(Value *val, Type *target_type,
                                     const std::string &name)
{
    assert(val->type()->type_id() == Type::FpTy &&
           "FPToUI source must be float");
    assert(target_type->type_id() == Type::IntTy &&
           "FPToUI target must be integer");
    auto *inst = FPToUIInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

SIToFPInst *IRBuilder::create_sitofp(Value *val, Type *target_type,
                                     const std::string &name)
{
    assert(val->type()->type_id() == Type::IntTy &&
           "SIToFP source must be integer");
    assert(target_type->type_id() == Type::FpTy &&
           "SIToFP target must be float");
    auto *inst = SIToFPInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

UIToFPInst *IRBuilder::create_uitofp(Value *val, Type *target_type,
                                     const std::string &name)
{
    assert(val->type()->type_id() == Type::IntTy &&
           "UIToFP source must be integer");
    assert(target_type->type_id() == Type::FpTy &&
           "UIToFP target must be float");
    auto *inst = UIToFPInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}

// Truncate Instruction
TruncInst *IRBuilder::create_trunc(Value *val, Type *target_type,
                                   const std::string &name)
{
    assert(val->type()->type_id() == Type::IntTy &&
           "Trunc source must be integer");
    assert(target_type->type_id() == Type::IntTy &&
           "Trunc target must be integer");
    assert(target_type->size() < val->type()->size() &&
           "Trunc must reduce to smaller type");
    auto *inst = TruncInst::create(val, target_type, insert_block_, name);
    insert(inst);
    return inst;
}
Value *IRBuilder::create_cast(Value *src_val, Type *target_type,
                              const std::string &name,
                              bool is_explicit /* = false */,
                              bool strict_mode /* = false */)
{
    // Get the source type from the source value.
    Type *src_type = src_val->type();
    // If the source and target types are the same, return the source value directly.
    if (*src_type == *target_type)
        return src_val;

    Instruction *inst = nullptr;
    // Try to dynamically cast the source and target types to integer and float types.
    const auto src_int = dynamic_cast<IntegerType *>(src_type);
    const auto tgt_int = dynamic_cast<IntegerType *>(target_type);
    const auto src_fp = dynamic_cast<FloatType *>(src_type);
    const auto tgt_fp = dynamic_cast<FloatType *>(target_type);

    /*-------------------- Special handling for boolean type --------------------*/
    if (tgt_int && tgt_int->bit_width() == 1)
    {
        // Any type conversion to boolean requires explicit comparison with zero.
        Value *zero = nullptr;
        if (src_int)
        {
            // Get the integer constant zero for the source integer type.
            zero = module_->get_constant_int(src_int, 0);
            // Create an ICmp instruction to compare the source value with zero for inequality.
            inst = create_icmp(ICmpInst::NE, src_val, zero, name);
        }
        else if (src_fp)
        {
            // Get the floating-point constant zero for the source floating-point type.
            zero = module_->get_constant_fp(src_fp, 0.0);
            // Create an FCmp instruction to compare the source value with zero for un-ordered or not-equal.
            inst = create_fcmp(FCmpInst::ONE, src_val, zero, name);
        }
        if (inst)
        {
            // Insert the instruction into the current basic block.
            insert(inst);
            // Return the created instruction.
            return inst;
        }
    }

    /*-------------------- Integer type conversion --------------------*/
    if (src_int && tgt_int)
    {
        // Get the bit widths of the source and target integer types.
        const uint8_t src_bit_width = src_int->bit_width();
        const uint8_t tgt_bit_width = tgt_int->bit_width();

        if (src_bit_width < tgt_bit_width)
        {
            // If the source bit width is less than the target bit width, perform either sign extension or zero extension.
            // The decision depends on whether the source integer type is signed or unsigned.
            inst = src_int->is_signed() ? static_cast<Instruction *>(create_sext(src_val, target_type, name)) : static_cast<Instruction *>(create_zext(src_val, target_type, name));
        }
        else if (src_bit_width > tgt_bit_width)
        {
            // If the source bit width is greater than the target bit width, perform truncation.
            inst = create_trunc(src_val, target_type, name);
        }
        else
        {
            // If the bit widths are the same but the signs are different, use bitcast.
            inst = create_bitcast(src_val, target_type, name);
        }
    }

    /*-------------------- Floating-point and integer interconversion --------------------*/
    else if (src_fp && tgt_int)
    {
        // If converting from floating-point to integer, issue a warning if strict mode is enabled and the conversion is implicit.
        if (strict_mode && !is_explicit)
        {
            std::cerr << "Warning: implicit float to integer conversion\n";
        }
        // Perform either floating-point to signed integer conversion or floating-point to unsigned integer conversion.
        // The decision depends on whether the target integer type is signed or unsigned.
        inst = tgt_int->is_signed() ? static_cast<Instruction *>(create_fptosi(src_val, target_type, name)) : static_cast<Instruction *>(create_fptoui(src_val, target_type, name));
    }
    else if (src_int && tgt_fp)
    {
        // Perform either signed integer to floating-point conversion or unsigned integer to floating-point conversion.
        // The decision depends on whether the source integer type is signed or unsigned.
        inst = src_int->is_signed() ? static_cast<Instruction *>(create_sitofp(src_val, target_type, name)) : static_cast<Instruction *>(create_uitofp(src_val, target_type, name));
    }

    /*-------------------- Floating-point bit_width adjustment --------------------*/
    else if (src_fp && tgt_fp)
    {
        // If converting between floating-point types, adjust the bit_width.
        if (src_fp->bit_width() < tgt_fp->bit_width())
        {
            // If the source floating-point type has lower bit_width than the target, perform floating-point extension.
            inst = create_fpext(src_val, target_type, name);
        }
        else
        {
            // Otherwise, perform floating-point truncation.
            inst = create_fptrunc(src_val, target_type, name);
        }
    }

    /*-------------------- Pointer-related conversions --------------------*/
    else if (src_type->is_pointer())
    {
        // If the source type is a pointer.
        if (target_type->is_pointer())
        {
            // If the target type is also a pointer, perform bitcast.
            inst = create_bitcast(src_val, target_type, name);
        }
        else if (target_type->is_integer())
        {
            // If the target type is an integer, perform pointer to integer conversion.
            assert(src_type->size() == target_type->size() && "PtrToInt size mismatch");
            inst = create_ptrtoint(src_val, target_type, name);
        }
    }
    else if (target_type->is_pointer())
    {
        // If the target type is a pointer and the source type is not a pointer.
        assert(src_type->is_integer() && "IntToPtr requires integer source");
        assert(src_type->size() == target_type->size() && "IntToPtr size mismatch");
        // Perform integer to pointer conversion.
        inst = create_inttoptr(src_val, target_type, name);
    }

    // Assert that the instruction was created.  If not, the cast is unsupported.
    assert(inst && ("Unsupported cast: " + src_type->name() + " -> " + target_type->name()).c_str());
    // Insert the instruction into the current basic block.
    insert(inst);
    // Return the created instruction.
    return inst;
}
