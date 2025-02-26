#include "ir.h"
#include <sstream>
//===----------------------------------------------------------------------===//
//                              Type Implementation
//===----------------------------------------------------------------------===//
Type *Type::get_void_type(Module *m)
{
    return m->get_void_type();
}

IntegerType::IntegerType(Module *m, unsigned bits)
    : Type(IntTy, m), bits_(bits) {}

IntegerType *IntegerType::get(Module *m, unsigned bits)
{
    return m->get_integer_type(bits);
}

FloatType::FloatType(Module *m, FloatType::Precision precision)
    : Type(FpTy, m), precision_(precision)
{
    switch (precision)
    {
    case Half:
        bits_ = 16;
        break;
    case Single:
        bits_ = 32;
        break;
    case Double:
        bits_ = 64;
        break;
    case Quad:
        bits_ = 128;
        break;
    default:
        assert(0 && "Invalid float precision");
    }
}

FloatType *FloatType::get(Module *m, FloatType::Precision precision)
{
    return m->get_float_type(precision);
}

PointerType::PointerType(Module *m, Type *element_type)
    : Type(PtrTy, m), element_type_(element_type) {}

PointerType *PointerType::get(Module *m, Type *element_type)
{
    return m->get_pointer_type(element_type);
}

VoidType *VoidType::get(Module *m)
{
    if (!m->void_type_)
    {
        m->void_type_ = std::unique_ptr<VoidType>(new VoidType(m));
    }
    return static_cast<VoidType *>(m->void_type_.get());
}

ArrayType::ArrayType(Module *m, Type *element_type, uint64_t num_elements)
    : Type(ArrayTy, m), element_type_(element_type),
      num_elements_(num_elements)
{
    assert(element_type && "Invalid element type");
}

size_t ArrayType::size() const
{
    return element_type_->size() * num_elements_;
}

ArrayType *ArrayType::get(Module *m, Type *element_type, uint64_t num_elements)
{
    return m->get_array_type(element_type, num_elements);
}

StructType::StructType(Module *m)
    : Type(StructTy, m), is_opaque_(true), size_(0) {}

void StructType::set_body(std::vector<Type *> members)
{
    assert(is_opaque_ && "Struct already has body");
    members_ = std::move(members);
    is_opaque_ = false;

    // Calculate offsets (simple compact layout)
    size_ = 0;
    for (auto *ty : members_)
    {
        offsets_.push_back(size_);
        size_ += ty->size();
    }
}

StructType *StructType::create(Module *m, const std::string &name)
{
    return m->create_struct_type(name);
}

Type *StructType::get_member_type(unsigned i) const
{
    assert(i < members_.size() && "Invalid member index");
    return members_[i];
}

size_t StructType::get_member_offset(unsigned index) const
{
    assert(index < members_.size() && "Invalid member index");
    return offsets_[index];
}

size_t StructType::size() const
{
    assert(!is_opaque_ && "Opaque struct has no size");
    return size_;
}

//===----------------------------------------------------------------------===//
//                             Value Implementation
//===----------------------------------------------------------------------===//
Value::~Value()
{
    // Notify all users that this value is invalid
    for (auto *user : users_)
    {
        user->remove_use_of(this);
    }
}

void Value::remove_user(Value *user)
{
    users_.erase(std::remove(users_.begin(), users_.end(), user), users_.end());
}

//===----------------------------------------------------------------------===//
//                             User Implementation
//===----------------------------------------------------------------------===//
User::~User()
{
    for (auto *op : operands_)
    {
        if (op)
            op->remove_user(this);
    }
}

void User::set_operand(unsigned i, Value *v)
{
    if (i < operands_.size() && operands_[i])
    {
        operands_[i]->remove_user(this);
    }

    if (i >= operands_.size())
    {
        operands_.resize(i + 1);
    }

    operands_[i] = v;
    if (v)
    {
        v->add_user(this);
    }
}

void User::remove_use_of(Value *v)
{
    auto it = std::find(operands_.begin(), operands_.end(), v);
    if (it != operands_.end())
    {
        *it = nullptr;
    }
}

//===----------------------------------------------------------------------===//
//                           Instruction Implementation
//===----------------------------------------------------------------------===//
Instruction::Instruction(Opcode opcode, Type *type, BasicBlock *parent,
                         std::vector<Value *> operands, const std::string &name)
    : User(type, name), opcode_(opcode), parent_(parent),
      prev_(nullptr), next_(nullptr)
{
    operands_ = std::move(operands);
    for (auto *op : operands_)
    {
        if (op)
            op->add_user(this);
    }
}

Instruction *Instruction::create(Opcode opc, Type *type,
                                 std::vector<Value *> operands,
                                 BasicBlock *parent)
{
    auto *inst = new Instruction(opc, type, parent, operands);
    parent->insert_before(nullptr, std::unique_ptr<Instruction>(inst));
    return inst;
}

//===----------------------------------------------------------------------===//
//                           BasicBlock Implementation
//===----------------------------------------------------------------------===//
BasicBlock::BasicBlock(const std::string &name, Function *parent)
    : Value(Type::get_void_type(parent->parent()), name),
      parent_(parent), head_(nullptr), tail_(nullptr) {}

BasicBlock::~BasicBlock()
{
    auto *inst = head_;
    while (inst)
    {
        auto *next = inst->next();
        delete inst;
        inst = next;
    }
}

void BasicBlock::insert_before(Instruction *pos, std::unique_ptr<Instruction> inst)
{
    auto *new_inst = inst.release();
    new_inst->parent_ = this;

    if (!pos)
    {
        // Insert at the end
        if (!tail_)
        {
            head_ = tail_ = new_inst;
        }
        else
        {
            tail_->next_ = new_inst;
            new_inst->prev_ = tail_;
            tail_ = new_inst;
        }
    }
    else
    {
        new_inst->next_ = pos;
        new_inst->prev_ = pos->prev_;
        if (pos->prev_)
        {
            pos->prev_->next_ = new_inst;
        }
        else
        {
            head_ = new_inst;
        }
        pos->prev_ = new_inst;
    }
}

void BasicBlock::insert_after(Instruction *pos, std::unique_ptr<Instruction> inst)
{
    auto *new_inst = inst.release();
    new_inst->parent_ = this;

    if (!pos)
    {
        // Insert at the beginning
        if (!head_)
        {
            head_ = tail_ = new_inst;
        }
        else
        {
            new_inst->next_ = head_;
            head_->prev_ = new_inst;
            head_ = new_inst;
        }
    }
    else
    {
        new_inst->prev_ = pos;
        new_inst->next_ = pos->next_;
        if (pos->next_)
        {
            pos->next_->prev_ = new_inst;
        }
        else
        {
            tail_ = new_inst;
        }
        pos->next_ = new_inst;
    }
}

void BasicBlock::add_successor(BasicBlock *bb)
{
    if (std::find(successors_.begin(), successors_.end(), bb) == successors_.end())
    {
        successors_.push_back(bb);
        bb->predecessors_.push_back(this);
    }
}

void BasicBlock::append(Instruction *inst)
{
    inst->parent_ = this;

    if (!tail_)
    {
        head_ = tail_ = inst;
    }
    else
    {
        tail_->next_ = inst;
        inst->prev_ = tail_;
        tail_ = inst;
    }
}

//===----------------------------------------------------------------------===//
//                             Function Implementation
//===----------------------------------------------------------------------===//
Function::Function(const std::string &name, Module *parent, Type *return_type,
                   const ParamList &params)
    : Value(return_type, name),
      parent_(parent),
      return_type_(return_type)
{
    // Create arguments
    for (const auto &[param_name, param_type] : params)
    {
        arguments_.emplace_back(
            std::make_unique<Argument>(param_name, param_type, this));
        args_.push_back(arguments_.back().get());
    }
}

Function::~Function() = default;

BasicBlock *Function::create_basic_block(const std::string &name)
{
    basic_blocks_.push_back(std::make_unique<BasicBlock>(name, this));
    basic_block_ptrs_.push_back(basic_blocks_.back().get());
    return basic_block_ptrs_.back();
}

//===----------------------------------------------------------------------===//
//                             Module Implementation
//===----------------------------------------------------------------------===//
Module::Module()
{
    VoidType::get(this);
}

Module::~Module() = default;

Function *Module::create_function(
    const std::string &name,
    Type *return_type,
    const std::vector<std::pair<std::string, Type *>> &params)
{
    functions_.push_back(
        std::make_unique<Function>(name, this, return_type, params));
    function_ptrs_.push_back(functions_.back().get());
    return function_ptrs_.back();
}

GlobalVariable *Module::create_global_variable(Type *type, bool is_constant, Constant *initializer, const std::string &name)
{
    auto *gv_ptr = new GlobalVariable(type, is_constant, initializer, name);
    auto gv = std::unique_ptr<GlobalVariable>(gv_ptr);
    global_variables_.push_back(std::move(gv));
    global_variable_ptrs_.push_back(gv_ptr);
    return gv_ptr;
}

IntegerType *Module::get_integer_type(unsigned bits)
{
    auto &type = integer_types_[bits];
    if (!type)
    {
        type = std::unique_ptr<IntegerType>(new IntegerType(this, bits));
    }
    return type.get();
}

FloatType *Module::get_float_type(FloatType::Precision precision)
{
    auto &type = float_types_[precision];
    if (!type)
    {
        type = std::unique_ptr<FloatType>(new FloatType(this, precision));
    }
    return type.get();
}

PointerType *Module::get_pointer_type(Type *element_type)
{
    auto &type = pointer_types_[element_type];
    if (!type)
    {
        type = std::unique_ptr<PointerType>(new PointerType(this, element_type));
    }
    return type.get();
}

FunctionType *Module::get_function_type(Type *return_type, const std::vector<Type *> &param_types)
{
    auto key = std::make_pair(return_type, param_types);
    auto it = function_types_.find(key);
    if (it != function_types_.end())
    {
        return it->second.get();
    }

    auto function_type = std::unique_ptr<FunctionType>(new FunctionType(this, return_type, param_types));
    auto *result = function_type.get();
    function_types_[key] = std::move(function_type);
    return result;
}

Type *Module::get_void_type()
{
    return void_type_.get();
}

ArrayType *Module::get_array_type(Type *element_type, uint64_t num_elements)
{
    auto ty = new ArrayType(this, element_type, num_elements);
    array_types_[{element_type, num_elements}] = std::unique_ptr<ArrayType>(ty);
    return ty;
}

StructType *Module::create_struct_type(const std::string &name)
{
    auto ty = new StructType(this);
    this->struct_types_.push_back(std::unique_ptr<StructType>(ty));
    return ty;
}

// FIXME: should not get by members
StructType *Module::get_struct_type(const std::vector<Type *> &members)
{
    // Find existing struct
    for (auto &st : struct_types_)
    {
        if (!st->is_opaque() && st->members() == members)
        {
            return st.get();
        }
    }

    // Create new struct
    auto *st = new StructType(this);
    st->set_body(members);
    struct_types_.push_back(std::unique_ptr<StructType>(st));
    return st;
}

VectorType *Module::get_vector_type(Type *element_type, uint64_t num_elements)
{
    auto key = std::make_pair(element_type, num_elements);
    auto it = vector_types_.find(key);
    if (it != vector_types_.end())
    {
        return it->second.get();
    }

    auto vector_type = std::unique_ptr<VectorType>(new VectorType(this, element_type, num_elements));
    auto *result = vector_type.get();
    vector_types_[key] = std::move(vector_type);
    return result;
}

ConstantInt *Module::get_constant_int(IntegerType *type, uint64_t value)
{
    if (auto it = constant_ints_.find({type, value}); it != constant_ints_.end())
    {
        return it->second.get();
    }

    auto &constant = constant_ints_[{type, value}];
    constant = std::unique_ptr<ConstantInt>(new ConstantInt(type, value));
    return constant.get();
}

ConstantInt *Module::get_constant_int(unsigned bits, uint64_t value)
{
    return get_constant_int(get_integer_type(bits), value);
}

ConstantFP *Module::get_constant_fp(FloatType *type, double value)
{
    if (auto it = constant_fps_.find({type, value}); it != constant_fps_.end())
    {
        return it->second.get();
    }

    auto &constant = constant_fps_[{type, value}];
    constant = std::unique_ptr<ConstantFP>(new ConstantFP(type, value));
    return constant.get();
}

ConstantFP *Module::get_constant_fp(FloatType::Precision precision, double value)
{
    return get_constant_fp(get_float_type(precision), value);
}

//===----------------------------------------------------------------------===//
//                            ConstantInt Implementation
//===----------------------------------------------------------------------===//
ConstantInt::ConstantInt(IntegerType *type, uint64_t value)
    : Constant(type, ""), value_(value) {}

ConstantInt *ConstantInt::get(Module *m, IntegerType *type, uint64_t value)
{
    return m->get_constant_int(type, value);
}

ConstantInt *ConstantInt::zext_value(Module *m, IntegerType *dest_type) const
{
    assert(dest_type->bits() > type()->bits());
    return ConstantInt::get(m, dest_type, value_);
}

ConstantInt *ConstantInt::sext_value(Module *m, IntegerType *dest_type) const
{
    assert(dest_type->bits() > type()->bits());
    const uint64_t sign_bit = 1ULL << (type()->bits() - 1);
    const uint64_t sign_extended = (value_ & sign_bit) ? (value_ | ~((1ULL << type()->bits()) - 1)) : value_;
    return ConstantInt::get(m, dest_type, sign_extended);
}

std::string ConstantInt::as_string() const
{
    if (type()->bits() == 1)
    {
        return value_ ? "true" : "false";
    }
    return std::to_string(value_);
}

//===----------------------------------------------------------------------===//
//                            ConstantFP Implementation
//===----------------------------------------------------------------------===//
std::string ConstantFP::as_string() const
{
    std::stringstream ss;
    ss << type()->name() << " " << std::fixed << value_;
    return ss.str();
}

ConstantFP::ConstantFP(FloatType *type, double value)
    : Constant(type, ""), value_(value) {}

ConstantFP *ConstantFP::get(Module *m, FloatType *type, double value)
{
    return m->get_constant_fp(type, value);
}

ConstantFP *ConstantFP::get(Module *m, FloatType::Precision precision, double value)
{
    return m->get_constant_fp(m->get_float_type(precision), value);
}

std::string ConstantArray::as_string() const
{
    std::stringstream ss;
    ss << type()->name() << " [";
    for (size_t i = 0; i < elements_.size(); ++i)
    {
        if (i > 0)
            ss << ", ";
        ss << elements_[i]->as_string();
    }
    ss << "]";
    return ss.str();
}

// ---------- ConstantStruct ----------
std::string ConstantStruct::as_string() const
{
    std::stringstream ss;
    ss << type()->name() << " {";
    for (size_t i = 0; i < members_.size(); ++i)
    {
        if (i > 0)
            ss << ", ";
        ss << members_[i]->as_string();
    }
    ss << "}";
    return ss.str();
}

// ---------- GlobalVariable ----------
std::string GlobalVariable::as_string() const
{
    return "@" + name() + " = " +
           (is_constant_ ? "constant " : "global ") +
           initializer_->as_string();
}

// ---------- ConstantPointerNull ----------
std::string ConstantPointerNull::as_string() const
{
    return type()->name() + " null";
}

// ---------- ConstantAggregateZero ----------
std::string ConstantAggregateZero::as_string() const
{
    return type()->name() + " zeroinitializer";
}

//===----------------------------------------------------------------------===//
//                           Instruction Subclass Implementations
//===----------------------------------------------------------------------===//
BranchInst::BranchInst(BasicBlock *target, BasicBlock *parent,
                       std::vector<Value *> ops)
    : Instruction(Opcode::Br, Type::get_void_type(parent->parent()->parent()),
                  parent, ops),
      true_bb_(target),
      false_bb_(nullptr) {}

BranchInst *BranchInst::create(BasicBlock *target, BasicBlock *parent)
{
    std::vector<Value *> ops;
    auto *inst = new BranchInst(target, parent, ops);
    parent->add_successor(target);
    return inst;
}

BranchInst *BranchInst::create_cond(Value *cond, BasicBlock *true_bb,
                                    BasicBlock *false_bb, BasicBlock *parent)
{
    std::vector<Value *> ops{cond};
    auto *inst = new BranchInst(true_bb, parent, ops);
    parent->add_successor(true_bb);
    parent->add_successor(false_bb);
    inst->false_bb_ = false_bb;
    return inst;
}

BasicBlock *BranchInst::get_true_successor() const
{
    return true_bb_;
}

BasicBlock *BranchInst::get_false_successor() const
{
    return false_bb_;
}

ReturnInst::ReturnInst(Value *value, BasicBlock *parent)
    : Instruction(Opcode::Ret, Type::get_void_type(parent->parent()->parent()),
                  parent, {value}) {}

ReturnInst *ReturnInst::create(Value *value, BasicBlock *parent)
{
    return new ReturnInst(value, parent);
}

PhiInst::PhiInst(Type *type, BasicBlock *parent)
    : Instruction(Opcode::Phi, type, parent, {}) {}

PhiInst *PhiInst::create(Type *type, BasicBlock *parent)
{
    return new PhiInst(type, parent);
}

void PhiInst::add_incoming(Value *val, BasicBlock *bb)
{
    operands_.push_back(val);
    operands_.push_back(bb);
    val->add_user(this);
}

BasicBlock *PhiInst::get_incoming_block(unsigned i) const
{
    return static_cast<BasicBlock *>(operands_[2 * i + 1]);
}

ICmpInst::ICmpInst(BasicBlock *parent, std::vector<Value *> ops)
    : Instruction(Opcode::ICmp, IntegerType::get(parent->parent()->parent(), 1),
                  parent, ops),
      pred_(EQ) {}

ICmpInst *ICmpInst::create(Predicate pred, Value *lhs, Value *rhs,
                           BasicBlock *parent)
{
    std::vector<Value *> ops{lhs, rhs};
    auto *inst = new ICmpInst(parent, ops);
    inst->pred_ = pred;
    return inst;
}

FCmpInst::FCmpInst(Predicate pred, Type *type, BasicBlock *parent, std::vector<Value *> operands, const std::string &name)
    : Instruction(Opcode::FCmp, type, parent, operands, name), pred_(pred) {}

FCmpInst *FCmpInst::create(Predicate pred, Value *lhs, Value *rhs, BasicBlock *parent, const std::string &name)
{
    return new FCmpInst(pred, lhs->type(), parent, {lhs, rhs}, name);
}

AllocaInst::AllocaInst(Type *allocated_type, Type *ptr_type, BasicBlock *parent)
    : Instruction(Opcode::Alloca, ptr_type, parent, {}),
      allocated_type_(allocated_type) {}

AllocaInst *AllocaInst::create(Type *allocated_type, BasicBlock *parent,
                               const std::string &name)
{
    Type *ptr_type = PointerType::get(parent->parent()->parent(), allocated_type);
    auto *inst = new AllocaInst(allocated_type, ptr_type, parent);
    inst->set_name(name); // TODO: set by intializer?
    return inst;
}

LoadInst::LoadInst(Type *loaded_type, BasicBlock *parent, Value *ptr)
    : Instruction(Opcode::Load, loaded_type, parent, {ptr}) {}

LoadInst *LoadInst::create(Value *ptr, BasicBlock *parent,
                           const std::string &name)
{
    auto *ptr_type = static_cast<PointerType *>(ptr->type());
    auto *inst = new LoadInst(ptr_type->element_type(), parent, ptr);
    inst->set_name(name);
    return inst;
}

StoreInst::StoreInst(BasicBlock *parent, Value *value, Value *ptr)
    : Instruction(Opcode::Store, Type::get_void_type(parent->parent()->parent()),
                  parent, {value, ptr}) {}

StoreInst *StoreInst::create(Value *value, Value *ptr, BasicBlock *parent)
{
    auto *inst = new StoreInst(parent, value, ptr);
    return inst;
}

GetElementPtrInst::GetElementPtrInst(Type *result_type, BasicBlock *parent,
                                     Value *ptr, std::vector<Value *> indices)
    : Instruction(Opcode::GetElementPtr, result_type, parent, {ptr})
{
    operands_.insert(operands_.end(), indices.begin(), indices.end());
}

const std::vector<Value *> GetElementPtrInst::indices() const
{
    return std::vector<Value *>(operands_.begin() + 1, operands_.end());
}

Type *GetElementPtrInst::get_result_type(Type *base_type,
                                         const std::vector<Value *> &indices)
{
    Type *current_type = base_type;

    for (size_t i = 0; i < indices.size(); ++i)
    {
        // First, convert to pointer type
        if (auto *ptr_ty = dynamic_cast<PointerType *>(current_type))
        {
            current_type = ptr_ty->element_type();
            continue;
        }

        // Handle array type
        if (auto *array_ty = dynamic_cast<ArrayType *>(current_type))
        {
            current_type = array_ty->element_type();
            continue;
        }

        // Handle vector type
        if (auto *vector_ty = dynamic_cast<VectorType *>(current_type))
        {
            current_type = vector_ty->element_type();
            continue;
        }

        // Handle struct type
        if (auto *struct_ty = dynamic_cast<StructType *>(current_type))
        {
            auto *index_val = dynamic_cast<ConstantInt *>(indices[i]);
            assert(index_val && "Struct indices must be constants");
            unsigned idx = index_val->value();
            assert(idx < struct_ty->members().size() && "Struct index out of bounds");
            current_type = struct_ty->get_member_type(idx);
            continue;
        }

        // Handle function type
        if (auto *func_ty = dynamic_cast<FunctionType *>(current_type))
        {
            (void)func_ty;
            assert(false && "Cannot index into a function type");
            continue;
        }

        assert(false && "Invalid GEP index sequence");
    }

    return PointerType::get(base_type->module(), current_type);
}

GetElementPtrInst *GetElementPtrInst::create(Value *ptr, std::vector<Value *> indices,
                                             BasicBlock *parent,
                                             const std::string &name)
{
    Type *result_type = get_result_type(ptr->type(), indices);
    auto *inst = new GetElementPtrInst(result_type, parent, ptr, indices);
    inst->set_name(name);
    return inst;
}

BinaryInst::BinaryInst(Opcode op, Type *type, BasicBlock *parent, std::vector<Value *> operands, const std::string &name)
    : Instruction(op, type, parent, operands, name) {}

bool BinaryInst::isBinaryOp(Opcode op)
{
    return op == Opcode::Add || op == Opcode::Sub || op == Opcode::Mul ||
           op == Opcode::UDiv || op == Opcode::SDiv;
}

BinaryInst *BinaryInst::create(Opcode op, Value *lhs, Value *rhs, BasicBlock *parent, const std::string &name)
{
    assert(isBinaryOp(op) && "Invalid binary opcode");
    return new BinaryInst(op, lhs->type(), parent, {lhs, rhs}, name);
}

ConversionInst::ConversionInst(Opcode op, Type *dest_type, BasicBlock *parent, std::vector<Value *> operands, const std::string &name)
    : Instruction(op, dest_type, parent, operands, name) {}

bool ConversionInst::isConversionOp(Opcode op)
{
    return op == Opcode::ZExt || op == Opcode::SExt || op == Opcode::Trunc;
}

ConversionInst *ConversionInst::create(Opcode op, Value *val, Type *dest_type, BasicBlock *parent, const std::string &name)
{
    assert(isConversionOp(op) && "Invalid conversion opcode");
    return new ConversionInst(op, dest_type, parent, {val}, name);
}
