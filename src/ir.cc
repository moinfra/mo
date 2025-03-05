#include "ir.h"

#include <sstream>
#include <iomanip>
#include <limits>

#include "utils.h"

//===----------------------------------------------------------------------===//
//                              Type Implementation
//===----------------------------------------------------------------------===//
Type *Type::get_void_type(Module *m)
{
    return m->get_void_type();
}

bool Type::is_tuple() const { return tid_ == StructTy && as_struct()->is_tuple(); }

Type *Type::element_type() const
{
    if (is_array())
    {
        return dynamic_cast<const ArrayType *>(this)->element_type();
    }
    else if (is_vector())
    {
        return dynamic_cast<const VectorType *>(this)->element_type();
    }
    else if (is_pointer())
    {
        return dynamic_cast<const PointerType *>(this)->element_type();
    }
    else
    {
        std::cerr << "Type::element_type() called on non-aggregate type: " << Type::id_to_str(tid_) << std::endl;
        return nullptr;
    }
}

IntegerType::IntegerType(Module *m, unsigned bits, bool unsigned_)
    : NumericType(IntTy, m), bits_(bits), unsigned_(unsigned_) {}

FloatType::FloatType(Module *m, FloatType::Precision precision)
    : NumericType(FpTy, m), precision_(precision)
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

PointerType::PointerType(Module *m, Type *element_type)
    : Type(PtrTy, m), element_type_(element_type) {}

ArrayType::ArrayType(Module *m, Type *element_type, uint64_t num_elements)
    : AggregateType(ArrayTy, m), element_type_(element_type),
      num_elements_(num_elements)
{
    assert(element_type && "Invalid element type");
}

size_t ArrayType::size() const
{
    assert(element_type_ && "Invalid element type");
    return element_type_->size() * num_elements_;
}

StructLayout calculate_aligned_layout(const std::vector<Type *> &members)
{
    StructLayout layout;
    size_t offset = 0;
    size_t max_alignment = 1;

    for (auto *member_type : members)
    {
        size_t alignment = member_type->alignment();

        if (alignment > max_alignment)
        {
            max_alignment = alignment;
        }

        if (offset % alignment != 0)
        {
            offset += alignment - (offset % alignment);
        }

        // 记录成员的偏移量
        layout.members.push_back({member_type, offset});

        // 更新偏移量
        offset += member_type->size();
    }

    if (offset % max_alignment != 0)
    {
        offset += max_alignment - (offset % max_alignment);
    }

    layout.size = offset;
    layout.alignment = max_alignment;

    return layout;
}
StructType::StructType(Module *m, const std::string &name, const std::vector<MemberInfo> &members)
    : AggregateType(StructTy, m), name_(name), module_(m), is_opaque_(false), size_(0)
{
    if (!members.empty())
    {
        set_body(members);
    }
}

StructType::StructType(Module *m, const std::vector<MemberInfo> &members)
    : AggregateType(StructTy, m), name_(""), module_(m), is_opaque_(members.empty()), size_(0)
{
    if (!members.empty())
    {
        set_body(members);
    }
}

void StructType::set_body(const std::vector<MemberInfo> &members)
{
    assert(members_.empty() && "Struct already has body");
    members_ = members;
    is_opaque_ = false;

    // 提取成员类型以计算布局
    std::vector<Type *> member_types;
    for (const auto &member : members)
    {
        member_types.push_back(member.type);
    }

    // 计算对齐布局
    StructLayout layout = calculate_aligned_layout(member_types);
    size_ = layout.size;
    offsets_.clear();
    for (const auto &offset_entry : layout.members)
    { // 假设 layout.members 包含偏移信息
        offsets_.push_back(offset_entry.offset);
    }
}

Type *StructType::get_member_type(unsigned index) const
{
    assert(index < members_.size() && "Invalid member index");
    return members_[index].type;
}

size_t StructType::get_member_offset(unsigned index) const
{
    assert(index < members_.size() && "Invalid member index");
    return offsets_[index];
}

size_t StructType::get_member_index(const std::string &name) const
{
    for (size_t i = 0; i < members_.size(); ++i)
    {
        MO_DEBUG("StructType::get_member_index: %s, %s", members_[i].name.c_str(), name.c_str());
        if (members_[i].name == name)
        {
            return i;
        }
    }

    MO_ASSERT(false, "Invalid struct member name %s", name.c_str());
    return std::numeric_limits<size_t>::max();
}

bool StructType::has_member(const std::string &name) const
{
    for (const auto &member : members_)
    {
        if (member.name == name)
        {
            return true;
        }
    }
    return false;
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
    MO_ASSERT(parent != nullptr, "Parent block is null");
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
    : Value(parent->parent_module()->get_void_type(), name),
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

Instruction *BasicBlock::first_non_phi() const
{
    Instruction *inst = head_;
    while (inst && inst->opcode() == Opcode::Phi)
    {
        inst = inst->next();
    }
    return inst;
}

Instruction *BasicBlock::last_non_phi() const
{
    Instruction *inst = tail_;
    while (inst && inst->opcode() == Opcode::Phi)
    {
        inst = inst->prev();
    }
    return inst;
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

Instruction *BasicBlock::get_terminator() const
{
    if (!tail_)
    {
        return nullptr;
    }

    // 检查最后一条指令的类型
    switch (tail_->opcode())
    {
    case Opcode::Ret:
    case Opcode::Br:
    case Opcode::CondBr:
    case Opcode::Unreachable:
    {
        return tail_;
    }
    default:
        return nullptr;
    }
}

//===----------------------------------------------------------------------===//
//                             Function Implementation
//===----------------------------------------------------------------------===//
Function::Function(const std::string &name, Module *parent, Type *return_type,
                   const ParamList &params)
    : Value(parent->get_function_type(return_type, param_list_to_types(params)), name),
      parent_(parent),
      return_type_(return_type)
{
    assert(return_type && "Invalid return type");

    // Create arguments
    for (const auto &[param_name, param_type] : params)
    {
        arguments_.emplace_back(
            std::make_unique<Argument>(param_name, param_type, this));
        args_.push_back(arguments_.back().get());
    }
}

bool Function::remove_basic_block(BasicBlock *bb)
{
    auto it = std::find(basic_block_ptrs_.begin(), basic_block_ptrs_.end(), bb);
    if (it != basic_block_ptrs_.end())
    {
        basic_block_ptrs_.erase(it);
        return true;
    }

    return false;
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
Module::Module(std::string name)
    : name_(std::move(name))
{
    void_type_ = std::unique_ptr<VoidType>(new VoidType(this));
}

Module::~Module() = default;

Function *Module::create_function(
    const std::string &name,
    Type *return_type,
    const std::vector<std::pair<std::string, Type *>> &params)
{
    functions_.push_back(
        std::make_unique<Function>(name, this, return_type, params));
    return functions_.back().get();
}

Function *Module::create_function(
    const std::string &name,
    FunctionType *type)
{
    auto params = type->params();
    functions_.push_back(
        std::make_unique<Function>(name, this, type->return_type(), params));
    return functions_.back().get();
}

GlobalVariable *Module::create_global_variable(Type *type, bool is_constant, Constant *initializer, const std::string &name)
{
    auto *gv_ptr = new GlobalVariable(type, is_constant, initializer, name);
    auto gv = std::unique_ptr<GlobalVariable>(gv_ptr);
    global_variables_.push_back(std::move(gv));
    return gv_ptr;
}

IntegerType *Module::get_integer_type(unsigned bits, bool unsigned_)
{
    auto &type = integer_types_[bits];
    if (!type)
    {
        type = std::unique_ptr<IntegerType>(new IntegerType(this, bits, unsigned_));
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
    assert(element_type && "Invalid element type");

    auto &type = pointer_types_[element_type];
    if (!type)
    {
        type = std::unique_ptr<PointerType>(new PointerType(this, element_type));
    }
    return type.get();
}

FunctionType *Module::get_function_type(Type *return_type, const std::vector<Type *> &param_types)
{
    assert(return_type && "Invalid return type");

    auto key = std::make_pair(return_type, param_types);
    auto it = function_types_.find(key);
    if (it != function_types_.end())
    {
        return it->second.get();
    }

    ParamList params;
    params.reserve(param_types.size());
    for (auto *param_type : param_types)
    {
        params.push_back({"", param_type});
    }
    auto function_type = std::unique_ptr<FunctionType>(new FunctionType(this, return_type, params));
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
    const auto key = std::make_pair(element_type, num_elements);
    auto it = array_types_.find(key);

    if (it != array_types_.end())
    {
        return it->second.get();
    }

    auto ty = new ArrayType(this, element_type, num_elements);
    array_types_.emplace(key, std::unique_ptr<ArrayType>(ty));
    return ty;
}

// FIXME: should not get by members
StructType *Module::get_struct_type_anonymous(const std::vector<MemberInfo> &members)
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
    auto *st = new StructType(this, members);
    struct_types_.push_back(std::unique_ptr<StructType>(st));
    return st;
}

StructType *Module::try_get_struct_type(const std::string &name)
{
    assert(!name.empty() && "Invalid struct name");

    // Check existing struct types
    for (auto &st : struct_types_)
    {
        if (st->name() == name)
        {
            return st.get();
        }
    }
    // Check opaque types
    if (auto it = opaque_structs_.find(name); it != opaque_structs_.end())
    {
        return it->second.get();
    }
    return nullptr;
}

StructType *Module::get_struct_type(const std::string &name, const std::vector<MemberInfo> &members)
{
    // Check existing struct types
    if (auto st = try_get_struct_type(name); st)
    {
        // Check if the struct has the same members
        if (st->members() != members)
        {
            assert(false && "Struct with the same name already exists");
        }
        return st;
    }

    // Create new struct
    auto *st = new StructType(this, name, members);
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

static bool is_supported_bits(unsigned bits)
{
    return bits == 1 || bits == 8 || bits == 16 || bits == 32 || bits == 64;
}

static uint64_t truncate_value(uint64_t value, unsigned bits, bool is_signed)
{
    const unsigned mask = (bits == 64) ? 0xFFFFFFFFFFFFFFFF : (1ULL << bits) - 1;
    value &= mask; // truncate high bits

    if (is_signed && bits > 1)
    {
        // sign ext: if the most significant bit is 1 and signed, convert it to supplement form
        const uint64_t sign_bit = 1ULL << (bits - 1);
        if (value & sign_bit)
        {
            value |= ~mask; // prevent shift overflow for 64-bit values
        }
    }
    return value;
}

ConstantInt *Module::get_constant_int(IntegerType *type, uint64_t value)
{
    MO_ASSERT(type != nullptr, "Invalid integer type");

    const bool is_signed = type->is_signed();
    const uint64_t processed_value = truncate_value(value, type->bits(), is_signed);

    const auto key = std::make_pair(type, processed_value);
    if (auto it = constant_ints_.find(key); it != constant_ints_.end())
    {
        return it->second.get();
    }

    auto result = constant_ints_.emplace(
        key,
        std::unique_ptr<ConstantInt>(new ConstantInt(type, processed_value)));

    return result.first->second.get();
}

ConstantInt *Module::get_constant_int(unsigned bits, uint64_t value, bool unsigned_)
{
    MO_ASSERT(is_supported_bits(bits), "Bits must be 1/8/16/32/64");
    return get_constant_int(get_integer_type(bits, unsigned_), value);
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

ConstantString *Module::get_constant_string(std::string value)
{
    for (auto &constant : constant_strings_)
    {
        if (constant->value() == value)
        {
            return constant.get();
        }
    }

    auto &constant = constant_strings_.emplace_back(
        std::unique_ptr<ConstantString>(new ConstantString(get_array_type(get_integer_type(8), value.size() + 1), value)));
    return constant.get();
}

Constant *Module::get_constant_zero(Type *type)
{
    if (type->is_pointer())
    {
        // return ConstantPointerNull::get(cast<PointerType>(type));
        return get_constant_pointer_null(type->as_pointer());
    }
    else if (type->is_aggregate() || type->is_vector())
    {
        return get_constant_aggregate_zero(type);
    }
    else if (type->is_integer())
    {
        return get_constant_int(type->as_integer(), 0);
    }
    else if (type->is_float())
    {
        return get_constant_fp(type->as_float(), 0.0);
    }

    MO_UNREACHABLE();
}

ConstantAggregateZero *Module::get_constant_aggregate_zero(Type *type)
{
    for (auto &constant : constant_aggregate_zeros_)
    {
        if (*constant->type() == *type)
        {
            return constant.get();
        }
    }

    auto &constant = constant_aggregate_zeros_.emplace_back(
        std::unique_ptr<ConstantAggregateZero>(new ConstantAggregateZero(type)));
    return constant.get();
}

ConstantPointerNull *Module::get_constant_pointer_null(PointerType *type)
{
    for (auto &constant : constant_pointer_nulls_)
    {
        if (*constant->type() == *type)
        {
            return constant.get();
        }
    }

    auto &constant = constant_pointer_nulls_.emplace_back(
        std::unique_ptr<ConstantPointerNull>(new ConstantPointerNull(type)));
    return constant.get();
}

ConstantStruct *Module::get_constant_struct(StructType *type, const std::vector<Constant *> &members)
{
    for (auto &constant : constant_structs_)
    {
        if (*constant->type() == *type && constant->members() == members)
        {
            return constant.get();
        }
    }

    auto &constant = constant_structs_.emplace_back(
        std::unique_ptr<ConstantStruct>(new ConstantStruct(type, members)));
    return constant.get();
}

ConstantArray *Module::get_constant_array(ArrayType *type, const std::vector<Constant *> &elements)
{
    for (auto &constant : constant_arrays_)
    {
        if (*constant->type() == *type && constant->elements() == elements)
        {
            return constant.get();
        }
    }

    auto &constant = constant_arrays_.emplace_back(
        std::unique_ptr<ConstantArray>(new ConstantArray(type, elements)));
    return constant.get();
}

//===----------------------------------------------------------------------===//
//                            ConstantInt Implementation
//===----------------------------------------------------------------------===//
ConstantInt::ConstantInt(IntegerType *type, uint64_t value)
    : Constant(type, ""), value_(value) {}

ConstantInt *ConstantInt::zext_value(Module *m, IntegerType *dest_type) const
{
    assert(dest_type->bits() > type()->bits());
    return m->get_constant_int(dest_type, value_);
}

ConstantInt *ConstantInt::sext_value(Module *m, IntegerType *dest_type) const
{
    assert(dest_type->bits() > type()->bits());
    const uint64_t sign_bit = 1ULL << (type()->bits() - 1);
    const uint64_t sign_extended = (value_ & sign_bit) ? (value_ | ~((1ULL << type()->bits()) - 1)) : value_;
    return m->get_constant_int(dest_type, sign_extended);
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

//===----------------------------------------------------------------------===//
//                            ConstantArray Implementation
//===----------------------------------------------------------------------===//
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

//===----------------------------------------------------------------------===//
//                            ConstantString Implementation
//===----------------------------------------------------------------------===//
ConstantString::ConstantString(ArrayType *type, const std::string &value)
    : Constant(type), value_(value) {}

const std::string &ConstantString::value() const
{
    return value_;
}

std::string ConstantString::as_string() const
{
    return "c\"" + escape_string(value_) + "\\00\"";
}

std::string ConstantString::escape_string(const std::string &input)
{
    std::stringstream ss;
    for (char c : input)
    {
        if (c == '\\')
        {
            ss << "\\5C";
        }
        else if (c == '"')
        {
            ss << "\\22";
        }
        else if (c >= 0x20 && c <= 0x7E)
        {
            // Most printable characters are not escaped
            ss << c;
        }
        else
        {
            // Escape non-printable characters as \HH
            ss << "\\" << std::hex << std::setw(2) << std::setfill('0')
               << static_cast<unsigned int>(static_cast<unsigned char>(c));
        }
    }
    return ss.str();
}

//===----------------------------------------------------------------------===//
//                            ConstantStruct Implementation
//===----------------------------------------------------------------------===//
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

//===----------------------------------------------------------------------===//
//                            GlobalVariable Implementation
//===----------------------------------------------------------------------===//
std::string GlobalVariable::as_string() const
{
    return "@" + name();
    //  + " = " + (is_constant_ ? "constant " : "global ") + initializer_->as_string();
}

//===----------------------------------------------------------------------===//
//                            ConstantPointerNull Implementation
//===----------------------------------------------------------------------===//
std::string ConstantPointerNull::as_string() const
{
    return type()->name() + " null";
}

//===----------------------------------------------------------------------===//
//                            ConstantAggregateZero Implementation
//===----------------------------------------------------------------------===//
std::string ConstantAggregateZero::as_string() const
{
    return type()->name() + " zeroinitializer";
}

//===----------------------------------------------------------------------===//
//                            ConstantAggregate Implementation
//===----------------------------------------------------------------------===//
std::string ConstantAggregate::as_string() const
{
    std::stringstream ss;
    ss << type()->name() << " {";
    for (size_t i = 0; i < elements_.size(); ++i)
    {
        if (i > 0)
            ss << ", ";
        ss << elements_[i]->as_string();
    }
    ss << "}";
    return ss.str();
}

//===----------------------------------------------------------------------===//
//                           Instruction Subclass Implementations
//===----------------------------------------------------------------------===//

//____________________________________________________________________________
//                           BranchInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
BranchInst::BranchInst(BasicBlock *target, BasicBlock *parent,
                       std::vector<Value *> ops)
    : Instruction(Opcode::Br, Type::get_void_type(parent->parent_function()->parent_module()),
                  parent, ops),
      true_bb_(target),
      false_bb_(nullptr) {}

BranchInst *BranchInst::create(BasicBlock *target, BasicBlock *parent)
{
    std::vector<Value *> ops;
    MO_ASSERT(target != nullptr, "Invalid target block");
    MO_ASSERT(parent != nullptr, "Invalid parent block");
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

//____________________________________________________________________________
//                           ReturnInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
ReturnInst::ReturnInst(Value *value, BasicBlock *parent)
    : Instruction(Opcode::Ret, Type::get_void_type(parent->parent_function()->parent_module()),
                  parent, {value}) {}

ReturnInst *ReturnInst::create(Value *value, BasicBlock *parent)
{
    return new ReturnInst(value, parent);
}

//____________________________________________________________________________
//                           UnreachableInst  Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
UnreachableInst::UnreachableInst(BasicBlock *parent)
    : Instruction(Opcode::Unreachable, Type::get_void_type(parent->parent_function()->parent_module()),
                  parent, {}) {}

UnreachableInst *UnreachableInst::create(BasicBlock *parent)
{
    return new UnreachableInst(parent);
}

//____________________________________________________________________________
//                           PhiInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
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

//____________________________________________________________________________
//                           ICmpInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
ICmpInst::ICmpInst(BasicBlock *parent, std::vector<Value *> ops)
    : Instruction(Opcode::ICmp, parent->parent_function()->parent_module()->get_integer_type(1),
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

//____________________________________________________________________________
//                           FCmpInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
FCmpInst::FCmpInst(Predicate pred, BasicBlock *parent, std::vector<Value *> operands, const std::string &name)
    : Instruction(Opcode::FCmp, parent->parent_function()->parent_module()->get_integer_type(1), parent, operands, name), pred_(pred) {}

FCmpInst *FCmpInst::create(Predicate pred, Value *lhs, Value *rhs, BasicBlock *parent, const std::string &name)
{
    return new FCmpInst(pred, parent, {lhs, rhs}, name);
}

//____________________________________________________________________________
//                           AllocaInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
AllocaInst::AllocaInst(Type *allocated_type, Type *ptr_type, BasicBlock *parent)
    : Instruction(Opcode::Alloca, ptr_type, parent, {}),
      allocated_type_(allocated_type) {}

AllocaInst *AllocaInst::create(Type *allocated_type, BasicBlock *parent,
                               const std::string &name)
{
    Type *ptr_type = parent->parent_function()->parent_module()->get_pointer_type(allocated_type);
    auto *inst = new AllocaInst(allocated_type, ptr_type, parent);
    inst->set_name(name); // TODO: set by intializer?
    return inst;
}

//____________________________________________________________________________
//                           LoadInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
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

//____________________________________________________________________________
//                           StoreInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
StoreInst::StoreInst(BasicBlock *parent, Value *value, Value *ptr)
    : Instruction(Opcode::Store, Type::get_void_type(parent->parent_function()->parent_module()),
                  parent, {value, ptr}) {}

StoreInst *StoreInst::create(Value *value, Value *ptr, BasicBlock *parent)
{

    auto *inst = new StoreInst(parent, value, ptr);
    return inst;
}

//____________________________________________________________________________
//                           GetElementPtrInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
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
            MO_ASSERT(idx < struct_ty->members().size(), "Struct index %u out of bounds %zu", idx, struct_ty->members().size());
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

    return base_type->module()->get_pointer_type(current_type);
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

//____________________________________________________________________________
//                           BinaryInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
BinaryInst::BinaryInst(Opcode op, Type *type, BasicBlock *parent, std::vector<Value *> operands, const std::string &name)
    : Instruction(op, type, parent, operands, name) {}

bool BinaryInst::isBinaryOp(Opcode op)
{
    switch (op)
    {
    case Opcode::Add:
    case Opcode::Sub:
    case Opcode::Mul:
    case Opcode::UDiv:
    case Opcode::SDiv:
    case Opcode::URem:
    case Opcode::SRem:
    case Opcode::BitAnd:
    case Opcode::BitOr:
    case Opcode::BitXor:
    case Opcode::Shl:
    case Opcode::LShr:
    case Opcode::AShr:
    case Opcode::ICmp:
    case Opcode::FCmp:
        return true;
    default:
        return false;
    }
}

BinaryInst *BinaryInst::create(Opcode op, Value *lhs, Value *rhs, BasicBlock *parent, const std::string &name)
{
    assert(isBinaryOp(op) && "Invalid binary opcode");
    return new BinaryInst(op, lhs->type(), parent, {lhs, rhs}, name);
}

//____________________________________________________________________________
//                           ConversionInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
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

//____________________________________________________________________________
//                           CastInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
CastInst::CastInst(Opcode op, Type *target_type, BasicBlock *parent,
                   std::initializer_list<Value *> operands,
                   const std::string &name)
    : Instruction(op, target_type, parent, operands, name) {}

//____________________________________________________________________________
//                           BitCastInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
BitCastInst::BitCastInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    // : Instruction(Opcode::BitCast, target_type, parent, {val}, name) {}
    : CastInst(Opcode::BitCast, target_type, parent, {val}, name)
{
}

BitCastInst *BitCastInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new BitCastInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           PtrToIntInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
PtrToIntInst *PtrToIntInst::create(Value *ptr, Type *target_type, BasicBlock *parent, const std::string &name)
{
    assert(ptr->type()->is_pointer() && "Source value must be a pointer type");
    assert(target_type->is_integer() && "Target type must be an integer type");
    return new PtrToIntInst(parent, ptr, target_type, name);
}

//____________________________________________________________________________
//                           CallInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
CallInst::CallInst(BasicBlock *parent, Value *callee, Type *return_type, const std::vector<Value *> &args, const std::string &name)
    : Instruction(Opcode::Call, return_type, parent, create_operand_list(callee, args), name) {}

CallInst *CallInst::create(Value *callee, Type *return_type, const std::vector<Value *> &args, BasicBlock *parent, const std::string &name)
{
    return new CallInst(parent, callee, return_type, args, name);
}
CallInst *CallInst::create(Function *callee, const std::vector<Value *> &args, BasicBlock *parent, const std::string &name)
{
    auto return_type = callee->return_type();
    return new CallInst(parent, callee, return_type, args, name);
}

std::vector<Value *> CallInst::create_operand_list(Value *callee, const std::vector<Value *> &args)
{
    std::vector<Value *> operands;
    operands.reserve(args.size() + 1);
    operands.push_back(callee);
    operands.insert(operands.end(), args.begin(), args.end());
    return operands;
}

std::vector<Value *> CallInst::arguments() const
{
    std::vector<Value *> args;
    for (size_t i = 1; i < this->operands().size(); ++i)
    {
        args.push_back(this->operand(i));
    }
    return args;
}

//____________________________________________________________________________
//                           SExtInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
SExtInst::SExtInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    : CastInst(Opcode::SExt, target_type, parent, {val}, name)
{
}

SExtInst *SExtInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new SExtInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           ZExtInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
ZExtInst::ZExtInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    : CastInst(Opcode::ZExt, target_type, parent, {val}, name)
{
}

ZExtInst *ZExtInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new ZExtInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           TruncInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
TruncInst::TruncInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    // : Instruction(Opcode::Trunc, target_type, parent, {val}, name) {}
    : CastInst(Opcode::Trunc, target_type, parent, {val}, name)
{
}

TruncInst *TruncInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new TruncInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           SIToFPInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
SIToFPInst::SIToFPInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    : CastInst(Opcode::SIToFP, target_type, parent, {val}, name) {}

SIToFPInst *SIToFPInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new SIToFPInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           FPToSIInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
FPToSIInst::FPToSIInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    : CastInst(Opcode::FPToSI, target_type, parent, {val}, name) {}

FPToSIInst *FPToSIInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new FPToSIInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           FPExtInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
FPExtInst::FPExtInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    : CastInst(Opcode::FPExt, target_type, parent, {val}, name) {}

FPExtInst *FPExtInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new FPExtInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           FPTruncInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
FPTruncInst::FPTruncInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    : CastInst(Opcode::FPTrunc, target_type, parent, {val}, name) {}

FPTruncInst *FPTruncInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new FPTruncInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           IntToPtrInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
IntToPtrInst::IntToPtrInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    : CastInst(Opcode::IntToPtr, target_type, parent, {val}, name)
{
    assert(val->type()->is_integer() && "Source value must be an integer type");
    assert(target_type->is_pointer() && "Target type must be a pointer type");
}

IntToPtrInst *IntToPtrInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new IntToPtrInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           FPToUIInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
FPToUIInst::FPToUIInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    : CastInst(Opcode::FPToUI, target_type, parent, {val}, name)
{
    assert(val->type()->is_float() && "Source value must be a floating point type");
    assert(target_type->is_integer() && "Target type must be an integer type");
}

FPToUIInst *FPToUIInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new FPToUIInst(parent, val, target_type, name);
}

//____________________________________________________________________________
//                           UIToFPInst Implementations
// ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
UIToFPInst::UIToFPInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name)
    : CastInst(Opcode::UIToFP, target_type, parent, {val}, name)
{
    assert(val->type()->is_integer() && "Source value must be an integer type");
    assert(target_type->is_float() && "Target type must be a floating point type");
}

UIToFPInst *UIToFPInst::create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name)
{
    return new UIToFPInst(parent, val, target_type, name);
}
