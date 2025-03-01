// ir.h - SSA IR Types and Values

#pragma once

#include <iostream>
#include <vector>
#include <cstring>
#include <type_traits>
#include <memory>
#include <string>
#include <cassert>
#include <unordered_map>
#include <algorithm>

// Forward declarations
class Type;
class Value;
class User;
class Function;
class GlobalVariable;
class BasicBlock;
class Instruction;
class Constant;
class Module;
class BranchInst;
class PhiInst;
class ICmpInst;
class AllocaInst;
class LoadInst;
class StoreInst;
class GetElementPtrInst;
class IRBuilder;
class ConstantInt;
class ConstantFP;
class IntegerType;
class VoidType;
class PointerType;
class ArrayType;
class StructType;
class ConstantString;
class ConstantPointerNull;
class ConstantAggregateZero;
class ConstantStruct;
class ConstantArray;

//===----------------------------------------------------------------------===//
//                              Hash function specialization
//===----------------------------------------------------------------------===//
namespace std
{
    template <>
    struct hash<std::pair<Type *, uint64_t>>
    {
        size_t operator()(const std::pair<Type *, uint64_t> &p) const
        {
            return hash<Type *>{}(p.first) ^ (hash<uint64_t>{}(p.second) << 1);
        }
    };
}

//===----------------------------------------------------------------------===//
//                               Type System
//===----------------------------------------------------------------------===//
using ParamList = std::vector<std::pair<std::string, Type *>>;

class Type
{
public:
    enum TypeID
    {
        VoidTy,
        IntTy,
        FpTy,
        PtrTy,
        FuncTy,
        ArrayTy,
        StructTy,
        VecTy,
    };
    static const char *id_to_str(TypeID id)
    {
        switch (id)
        {
        case VoidTy:
            return "void";
        case IntTy:
            return "i";
        case FpTy:
            return "f";
        case PtrTy:
            return "ptr";
        case FuncTy:
            return "func";
        case ArrayTy:
            return "array";
        case StructTy:
            return "struct";
        case VecTy:
            return "vec";
        default:
            return "unknown";
        }
    }

    Type(TypeID tid, Module *m) : tid_(tid), module_(m) {}
    virtual ~Type() = default;

    TypeID type_id() const { return tid_; }
    virtual size_t size() const = 0;
    virtual std::string name() const = 0;
    virtual unsigned bits() const = 0;
    virtual size_t alignment() const { return size(); } // TODO: for different architectures

    Module *module() const { return module_; }

    static Type *get_void_type(Module *m);

    bool is_float() const { return tid_ == FpTy; }
    bool is_integer() const { return tid_ == IntTy; }
    bool is_pointer() const { return tid_ == PtrTy; }
    bool is_function() const { return tid_ == FuncTy; }
    bool is_array() const { return tid_ == ArrayTy; }
    bool is_struct() const { return tid_ == StructTy; }
    bool is_vector() const { return tid_ == VecTy; }

    bool is_const() const { return is_const_; }

    Type *element_type() const;

protected:
    TypeID tid_;
    Module *module_;
    bool is_const_ = false;
};

class IntegerType : public Type
{
public:
    size_t size() const override { return (bits_ + 7) / 8; }
    std::string name() const override { return "i" + std::to_string(bits_); }
    unsigned bits() const override { return bits_; }

private:
    explicit IntegerType(Module *m, unsigned bits);
    unsigned bits_;

    friend Module;
};

class FloatType : public Type
{
public:
    enum Precision
    {
        Half,   // 16-bit
        Single, // 32-bit
        Double, // 64-bit
        Quad    // 128-bit
    };

    Precision precision() const { return precision_; }
    unsigned bits() const override { return bits_; }
    size_t size() const override { return (bits_ + 7) / 8; }
    std::string name() const override { return "f" + std::to_string(bits_); }

private:
    explicit FloatType(Module *m, Precision precision);
    unsigned bits_;

    Precision precision_;
    friend class Module;
};

class VoidType : public Type
{
public:
    size_t size() const override { return 0; }
    std::string name() const override { return "void"; }
    unsigned bits() const override { return 0; }

private:
    explicit VoidType(Module *m) : Type(VoidTy, m) {}

    Module *module_;

    friend Module;
};

class PointerType : public Type
{
public:
    Type *element_type() const { return element_type_; }

    // FIXME: Size should be handled through DataLayout instead of hardcoding
    size_t size() const override { return sizeof(void *); }
    std::string name() const override { return element_type_->name() + "*"; }
    unsigned bits() const override { return sizeof(void *) * 8; }

private:
    PointerType(Module *m, Type *element_type);

    Type *element_type_;
    Module *module_;

    friend Module;
};

class FunctionType : public Type
{
public:
    Type *return_type() const { return return_type_; }

    const std::vector<Type *> param_types() const
    {
        std::vector<Type *> result;
        for (auto &p : params_)
        {
            result.push_back(p.second);
        }
        return result;
    }
    const Type *param_type(unsigned index) const { return params_[index].second; }
    const std::string &param_name(unsigned index) const { return params_[index].first; }

    ParamList params() const { return ParamList(params_.begin(), params_.end()); }
    size_t num_params() const { return params_.size(); }
    size_t size() const override { return 0; }

    std::string name() const override
    {
        std::string result = return_type_->name() + " (";
        for (size_t i = 0; i < params_.size(); ++i)
        {
            if (i != 0)
                result += ", ";
            result += params_[i].second->name();
        }
        result += ")";
        return result;
    }

    unsigned bits() const override { return 0; }

private:
    FunctionType(Module *m, Type *return_type, const ParamList &params)
        : Type(FuncTy, m), return_type_(return_type), params_(params.begin(), params.end())
    {
        for (size_t i = 0; i < params_.size(); ++i)
        {
            if (params_[i].first.empty())
            {
                params_[i].first = "__arg" + std::to_string(i);
            }
        }
    }

    Type *return_type_;
    ParamList params_;

    friend class Module;
};

class ArrayType : public Type
{
public:
    Type *element_type() const { return element_type_; }
    uint64_t num_elements() const { return num_elements_; }
    size_t size() const override;
    std::string name() const override
    {
        return "[" + std::to_string(num_elements_) + " x " + element_type_->name() + "]";
    }

    unsigned bits() const override { return size() * 8; }

private:
    ArrayType(Module *m, Type *element_type, uint64_t num_elements);

    Type *element_type_;
    uint64_t num_elements_;
    Module *module_;

    friend Module;
};

class StructType : public Type
{
public:
    friend class Module;
    // For named structs, identifier is the name
    // For anonymous structs, identifier is all members

    void set_name(const std::string &name) { name_ = name; }
    std::string name() const override
    {
        if (is_opaque_)
            return "opaque";
        std::string result = "{ ";
        for (size_t i = 0; i < members_.size(); ++i)
        {
            if (i != 0)
                result += ", ";
            result += members_[i]->name();
        }
        result += " }";
        return result;
    }
    unsigned bits() const override { return size() * 8; }

    // Completes the struct definition
    void set_body(std::vector<Type *> members);

    // Gets a member by index
    Type *get_member_type(unsigned index) const;

    // Gets the member offset
    size_t get_member_offset(unsigned index) const;

    size_t get_member_index(const std::string &name) const;
    bool has_member(const std::string &name) const;

    size_t size() const override;

    bool is_opaque() const { return is_opaque_; }
    const std::vector<Type *> &members() const { return members_; }

private:
    StructType(Module *m, const std::string &name, std::vector<Type *> members);
    StructType(Module *m, std::vector<Type *> members);

    std::string name_;
    Module *module_;
    bool is_opaque_; // true if forward declaration
    std::vector<Type *> members_;
    std::vector<size_t> offsets_;
    size_t size_;

    friend Module;
};

class VectorType : public Type
{
public:
    Type *element_type() const { return element_type_; }
    uint64_t num_elements() const { return num_elements_; }

    size_t size() const override
    {
        return element_type_->size() * num_elements_;
    }

    std::string name() const override
    {
        return "<" + std::to_string(num_elements_) + " x " + element_type_->name() + ">";
    }

    unsigned bits() const override
    {
        return element_type_->bits() * num_elements_;
    }

private:
    VectorType(Module *m, Type *element_type, uint64_t num_elements)
        : Type(VecTy, m), element_type_(element_type), num_elements_(num_elements) {}

    Type *element_type_;
    uint64_t num_elements_;

    friend class Module;
};

//===----------------------------------------------------------------------===//
//                              Value Base Class
//===----------------------------------------------------------------------===//
class Value
{
public:
    virtual ~Value();

    const std::string &name() const { return name_; }
    Type *type() const { return type_; }
    const std::vector<User *> &users() const { return users_; }

    void set_name(const std::string &name) { name_ = name; }
    void add_user(User *user) { users_.push_back(user); }
    void remove_user(Value *user);

protected:
    Value(Type *type, const std::string &name = "")
        : type_(type), name_(name) {}

    Type *type_;
    std::string name_;
    std::vector<User *> users_;
};

//===----------------------------------------------------------------------===//
//                              User Base Class
//===----------------------------------------------------------------------===//
class User : public Value
{
public:
    const std::vector<Value *> &operands() const { return operands_; }
    Value *operand(unsigned i) const { return operands_.at(i); }
    void set_operand(unsigned i, Value *v);
    void remove_use_of(Value *v);

protected:
    User(Type *type, const std::string &name = "")
        : Value(type, name) {}
    ~User() override;

    std::vector<Value *> operands_;
};

//===----------------------------------------------------------------------===//
//                              Instruction System
//===----------------------------------------------------------------------===//
enum class Opcode
{
    Add,
    Sub,
    Mul,
    UDiv,
    SDiv,
    Alloca,
    Load,
    Store,
    GetElementPtr,
    ICmp,
    FCmp,
    Br,
    CondBr,
    Ret,
    Phi,
    Call,
    ZExt,
    SExt,
    Trunc,
    SIToFP,
    FPToSI,
    FPExt,
    FPTrunc,
    BitCast,
    BitAnd,
    BitOr,
    BitXor
};

class Instruction : public User
{
public:
    Opcode opcode() const { return opcode_; }
    BasicBlock *parent() const { return parent_; }

    Instruction *next() const { return next_; }
    Instruction *prev() const { return prev_; }

    static Instruction *create(Opcode opc, Type *type,
                               std::vector<Value *> operands,
                               BasicBlock *parent);

protected:
    friend class BasicBlock;

    Instruction(Opcode opcode, Type *type, BasicBlock *parent,
                std::vector<Value *> operands, const std::string &name = "");

    Opcode opcode_;
    BasicBlock *parent_;
    Instruction *prev_;
    Instruction *next_;
};

//===----------------------------------------------------------------------===//
//                              Basic Block
//===----------------------------------------------------------------------===//
class BasicBlock : public Value
{
public:
    explicit BasicBlock(const std::string &name, Function *parent);
    ~BasicBlock() override;

    Function *parent_function() const { return parent_; }

    Instruction *first_instruction() const { return head_; }
    Instruction *last_instruction() const { return tail_; }
    Instruction *get_terminator() const;
    void insert_before(Instruction *pos, std::unique_ptr<Instruction> inst);
    void insert_after(Instruction *pos, std::unique_ptr<Instruction> inst);

    const std::vector<BasicBlock *> &predecessors() const { return predecessors_; }
    const std::vector<BasicBlock *> &successors() const { return successors_; }
    void add_successor(BasicBlock *bb);
    void append(Instruction *inst);

    class iterator
    {
    public:
        iterator(Instruction *ptr) : ptr_(ptr) {}

        Instruction &operator*() const { return *ptr_; }
        Instruction *operator->() { return ptr_; }
        iterator &operator++()
        {
            ptr_ = ptr_->next();
            return *this;
        }
        iterator operator++(int)
        {
            iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        bool operator==(const iterator &other) const { return ptr_ == other.ptr_; }
        bool operator!=(const iterator &other) const { return ptr_ != other.ptr_; }

    private:
        Instruction *ptr_;
    };

    class const_iterator
    {
    public:
        const_iterator(const Instruction *ptr) : ptr_(ptr) {}

        const Instruction &operator*() const { return *ptr_; }
        const Instruction *operator->() { return ptr_; }
        const_iterator &operator++()
        {
            ptr_ = ptr_->next();
            return *this;
        }
        const_iterator operator++(int)
        {
            const_iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        bool operator==(const const_iterator &other) const { return ptr_ == other.ptr_; }
        bool operator!=(const const_iterator &other) const { return ptr_ != other.ptr_; }

    private:
        const Instruction *ptr_;
    };

    iterator begin() { return iterator(head_); }
    iterator end() { return iterator(nullptr); }

    const_iterator begin() const { return const_iterator(head_); }
    const_iterator end() const { return const_iterator(nullptr); }

    using value_type = Instruction;
    using reference = Instruction &;
    using const_reference = const Instruction &;

private:
    Function *parent_;
    Instruction *head_;
    Instruction *tail_;

    std::vector<BasicBlock *> predecessors_;
    std::vector<BasicBlock *> successors_;
};

//===----------------------------------------------------------------------===//
//                              Function
//===----------------------------------------------------------------------===//
class Argument : public Value
{
public:
    Argument(const std::string &name, Type *type, Function *parent)
        : Value(type, name), parent_(parent) {}

    Function *parent() const { return parent_; }

private:
    Function *parent_;
};

class Function : public Value
{
public:
    Function(const std::string &name, Module *parent, Type *return_type,
             const ParamList &params);
    ~Function() override;

    BasicBlock *create_basic_block(const std::string &name = "");
    Module *parent_module() const { return parent_; }

    Type *return_type() const { return return_type_; }
    Type *arg_type(size_t idx) const { return args_.at(idx)->type(); }
    const std::vector<Argument *> &args() const { return args_; }
    Argument *arg(size_t idx) const { return args_.at(idx); }
    size_t num_args() const { return args_.size(); }
    void set_instance_method(bool is_instance_method) { is_instance_method_ = is_instance_method; }
    std::vector<Type *> param_types() const
    {
        std::vector<Type *> types;
        for (auto arg : args_)
            types.push_back(arg->type());
        return types;
    }
    auto begin() { return basic_blocks_.begin(); }
    auto end() { return basic_blocks_.end(); }

    const std::vector<BasicBlock *> &basic_blocks() const { return basic_block_ptrs_; }

private:
    Module *parent_;
    Type *return_type_;
    std::vector<std::unique_ptr<Argument>> arguments_;
    std::vector<Argument *> args_;
    std::vector<std::unique_ptr<BasicBlock>> basic_blocks_;
    std::vector<BasicBlock *> basic_block_ptrs_;
    bool is_instance_method_ = false;
};

//===----------------------------------------------------------------------===//
//                              Module
//===----------------------------------------------------------------------===//

class Module
{
    struct DoublePairHash
    {
        template <typename T1, typename T2>
        size_t operator()(const std::pair<T1, T2> &p) const
        {
            // ensure double is hashed fully
            static_assert(sizeof(double) == sizeof(uint64_t), "Unexpected double size");

            size_t hash1 = std::hash<T1>{}(p.first);
            uint64_t bits = 0;

            if constexpr (std::is_same_v<T2, double>)
            {
                // uint64_t convert double to uint64_t
                std::memcpy(&bits, &p.second, sizeof(double));
            }
            else
            {
                bits = std::hash<T2>{}(p.second);
            }

            size_t hash2 = std::hash<uint64_t>{}(bits);
            return hash1 ^ (hash2 << 1);
        }
    };

    struct DoublePairEqual
    {
        template <typename T1, typename T2>
        bool operator()(const std::pair<T1, T2> &lhs, const std::pair<T1, T2> &rhs) const
        {
            if (lhs.first != rhs.first)
                return false;

            if constexpr (std::is_same_v<T2, double>)
            {
                // 按二进制严格比较 double
                return std::memcmp(&lhs.second, &rhs.second, sizeof(double)) == 0;
            }
            else
            {
                return lhs.second == rhs.second;
            }
        }
    };

public:
    friend class VoidType;
    friend class PointerType;
    friend class StructType;
    friend class ArrayType;

    Module(std::string name = "");
    ~Module();

    Function *create_function(
        const std::string &name,
        Type *return_type,
        const std::vector<std::pair<std::string, Type *>> &params);

    Function *create_function(
        const std::string &name,
        Type *return_type,
        std::initializer_list<std::pair<std::string, Type *>> params)
    {
        return create_function(name, return_type, std::vector<std::pair<std::string, Type *>>(params));
    }

    Function *create_function(
        const std::string &name,
        FunctionType *type);

    GlobalVariable *create_global_variable(Type *type, bool is_constant, Constant *initializer, const std::string &name = "");

    Type *get_void_type();
    IntegerType *get_integer_type(unsigned bits);
    FloatType *get_float_type(FloatType::Precision precision);

    PointerType *get_pointer_type(Type *element_type);
    FunctionType *get_function_type(Type *return_type, const std::vector<Type *> &param_types);

    ConstantInt *get_constant_int(IntegerType *type, uint64_t value);
    ConstantInt *get_constant_int(unsigned bits, uint64_t value);

    ConstantFP *get_constant_fp(FloatType *type, double value);
    ConstantFP *get_constant_fp(FloatType::Precision precision, double value);

    ConstantString *get_constant_string(std::string value);
    ConstantPointerNull *get_constant_pointer_null(PointerType *type);
    ConstantAggregateZero *get_constant_aggregate_zero(Type *type);
    ConstantStruct *get_constant_struct(StructType *type, const std::vector<Constant *> &members);
    ConstantArray *get_constant_array(ArrayType *type, const std::vector<Constant *> &elements);

    const std::vector<Function *> functions() const
    {
        std::vector<Function *> result;
        result.reserve(functions_.size());
        for (auto &f : functions_)
            result.push_back(f.get());
        return result;
    }

    Function* get_function(const std::string &name) const {
        for (auto &f : functions_)
        {
            if (f->name() == name)
            {
                return f.get();
            }
        }
        return nullptr;
    }

    const std::vector<GlobalVariable *> global_variables() const
    {
        std::vector<GlobalVariable *> result;
        result.reserve(global_variables_.size());
        for (auto &gv : global_variables_)
            result.push_back(gv.second.get());
        return result;
    }

    StructType *get_struct_type_anonymous(const std::vector<Type *> &members);
    StructType *try_get_struct_type(const std::string &name);
    ArrayType *get_array_type(Type *element_type, uint64_t num_elements);
    StructType *get_struct_type(const std::string &name, const std::vector<Type *> &members);
    VectorType *get_vector_type(Type *element_type, uint64_t num_elements);

private:
    std::string name_;
    std::unique_ptr<VoidType> void_type_;
    std::unordered_map<unsigned, std::unique_ptr<IntegerType>> integer_types_;
    std::unordered_map<FloatType::Precision, std::unique_ptr<FloatType>> float_types_;
    // (element_type) -> pointer_type
    std::unordered_map<Type *, std::unique_ptr<PointerType>> pointer_types_;

    std::unordered_map<std::pair<Type *, uint64_t>,
                       std::unique_ptr<ConstantInt>>
        constant_ints_;

    std::unordered_map<
        std::pair<Type *, double>,
        std::unique_ptr<ConstantFP>,
        DoublePairHash,
        DoublePairEqual>
        constant_fps_;

    std::vector<std::unique_ptr<Function>> functions_;
    std::unordered_map<std::string, std::unique_ptr<GlobalVariable>> global_variables_;
    std::vector<std::unique_ptr<ConstantStruct>> constant_structs_;
    std::vector<std::unique_ptr<ConstantArray>> constant_arrays_;
    std::vector<std::unique_ptr<ConstantString>> constant_strings_;
    std::vector<std::unique_ptr<ConstantPointerNull>> constant_pointer_nulls_;
    std::vector<std::unique_ptr<ConstantAggregateZero>> constant_aggregate_zeros_;

    friend class ArrayType;
    friend class StructType;

    // Type storage
    std::unordered_map<std::pair<Type *, uint64_t>,
                       std::unique_ptr<ArrayType>>
        array_types_;
    std::vector<std::unique_ptr<StructType>> struct_types_;
    std::unordered_map<std::pair<Type *, uint64_t>, std::unique_ptr<VectorType>> vector_types_;
    std::unordered_map<std::string, std::unique_ptr<StructType>> opaque_structs_;

    using FunctionTypeKey = std::pair<Type *, std::vector<Type *>>;
    struct FunctionTypeKeyHash
    {
        size_t operator()(const FunctionTypeKey &key) const
        {
            size_t hash = std::hash<Type *>{}(key.first);
            for (auto *type : key.second)
            {
                hash ^= std::hash<Type *>{}(type);
            }
            return hash;
        }
    };
    std::unordered_map<FunctionTypeKey, std::unique_ptr<FunctionType>, FunctionTypeKeyHash> function_types_;
};

//===----------------------------------------------------------------------===//
//                              Constant
//===----------------------------------------------------------------------===//
class Constant : public Value
{
public:
    // std::string as_string() const override = 0;
    virtual std::string as_string() const = 0;

protected:
    Constant(Type *type, const std::string &name = "")
        : Value(type, name) {}
};

// Integer constant
class ConstantInt : public Constant
{
public:
    uint64_t value() const { return value_; }

    ConstantInt *zext_value(Module *m, IntegerType *dest_type) const;
    ConstantInt *sext_value(Module *m, IntegerType *dest_type) const;

    std::string as_string() const override;

private:
    ConstantInt(IntegerType *type, uint64_t value);

    uint64_t value_;
    friend class Module;
};

// Floating-point constant
class ConstantFP : public Constant
{
public:
    double value() const { return value_; }

    std::string as_string() const override;

private:
    ConstantFP(FloatType *type, double value);

    double value_;
    friend class Module;
};

// Array constant
class ConstantArray : public Constant
{
public:
    const std::vector<Constant *> &elements() const { return elements_; }

    std::string as_string() const override;

private:
    ConstantArray(ArrayType *type, const std::vector<Constant *> &elements)
        : Constant(type), elements_(elements) {}

    std::vector<Constant *> elements_;
    friend class Module;
};

// String constant
class ConstantString : public Constant
{
public:
    const std::string &value() const;
    std::string as_string() const override;

private:
    ConstantString(ArrayType *type, const std::string &value);
    static std::string escape_string(const std::string &input);

    std::string value_;
    friend class Module;
};

// Structure constant
class ConstantStruct : public Constant
{
public:
    const std::vector<Constant *> &members() const { return members_; }

    std::string as_string() const override;

private:
    ConstantStruct(StructType *type, const std::vector<Constant *> &members)
        : Constant(type), members_(members) {}

    std::vector<Constant *> members_;
    friend class Module;
};

class GlobalVariable : public Constant
{
public:
    bool is_constant() const { return is_constant_; }
    Constant *initializer() const { return initializer_; }

    std::string as_string() const override;

private:
    GlobalVariable(Type *type, bool is_constant, Constant *initializer,
                   const std::string &name)
        : Constant(type, name), is_constant_(is_constant),
          initializer_(initializer) {}

    bool is_constant_;
    Constant *initializer_;
    friend class Module;
};

// Null pointer constant
class ConstantPointerNull : public Constant
{
public:
    std::string as_string() const override;

private:
    ConstantPointerNull(PointerType *type)
        : Constant(type) {}

    friend class Module;
};

// Aggregate zero constant
class ConstantAggregateZero : public Constant
{
public:
    std::string as_string() const override;

private:
    ConstantAggregateZero(Type *type)
        : Constant(type) {}

    friend class Module;
};

//===----------------------------------------------------------------------===//
//                           Instruction Subclasses
//===----------------------------------------------------------------------===//
class BranchInst : public Instruction
{
public:
    static BranchInst *create(BasicBlock *target, BasicBlock *parent);

    static BranchInst *create_cond(Value *cond, BasicBlock *true_bb,
                                   BasicBlock *false_bb, BasicBlock *parent);

    bool is_conditional() const { return operands_.size() > 0; }
    BasicBlock *get_true_successor() const;
    BasicBlock *get_false_successor() const;

private:
    BranchInst(BasicBlock *target, BasicBlock *parent,
               std::vector<Value *> ops);

    BasicBlock *true_bb_;
    BasicBlock *false_bb_;
};

class ReturnInst : public Instruction
{
public:
    static ReturnInst *create(Value *value, BasicBlock *parent);

    Value *value() const { return operands_.size() > 0 ? operands_[0] : nullptr; }

private:
    ReturnInst(Value *value, BasicBlock *parent);
};

class PhiInst : public Instruction
{
public:
    static PhiInst *create(Type *type, BasicBlock *parent);

    void add_incoming(Value *val, BasicBlock *bb);

    unsigned num_incoming() const { return operands_.size() / 2; }
    Value *get_incoming_value(unsigned i) const { return operands_[2 * i]; }
    BasicBlock *get_incoming_block(unsigned i) const;

private:
    PhiInst(Type *type, BasicBlock *parent);
};

class ICmpInst : public Instruction
{
public:
    enum Predicate
    {
        EQ,
        NE,
        SLT,
        SLE,
        SGT,
        SGE,
        ULT,
        ULE,
        UGT,
        UGE
    };

    static ICmpInst *create(Predicate pred, Value *lhs, Value *rhs,
                            BasicBlock *parent);

    Predicate predicate() const { return pred_; }

private:
    ICmpInst(BasicBlock *parent, std::vector<Value *> ops);

    Predicate pred_;
};

class FCmpInst : public Instruction
{
public:
    enum Predicate
    {
        EQ,
        NE,
        OLT,
        OLE,
        OGT,
        OGE
    };

    static FCmpInst *create(Predicate pred, Value *lhs, Value *rhs, BasicBlock *parent, const std::string &name = "");
    Predicate predicate() const { return pred_; }

private:
    FCmpInst(Predicate pred, Type *type, BasicBlock *parent, std::vector<Value *> operands, const std::string &name);
    Predicate pred_;
};

//===----------------------------------------------------------------------===//
//                           Memory Operation Instruction Subclasses
//===----------------------------------------------------------------------===//
class AllocaInst : public Instruction
{
public:
    static AllocaInst *create(Type *allocated_type, BasicBlock *parent,
                              const std::string &name = "");

    Type *allocated_type() const { return allocated_type_; }

private:
    AllocaInst(Type *allocated_type, Type *ptr_type, BasicBlock *parent);

    Type *allocated_type_;
};

class LoadInst : public Instruction
{
public:
    static LoadInst *create(Value *ptr, BasicBlock *parent,
                            const std::string &name = "");

    Value *pointer() const { return operand(0); }

private:
    LoadInst(Type *loaded_type, BasicBlock *parent, Value *ptr);
};

class StoreInst : public Instruction
{
public:
    static StoreInst *create(Value *value, Value *ptr, BasicBlock *parent);

    Value *stored_value() const { return operand(0); }
    Value *pointer() const { return operand(1); }

private:
    StoreInst(BasicBlock *parent, Value *value, Value *ptr);
};

//===----------------------------------------------------------------------===//
//      Address Calculation Instruction Subclasses
//===----------------------------------------------------------------------===//
class GetElementPtrInst : public Instruction
{
public:
    static GetElementPtrInst *create(Value *ptr, std::vector<Value *> indices,
                                     BasicBlock *parent,
                                     const std::string &name = "");

    Value *base_pointer() const { return operand(0); }
    const std::vector<Value *> indices() const;

private:
    static Type *get_result_type(Type *base_type, const std::vector<Value *> &indices);

    GetElementPtrInst(Type *result_type, BasicBlock *parent,
                      Value *ptr, std::vector<Value *> indices);
};

class BinaryInst : public Instruction
{
public:
    static BinaryInst *create(Opcode op, Value *lhs, Value *rhs, BasicBlock *parent, const std::string &name = "");
    Value *get_lhs() const { return operand(0); }
    Value *get_rhs() const { return operand(1); }

private:
    BinaryInst(Opcode op, Type *type, BasicBlock *parent, std::vector<Value *> operands, const std::string &name);
    static bool isBinaryOp(Opcode op);
};

// FIXME: Deprecated use CastInst instead
class ConversionInst : public Instruction
{
public:
    static ConversionInst *create(Opcode op, Value *val, Type *dest_type, BasicBlock *parent, const std::string &name = "");
    Value *get_source() const { return operand(0); }
    Type *get_dest_type() const { return type(); }

private:
    ConversionInst(Opcode op, Type *dest_type, BasicBlock *parent, std::vector<Value *> operands, const std::string &name);
    static bool isConversionOp(Opcode op);
};

class CastInst : public Instruction
{
protected:
    CastInst(Opcode op, Type *target_type, BasicBlock *parent,
             std::initializer_list<Value *> operands,
             const std::string &name);

public:
    Value *source() const { return operand(0); }
    Type *target_type() const { return type(); }
};

class BitCastInst : public CastInst
{
public:
    static BitCastInst *create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name);

    Value *source_value() const { return operand(0); }
    Type *target_type() const { return type(); }

private:
    BitCastInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name);
};

class CallInst : public Instruction
{
public:
    static CallInst *create(Value *callee, Type *return_type, const std::vector<Value *> &args, BasicBlock *parent, const std::string &name);
    static CallInst *create(Function *callee, const std::vector<Value *> &args, BasicBlock *parent, const std::string &name);

    // Warning: This function is not safe to use if the function is not a direct
    // function call, but rather a function pointer or a function reference.
    Function *called_function() const { return dynamic_cast<Function *>(operand(0)); }
    std::vector<Value *> create_operand_list(Value *callee, const std::vector<Value *> &args);
    std::vector<Value *> arguments() const;

private:
    CallInst(BasicBlock *parent, Value *callee, Type *return_type, const std::vector<Value *> &args, const std::string &name);
};

class RawCallInst : public Instruction
{
public:
    static RawCallInst *create(Value *callee, const std::vector<Value *> &args, BasicBlock *parent, const std::string &name);

    Value *callee() const { return operand(0); }
    std::vector<Value *> arguments() const;

private:
    RawCallInst(BasicBlock *parent, Value *callee, const std::vector<Value *> &args, const std::string &name);
};

class SExtInst : public CastInst
{
public:
    static SExtInst *create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name);

    Value *source_value() const { return operand(0); }
    Type *target_type() const { return type(); }

private:
    SExtInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name);
};

class TruncInst : public CastInst
{
public:
    static TruncInst *create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name);

    Value *source_value() const { return operand(0); }
    Type *target_type() const { return type(); }

private:
    TruncInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name);
};

class SIToFPInst : public CastInst
{
public:
    static SIToFPInst *create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name);

    Value *source_value() const { return operand(0); }
    Type *target_type() const { return type(); }

private:
    SIToFPInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name);
};

class FPToSIInst : public CastInst
{
public:
    static FPToSIInst *create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name);

    Value *source_value() const { return operand(0); }
    Type *target_type() const { return type(); }

private:
    FPToSIInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name);
};

class FPExtInst : public CastInst
{
public:
    static FPExtInst *create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name);

    Value *source_value() const { return operand(0); }
    Type *target_type() const { return type(); }

private:
    FPExtInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name);
};

class FPTruncInst : public CastInst
{
public:
    static FPTruncInst *create(Value *val, Type *target_type, BasicBlock *parent, const std::string &name);

    Value *source_value() const { return operand(0); }
    Type *target_type() const { return type(); }

private:
    FPTruncInst(BasicBlock *parent, Value *val, Type *target_type, const std::string &name);
};

//===----------------------------------------------------------------------===//
//      Structure Layout
//===----------------------------------------------------------------------===//

struct Member
{
    Type *type;
    size_t offset;
};

struct StructLayout
{
    std::vector<Member> members;
    size_t size;
    size_t alignment;
};

StructLayout calculate_aligned_layout(const std::vector<Type *> &members);
