#pragma once
#include <vector>
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
class IntegerType;
class VoidType;
class PointerType;
class ArrayType;
class StructType;

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
class Type
{
public:
    enum TypeID
    {
        void_ty_id,     // void type
        integer_ty_id,  // integer type
        pointer_ty_id,  // pointer type
        function_ty_id, // function type
        array_ty_id,    // array type ID
        struct_ty_id    // struct type ID
    };

    // Modified constructor: Module* is now a required parameter
    Type(TypeID tid, Module *m) : tid_(tid), module_(m) {}
    virtual ~Type() = default;

    TypeID type_id() const { return tid_; }
    virtual size_t size() const = 0;
    virtual std::string name() const = 0;
    virtual unsigned bits() const = 0;


    Module *module() const { return module_; }

    static Type *get_void_type(Module *m);

protected:
    TypeID tid_;
    Module *module_;
};

class IntegerType : public Type
{
public:
    static IntegerType *get(Module *m, unsigned bits);
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

    static FloatType *get(Module *m, Precision precision);

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
    static VoidType *get(Module *m);

    size_t size() const override { return 0; }
    std::string name() const override { return "void"; }
    unsigned bits() const override { return 0; }

private:
    explicit VoidType(Module *m) : Type(void_ty_id, m) {}

    Module *module_;

    friend Module;
};

class PointerType : public Type
{
public:
    static PointerType *get(Module *m, Type *element_type);

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

class ArrayType : public Type
{
public:
    static ArrayType *get(Module *m, Type *element_type, uint64_t num_elements);

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


    // Creates an opaque struct (forward declaration)
    static StructType *create(Module *m, const std::string &name);

    // Completes the struct definition
    void set_body(std::vector<Type *> members);

    // Gets a member by index
    Type *get_member_type(unsigned index) const;

    // Gets the member offset
    size_t get_member_offset(unsigned index) const;

    size_t size() const override;

    bool is_opaque() const { return is_opaque_; }
    const std::vector<Type *> &members() const { return members_; }

private:
    StructType(Module *m);

    std::string name_;
    Module *module_;
    bool is_opaque_; // true if forward declaration
    std::vector<Type *> members_;
    std::vector<size_t> offsets_;
    size_t size_;

    friend Module;
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
    add,
    sub,
    mul,
    udiv,
    sdiv,
    alloca_,
    load,
    store,
    getelementptr,
    icmp,
    fcmp,
    br,
    cond_br,
    ret,
    phi,
    call,
    zext,
    sext,
    trunc
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
                std::vector<Value *> operands);

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

    Function *parent() const { return parent_; }

    Instruction *first_instruction() const { return head_; }
    Instruction *last_instruction() const { return tail_; }
    void insert_before(Instruction *pos, std::unique_ptr<Instruction> inst);

    const std::vector<BasicBlock *> &predecessors() const { return predecessors_; }
    const std::vector<BasicBlock *> &successors() const { return successors_; }
    void add_successor(BasicBlock *bb);

private:
    Function *parent_;
    Instruction *head_;
    Instruction *tail_;

    std::vector<BasicBlock *> predecessors_;
    std::vector<BasicBlock *> successors_;

    void link_instruction(Instruction *inst);
};

//===----------------------------------------------------------------------===//
//                              Function
//===----------------------------------------------------------------------===//
class Function : public Value
{
public:
    Function(const std::string &name, Module *parent, Type *return_type,
             const std::vector<Type *> &param_types);
    ~Function() override;

    BasicBlock *create_basic_block(const std::string &name = "");
    Module *parent() const { return parent_; }

    Type *return_type() const { return return_type_; }
    const std::vector<Type *> &param_types() const { return param_types_; }
    const std::vector<BasicBlock *> &basic_blocks() const { return basic_block_ptrs_; }

private:
    Module *parent_;
    Type *return_type_;
    std::vector<Type *> param_types_;
    std::vector<std::unique_ptr<BasicBlock>> basic_blocks_;
    std::vector<BasicBlock *> basic_block_ptrs_;
};

//===----------------------------------------------------------------------===//
//                              Module
//===----------------------------------------------------------------------===//
class Module
{
public:
    friend class VoidType;
    friend class PointerType;
    friend class StructType;
    friend class ArrayType;

    Module();
    ~Module();

    Function *create_function(const std::string &name,
                              Type *return_type,
                              const std::vector<Type *> &param_types);

    Type *get_void_type();
    IntegerType *get_integer_type(unsigned bits);
    PointerType *get_pointer_type(Type *element_type);

    ConstantInt *get_constant_int(IntegerType *type, uint64_t value);
    ConstantInt *get_constant_int(unsigned bits, uint64_t value);

    const std::vector<Function *> &functions() const { return function_ptrs_; }

    ArrayType *get_array_type(Type *element_type, uint64_t num_elements);

    StructType *create_struct_type(const std::string &name);

    StructType *get_struct_type(const std::vector<Type *> &members);

private:
    std::unique_ptr<VoidType> void_type_;
    std::unordered_map<unsigned, std::unique_ptr<IntegerType>> integer_types_;
    std::unordered_map<Type *, std::unique_ptr<PointerType>> pointer_types_;

    std::unordered_map<std::pair<Type *, uint64_t>,
                       std::unique_ptr<ConstantInt>>
        constant_ints_;

    std::vector<std::unique_ptr<Function>> functions_;
    std::vector<Function *> function_ptrs_;

    friend class ArrayType;
    friend class StructType;

    // Type storage
    std::unordered_map<std::pair<Type *, uint64_t>,
                       std::unique_ptr<ArrayType>>
        array_types_;
    std::vector<std::unique_ptr<StructType>> struct_types_;
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
    static ConstantInt *get(Module *m, IntegerType *type, uint64_t value);

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
    static ConstantFP *get(Module *m, Type *type, double value);

    double value() const { return value_; }

    std::string as_string() const override;

private:
    ConstantFP(Type *type, double value)
        : Constant(type), value_(value) {}

    double value_;
    friend class Module;
};

// Array constant
class ConstantArray : public Constant
{
public:
    static ConstantArray *get(Module *m, ArrayType *type,
                              const std::vector<Constant *> &elements);

    const std::vector<Constant *> &elements() const { return elements_; }

    std::string as_string() const override;

private:
    ConstantArray(ArrayType *type, const std::vector<Constant *> &elements)
        : Constant(type), elements_(elements) {}

    std::vector<Constant *> elements_;
    friend class Module;
};

// Structure constant
class ConstantStruct : public Constant
{
public:
    static ConstantStruct *get(Module *m, StructType *type,
                               const std::vector<Constant *> &members);

    const std::vector<Constant *> &members() const { return members_; }

    std::string as_string() const override;

private:
    ConstantStruct(StructType *type, const std::vector<Constant *> &members)
        : Constant(type), members_(members) {}

    std::vector<Constant *> members_;
    friend class Module;
};

// Global variable/constant
class GlobalVariable : public Constant
{
public:
    static GlobalVariable *get(Module *m, Type *type, bool is_constant,
                               Constant *initializer, const std::string &name);

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
    static ConstantPointerNull *get(Module *m, PointerType *type);

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
    static ConstantAggregateZero *get(Module *m, Type *type);

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

    BasicBlock *false_bb_;
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
//                           Address Calculation Instruction Subclasses
//===----------------------------------------------------------------------===//
class GetElementPtrInst : public Instruction
{
public:
    static GetElementPtrInst *create(Value *ptr, std::vector<Value *> indices,
                                     BasicBlock *parent,
                                     const std::string &name = "");

    Value *base_pointer() const { return operand(0); }
    const std::vector<Value *> &indices() const;

private:
    static Type *get_result_type(Type *base_type, const std::vector<Value *> &indices);

    GetElementPtrInst(Type *result_type, BasicBlock *parent,
                      Value *ptr, std::vector<Value *> indices);
};

//===----------------------------------------------------------------------===//
//                            IRBuilder Framework
//===----------------------------------------------------------------------===//
class IRBuilder
{
public:
    IRBuilder(Module *module = nullptr);

    // Basic block positioning
    void set_insert_point(BasicBlock *bb);

    void set_insert_point(Instruction *inst);

    // Instruction creation methods
    Value *create_add(Value *lhs, Value *rhs, const std::string &name = "");

    Value *create_icmp(ICmpInst::Predicate pred, Value *lhs, Value *rhs,
                       const std::string &name = "");

    BranchInst *create_br(BasicBlock *target);

    BranchInst *create_cond_br(Value *cond, BasicBlock *true_bb,
                               BasicBlock *false_bb);

    PhiInst *create_phi(Type *type, const std::string &name = "");

    // Constant creation
    ConstantInt *get_int32(int32_t val);

    // Memory operations
    AllocaInst *create_alloca(Type *type, const std::string &name = "");

    LoadInst *create_load(Value *ptr, const std::string &name = "");

    StoreInst *create_store(Value *value, Value *ptr);

    GetElementPtrInst *create_gep(Value *ptr, std::vector<Value *> indices,
                                  const std::string &name = "");

    template <typename... Args>
    GetElementPtrInst *create_gep(Value *ptr, Args... indices);

    Value *create_struct_gep(Value *struct_ptr, unsigned idx,
                             const std::string &name);

    ArrayType *get_array_type(Type *elem_ty, uint64_t num);

    StructType *create_struct_type(const std::string &name);

    StructType *get_struct_type(const std::vector<Type *> &members);

private:
    void insert(Instruction *inst);

    Value *create_binary(Opcode opc, Value *lhs, Value *rhs,
                         const std::string &name);

    Module *module_;
    BasicBlock *insert_block_;
    Instruction *insert_pos_;
};
