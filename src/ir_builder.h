#pragma once
#include "ir.h"

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
    BinaryInst *create_binary(Opcode opc, Value *lhs, Value *rhs,
                              const std::string &name = "");

    BinaryInst *create_add(Value *lhs, Value* rhs, const std::string &name = "") {
        return create_binary(Opcode::Add, lhs, rhs, name);
    }

    BinaryInst *create_sub(Value *lhs, Value* rhs, const std::string &name = "") {
        return create_binary(Opcode::Sub, lhs, rhs, name);
    }

    BinaryInst *create_mul(Value *lhs, Value* rhs, const std::string &name = "") {
        return create_binary(Opcode::Mul, lhs, rhs, name);
    }

    BinaryInst *create_udiv(Value *lhs, Value* rhs, const std::string &name = "") {
        return create_binary(Opcode::UDiv, lhs, rhs, name);
    }

    BinaryInst *create_sdiv(Value *lhs, Value* rhs, const std::string &name = "") {
        return create_binary(Opcode::SDiv, lhs, rhs, name);
    }

    ICmpInst *create_icmp(ICmpInst::Predicate pred, Value *lhs, Value *rhs,
                          const std::string &name = "");

    FCmpInst *create_fcmp(FCmpInst::Predicate pred, Value *lhs, Value *rhs,
                          const std::string &name = "");

    BranchInst *create_br(BasicBlock *target);

    BranchInst *create_cond_br(Value *cond, BasicBlock *true_bb,
                               BasicBlock *false_bb);

    ReturnInst *create_ret(Value *value);
    ReturnInst *create_ret_void();

    PhiInst *create_phi(Type *type, const std::string &name = "");

    // Constant creation
    ConstantInt *get_int32(int32_t val);
    ConstantInt *get_int64(int64_t val);
    ConstantInt *get_int1(bool val);
    ConstantFP *get_float(double val);

    // Memory operations
    AllocaInst *create_alloca(Type *type, const std::string &name = "");
    LoadInst *create_load(Value *ptr, const std::string &name = "");
    StoreInst *create_store(Value *value, Value *ptr);
    GetElementPtrInst *create_gep(Value *ptr, std::vector<Value *> indices,
                                  const std::string &name = "");

    template <typename... Args>
    GetElementPtrInst *create_gep(Value *ptr, Args... indices);

    Value *create_struct_gep(Value *struct_ptr, unsigned idx,
                             const std::string &name = "");

    ArrayType *get_array_type(Type *elem_ty, uint64_t num);

    StructType *create_struct_type(const std::string &name = "");

    StructType *get_struct_type(const std::vector<Type *> &members);

private:
    void insert(Instruction *inst);

    Module *module_;
    BasicBlock *insert_block_;
    Instruction *insert_pos_;
};
