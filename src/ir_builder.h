// ir_builder.h - Defines the IRBuilder framework for constructing IR.

#pragma once
#include "ir.h"

//===----------------------------------------------------------------------===//
//                            IRBuilder Framework
//===----------------------------------------------------------------------===//
class IRBuilder
{
public:
    IRBuilder(Module *module = nullptr);

    //===--------------------------------------------------------------------===//
    //                               Positioning
    //===--------------------------------------------------------------------===//
    void set_insert_point(BasicBlock *bb);
    void set_insert_point(Instruction *inst);
    void clear_insert_point();
    BasicBlock *get_insert_block() const { return insert_block_; }

    //===--------------------------------------------------------------------===//
    //                               Constants
    //===--------------------------------------------------------------------===//
    ConstantInt *get_int32(int32_t val);
    ConstantInt *get_int64(int64_t val);
    ConstantInt *get_int1(bool val);
    ConstantFP *get_float(double val);

    //===--------------------------------------------------------------------===//
    //                               Instructions
    //===--------------------------------------------------------------------===//

    //--- Arithmetic Instructions ---//
    BinaryInst *create_binary(Opcode opc, Value *lhs, Value *rhs,
                              const std::string &name = "");
    BinaryInst *create_add(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_sub(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_mul(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_udiv(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_sdiv(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_bitand(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_bitor(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_bitxor(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_srem(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_urem(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_shl(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_ashr(Value *lhs, Value *rhs, const std::string &name = "");
    BinaryInst *create_lshr(Value *lhs, Value *rhs, const std::string &name = "");

    //--- Comparison Instructions ---//
    ICmpInst *create_icmp(ICmpInst::Predicate pred, Value *lhs, Value *rhs,
                          const std::string &name = "");
    FCmpInst *create_fcmp(FCmpInst::Predicate pred, Value *lhs, Value *rhs,
                          const std::string &name = "");

    //--- Control Flow Instructions ---//
    BranchInst *create_br(BasicBlock *target);
    BranchInst *create_cond_br(Value *cond, BasicBlock *true_bb,
                               BasicBlock *false_bb);
    ReturnInst *create_ret(Value *value);
    ReturnInst *create_ret_void();
    UnreachableInst *create_unreachable();

    //--- Memory Instructions ---//
    AllocaInst *create_alloca(Type *type, const std::string &name = "");
    AllocaInst *create_entry_alloca(Type *type, const std::string &name = "");
    LoadInst *create_load(Value *ptr, const std::string &name = "");
    StoreInst *create_store(Value *value, Value *ptr);
    GetElementPtrInst *create_gep(Value *ptr, std::vector<Value *> indices,
                                  const std::string &name = "");
    Value *create_struct_gep(Value *struct_ptr, unsigned idx,
                             const std::string &name = "");
    Value *create_extract_value(Value *agg_val,
                                const std::vector<size_t> &indices,
                                const std::string &name = "");

    //--- Other Instructions ---//
    PhiInst *create_phi(Type *type, const std::string &name = "");
    CallInst *create_call(Function *callee, const std::vector<Value *> &args,
                          const std::string &name);
    CallInst *create_call(Value *callee, const std::vector<Value *> &args,
                          const std::string &name);

    CallInst *create_indirect_call(Value *callee, const std::vector<Value *> &args,
                                   const std::string &name);

    // --- Cast Instructions --- //
    BitCastInst *create_bitcast(Value *val, Type *target_type,
                                const std::string &name);
    PtrToIntInst *create_ptrtoint(Value *ptr, Type *target_type,
                                  const std::string &name);
    IntToPtrInst *create_inttoptr(Value *val, Type *target_type,
                                  const std::string &name);
    SExtInst *create_sext(Value *val, Type *target_type, const std::string &name);
    ZExtInst *create_zext(Value *val, Type *target_type,
                          const std::string &name);
    FPExtInst *create_fpext(Value *val, Type *target_type,
                            const std::string &name);
    FPTruncInst *create_fptrunc(Value *val, Type *target_type,
                                const std::string &name);
    TruncInst *create_trunc(Value *val, Type *target_type,
                            const std::string &name);
    FPToSIInst *create_fptosi(Value *val, Type *target_type,
                              const std::string &name);
    FPToUIInst *create_fptoui(Value *val, Type *target_type,
                              const std::string &name);
    SIToFPInst *create_sitofp(Value *val, Type *target_type,
                              const std::string &name);
    UIToFPInst *create_uitofp(Value *val, Type *target_type,
                              const std::string &name);
    Value *create_cast(Value *src_val, Type *target_type,
                       const std::string &name,
                       bool is_explicit = false,
                       bool strict_mode = false);

    //===--------------------------------------------------------------------===//
    //                               Types
    //===--------------------------------------------------------------------===//
    ArrayType *get_array_type(Type *elem_ty, uint64_t num);
    StructType *get_struct_type(const std::vector<MemberInfo> &members);

private:
    void insert(Instruction *inst);

    Module *module_;
    BasicBlock *insert_block_;
    Instruction *insert_pos_;
};
