// ir_generator.h -- Generates SSA IR from type-checked AST

#pragma once
#include "ast.h"
#include "ir_builder.h"
#include <stack>
#include <vector>
#include <unordered_map>

class IRGenerator
{
public:
    explicit IRGenerator(Module *module);
    void generate(const ast::Program &program);

protected:
    // Context management
    Module *module_;
    IRBuilder builder_;
    Function *current_func_ = nullptr;

    BasicBlock *current_block() const
    {
        auto bb = builder_.get_insert_block();
        if (bb == nullptr) {
            MO_WARN("No current block");
        }
        return bb;
    }

    // Symbol table stack for scoping
    std::vector<std::unordered_map<std::string, Value *>> sym_table_stack_;

    // Loop context stacks
    std::stack<BasicBlock *> loop_cond_stack_;
    std::stack<BasicBlock *> loop_end_stack_;

    // Type conversion
    Value *handle_conversion(Value *val, Type *target_type, bool is_explicit = false);

    std::unordered_map<const ast::Type *, Type *> type_cache_;

    bool struct_matches_ir(const StructType *ir_struct, const ast::StructType &ast_struct);
    Type *convert_type(const ast::Type &ast_type);
    Type *dominant_type(Type *t1, Type *t2);
    void validate_receiver_type(const ast::FunctionDecl &method,
                                StructType *expected_type);

    // Scope management
    void push_scope();
    void pop_scope();
    Value *lookup_symbol(const std::string &name);
    void declare_symbol(const std::string &name, Value *val);

    // Control flow
    void push_loop(BasicBlock *cond_bb, BasicBlock *end_bb);
    void pop_loop();

    // AST dispatch
    void declare_function(const ast::FunctionDecl &func);
    void generate_function_body(const ast::FunctionDecl &func);
    void generate_impl_block(const ast::ImplBlock &impl);
    void generate_stmt(const ast::Statement &stmt);
    void generate_array_init(AllocaInst *array_ptr, const ast::Expr &init_expr);
    Value *generate_expr(const ast::Expr &expr);
    Value *generate_lvalue(const ast::Expr &expr);
    Value *handle_compound_assign(const ast::BinaryExpr &bin);

    // Statement handlers
    void handle_block(const ast::BlockStmt &block);
    void handle_var_decl(const ast::VarDeclStmt &decl);
    void handle_if(const ast::IfStmt &if_stmt);
    void handle_loop(const ast::WhileStmt &loop);
    void handle_expr_stmt(const ast::ExprStmt &stmt);
    void handle_break(const ast::BreakStmt &);
    void handle_continue(const ast::ContinueStmt &);
    void store_struct_recursive(Value *src, Value *dest_ptr);
    void handle_return(const ast::ReturnStmt &stmt);

    // Expression handlers
    Value *handle_binary(const ast::BinaryExpr &bin);
    Value *handle_call(const ast::CallExpr &call);
    Value *handle_variable(const ast::VariableExpr &var);
    Value *handle_member_access(const ast::MemberAccessExpr &access);
    Value *handle_field_load(Value *base, const std::string &member);
    Value *handle_method_invocation(const ast::MemberAccessExpr &expr);
    Value *handle_this_pointer(Value *base, TokenType accessor);
    Value *handle_field_access(const ast::MemberAccessExpr &access);

    Value *handle_array_access(const ast::ArrayAccessExpr &access);
    Value *handle_logical_and(const ast::BinaryExpr &and_expr);
    Value *handle_logical_or(const ast::BinaryExpr &or_expr);
    Value *handle_integer_literal(const ast::IntegerLiteralExpr &expr);
    Value *handle_float_literal(const ast::FloatLiteralExpr &expr);
    Value *handle_string_literal(const ast::StringLiteralExpr &expr);
    Value *handle_unary(const ast::UnaryExpr &expr);
    Value *handle_cast(const ast::CastExpr &expr);
    Value *handle_sizeof(const ast::SizeofExpr &expr);
    Value *handle_address_of(const ast::AddressOfExpr &expr);
    Value *handle_deref(const ast::DerefExpr &expr);
    Value *handle_init_list(const ast::InitListExpr &expr);
    Value *handle_tuple(const ast::TupleExpr &expr);
    Value *handle_function_pointer(const ast::FunctionPointerExpr &expr);
    Value *handle_struct_literal(const ast::StructLiteralExpr &expr);

    // Global variable handler
    void generate_global(const ast::GlobalDecl &global);
    Constant *generate_constant_initializer(const ast::Expr &expr);
};
