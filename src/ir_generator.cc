#include "ir_generator.h"

IRGenerator::IRGenerator(Module *module)
    : module_(module), builder_(module)
{
    sym_table_stack_.emplace_back(); // Global scope
}

//===----------------------------------------------------------------------===//
// Scope Management
//===----------------------------------------------------------------------===//
void IRGenerator::push_scope()
{
    sym_table_stack_.emplace_back();
}

void IRGenerator::pop_scope()
{
    assert(sym_table_stack_.size() > 1 && "Cannot pop global scope");
    sym_table_stack_.pop_back();
}

Value *IRGenerator::lookup_symbol(const std::string &name)
{
    for (auto it = sym_table_stack_.rbegin(); it != sym_table_stack_.rend(); ++it)
    {
        if (auto found = it->find(name); found != it->end())
        {
            return found->second;
        }
    }
    return nullptr;
}

void IRGenerator::declare_symbol(const std::string &name, Value *val)
{
    sym_table_stack_.back()[name] = val;
}

//===----------------------------------------------------------------------===//
// Type Conversion
//===----------------------------------------------------------------------===//

bool should_return_via_hidden_ptr(Type *ty);
size_t calculate_struct_size(const StructType *st);
size_t calculate_struct_alignment(const StructType *st);

bool should_return_via_hidden_ptr(Type *ty)
{
    if (!ty->is_struct())
        return false;

    const size_t max_register_bytes = 16; // TODO: make this configurable
    return ty->size() > max_register_bytes;
}

size_t calculate_struct_size(const StructType *st)
{
    size_t offset = 0;        // Current offset
    size_t max_alignment = 1; // Overall alignment requirement for the struct

    for (const auto &member : st->members())
    {
        // Recursively calculate layout information for nested structs
        size_t member_alignment = 1;
        size_t member_size = 0;

        if (member.type->is_struct())
        {
            // Nested struct needs recursive calculation
            const StructType *nested_st = dynamic_cast<const StructType *>(member.type);
            member_size = calculate_struct_size(nested_st);
            member_alignment = calculate_struct_alignment(nested_st);
        }
        else
        {
            // Basic types directly obtain information
            member_size = member.type->size();
            member_alignment = member.type->alignment();
        }

        // Calculate padding bytes (alignment adjustment)
        size_t padding = (member_alignment - (offset % member_alignment)) % member_alignment;
        offset += padding;

        // Update maximum alignment requirement
        max_alignment = std::max(max_alignment, member_alignment);

        // Accumulate member space
        offset += member_size;
    }

    // Struct tail padding
    size_t tail_padding = (max_alignment - (offset % max_alignment)) % max_alignment;
    return offset + tail_padding;
}

// Helper function: Calculate struct alignment requirement
size_t calculate_struct_alignment(const StructType *st)
{
    size_t max_alignment = 1;

    for (const auto &member : st->members())
    {
        size_t member_alignment = 1;

        if (member.type->is_struct())
        {
            member_alignment = calculate_struct_alignment(
                dynamic_cast<const StructType *>(member.type));
        }
        else
        {
            member_alignment = member.type->alignment();
        }

        max_alignment = std::max(max_alignment, member_alignment);
    }

    return max_alignment;
}

bool IRGenerator::struct_matches_ir(const StructType *ir_struct, const ast::StructType &ast_struct)
{
    if (ir_struct->members().size() != ast_struct.member_count())
    {
        MO_DEBUG("Member count mismatch: IR has %zu, AST has %zu",
                 ir_struct->members().size(), ast_struct.member_count());
        return false;
    }

    for (size_t i = 0; i < ir_struct->members().size(); ++i)
    {
        const auto &ir_member = ir_struct->members()[i];
        const auto &ast_member = ast_struct.get_member(i);

        // name check
        if (ir_member.name != ast_member.name)
        {
            MO_DEBUG("Member %zu name mismatch: IR '%s' vs AST '%s'",
                     i, ir_member.name.c_str(), ast_member.name.c_str());
            return false;
        }

        // type check
        Type *ast_member_ir_type = convert_type(*ast_member.type);
        if (!ast_member_ir_type)
        {
            MO_DEBUG("Failed to convert AST member type: %s",
                     ast_member.type->to_string().c_str());
            return false;
        }

        // strict eq check
        if (*ir_member.type != *ast_member_ir_type)
        {
            MO_DEBUG("Member %zu type mismatch:\nIR: %s\nAST: %s",
                     i,
                     ir_member.type->name().c_str(),
                     ast_member_ir_type->name().c_str());
            return false;
        }
    }

    return true;
}

Type *IRGenerator::convert_type(const ast::Type &ast_type)
{
    if (auto it = type_cache_.find(&ast_type); it != type_cache_.end())
    {
        return it->second;
    }

    Type *ir_type = nullptr;
    switch (ast_type.kind())
    {
    case ast::Type::Kind::Void:
    {
        ir_type = module_->get_void_type();
        break;
    }
    case ast::Type::Kind::Int:
    {
        auto &int_type = static_cast<const ast::IntegerType &>(ast_type);
        ir_type = module_->get_integer_type(int_type.bit_width());
        break;
    }
    case ast::Type::Kind::Float:
    {
        auto &float_type = static_cast<const ast::FloatType &>(ast_type);
        switch (float_type.precision())
        {
        case ast::FloatType::Precision::Half:
            ir_type = module_->get_float_type(FloatType::Half);
            break;
        case ast::FloatType::Precision::Single:
            ir_type = module_->get_float_type(FloatType::Single);
            break;
        case ast::FloatType::Precision::Double:
            ir_type = module_->get_float_type(FloatType::Double);
            break;
        case ast::FloatType::Precision::Quad:
            ir_type = module_->get_float_type(FloatType::Quad);
            break;
        default:
            assert(false && "Unsupported float precision");
        }
        break;
    }
    case ast::Type::Kind::Bool:
    {
        ir_type = module_->get_integer_type(1); // Represent bool as i1
        break;
    }
    case ast::Type::Kind::String:
    {
        ir_type = module_->get_pointer_type(module_->get_integer_type(8)); // Represent string as i8*
        break;
    }
    case ast::Type::Kind::Array:
    {
        auto &array_type = static_cast<const ast::ArrayType &>(ast_type);
        Type *elem_type = convert_type(array_type.element_type());
        ir_type = module_->get_array_type(elem_type, array_type.size());
        break;
    }
    case ast::Type::Kind::Tuple:
    {
        auto &tuple_type = static_cast<const ast::TupleType &>(ast_type);
        std::vector<MemberInfo> member_infos;
        unsigned int idx = 0;
        for (const auto &elem_type : tuple_type.element_types())
        {
            member_infos.push_back(MemberInfo(std::to_string(idx), convert_type(*elem_type)));
            idx = idx + 1;
        }
        // Tuple as anynomous struct
        ir_type = module_->get_struct_type("", member_infos);
        break;
    }
    case ast::Type::Kind::Alias:
    {
        auto &alias_type = static_cast<const ast::AliasType &>(ast_type);
        ir_type = convert_type(alias_type.target());
        break;
    }
    case ast::Type::Kind::Function:
    {
        auto &function_type = static_cast<const ast::FunctionType &>(ast_type);
        Type *return_type = convert_type(function_type.return_type());
        std::vector<Type *> param_types;
        for (const auto &param : function_type.params())
        {
            param_types.push_back(convert_type(*param));
        }
        ir_type = module_->get_function_type(return_type, param_types);
        break;
    }
    case ast::Type::Kind::Pointer:
    {
        auto &pointer_type = static_cast<const ast::PointerType &>(ast_type);
        Type *pointee_type = convert_type(pointer_type.pointee());
        ir_type = module_->get_pointer_type(pointee_type);
        break;
    }
    case ast::Type::Kind::Struct:
    {
        auto &struct_type = static_cast<const ast::StructType &>(ast_type);
        const std::string &struct_name = struct_type.name();

        if (auto existing = module_->try_get_struct_type(struct_name))
        {
            if (existing->is_opaque())
            {
                // 填充 opaque 结构体的成员
                std::vector<MemberInfo> member_infos;
                for (size_t i = 0; i < struct_type.member_count(); ++i)
                {
                    const auto &member = struct_type.get_member(i);
                    member_infos.push_back(MemberInfo(member.name, convert_type(*member.type)));
                }
                existing->set_body(member_infos);
                ir_type = existing;
            }
            else
            {
                // 检查现有结构体成员是否一致
                if (!struct_matches_ir(existing, struct_type))
                {
                    MO_ASSERT(false, "Conflicting struct definition for %s", struct_name.c_str());
                }
                ir_type = existing;
            }
        }
        else
        {
            // 先收集成员信息
            std::vector<MemberInfo> member_infos;
            for (size_t i = 0; i < struct_type.member_count(); ++i)
            {
                const auto &member = struct_type.get_member(i);
                member_infos.push_back(MemberInfo(member.name, convert_type(*member.type)));
            }
            // 创建并缓存结构体
            StructType *st = module_->get_struct_type(struct_name, member_infos);
            type_cache_[&ast_type] = st;
            ir_type = st;
        }
        break;
    }
    case ast::Type::Kind::Qualified:
    {
        auto &qualified_type = static_cast<const ast::QualifiedType &>(ast_type);
        ir_type = convert_type(qualified_type.base_type());
        break;
    }
    case ast::Type::Kind::Placeholder:
    {
        // Handle placeholder type (e.g., return an i32)
        ir_type = module_->get_integer_type(32);
        break;
    }
    default:
        assert(false && "Unsupported type kind");
    }

    type_cache_[&ast_type] = ir_type;

    return ir_type;
}

Value *IRGenerator::handle_conversion(Value *val, Type *target_type, bool is_explicit)
{
    MO_ASSERT(val != nullptr && target_type != nullptr, "Invalid operands");
    Type *source_type = val->type();
    MO_ASSERT(source_type != nullptr, "Invalid source type");
    MO_DEBUG("Converting %s to %s is_explicit=%d", val->type()->name().c_str(), target_type->name().c_str(), is_explicit);

    if (val->type()->is_struct() && !target_type->is_struct())
    {
        // error("Cannot implicitly convert tuple to scalar type");
        assert(false && "Invalid conversion");
    }

    if (val->type()->is_struct() && target_type->is_struct())
    {
        if (*val->type() != *target_type)
        {
            // error("Tuple type mismatch in conversion");
            assert(false && "Invalid conversion");
        }
        return val;
    }

    // No conversion needed if types are the same
    if (*source_type == *target_type)
        return val;

    // Enable strict standard checking in debug mode
    const bool strict_mode = true; // Configurable via compiler options

    if (val->type()->is_pointer() &&
        *val->type()->element_type() == *target_type &&
        target_type->is_struct())
    {
        return builder_.create_load(val, "structload");
    }

    // ---------------------------
    // Boolean Context Conversion (C11 6.3.1.2)
    // ---------------------------
    if (auto dst_bool = dynamic_cast<IntegerType *>(target_type))
    {
        if (dst_bool->bits() == 1)
        {
            // Pointer -> Boolean (including null pointer check)
            if (source_type->is_pointer())
            {
                Value *null_ptr = module_->get_constant_pointer_null(dynamic_cast<PointerType *>(source_type));
                return builder_.create_icmp(ICmpInst::Predicate::NE, val, null_ptr, "ptrbool");
            }

            // Float -> Boolean
            if (source_type->is_float())
            {
                Value *zero_fp = module_->get_constant_fp(source_type->as_float()->precision(), 0.0);
                return builder_.create_fcmp(FCmpInst::Predicate::ONE, val, zero_fp, "floatbool");
            }

            // Integer -> Boolean
            if (source_type->is_integer())
            {
                Value *zero_int = module_->get_constant_int(source_type->as_integer()->bits(), 0);
                return builder_.create_icmp(ICmpInst::Predicate::NE, val, zero_int, "intbool");
            }
        }
    }

    // ---------------------------
    // Pointer Conversions (C11 6.3.2.3)
    // ---------------------------
    if (target_type->is_pointer())
    {
        if (source_type->is_pointer())
        {
            // Allow implicit void* to other pointer conversions (GNU extension, disabled by default)
            bool is_void_ptr = source_type->as_pointer()->element_type()->is_void();
            if (strict_mode && !is_void_ptr && !is_explicit)
            {
                std::cerr << "Warning: implicit pointer cast from "
                          << source_type->name() << " to "
                          << target_type->name() << " requires explicit cast\n";
            }
            return builder_.create_bitcast(val, target_type, "ptrcast");
        }

        // Integer -> Pointer (requires explicit conversion)
        if (source_type->is_integer())
        {
            if (!is_explicit && strict_mode)
            {
                std::cerr << "Error: invalid implicit conversion from integer to pointer\n";
                assert(false && "Cast required");
            }
            return builder_.create_inttoptr(val, target_type, "int2ptr");
        }
    }

    // ---------------------------
    // Pointer <-> Integer (C11 6.3.2.3)
    // ---------------------------
    if (source_type->is_pointer() && target_type->is_integer())
    {
        // Only allow conversion to uintptr_t (assuming it's defined)
        // if (target_type->bits() != module_->get_data_layout().pointer_size_bits()) TODO: support this
        // {
        //     std::cerr << "Error: pointer to integer conversion requires exact size match (uintptr_t)\n";
        //     assert(false && "Invalid pointer conversion");
        // }
        return builder_.create_ptrtoint(val, target_type, "ptr2int");
    }

    // ---------------------------
    // Integer Promotion (C11 6.3.1.1)
    // ---------------------------
    if (auto src_int = dynamic_cast<IntegerType *>(source_type))
    {
        if (auto dst_int = dynamic_cast<IntegerType *>(target_type))
        {
            // Signedness handling strategy
            if (src_int->bits() < dst_int->bits())
            {
                // Sign extension only if source and destination types are signed
                if (src_int->is_signed() && dst_int->is_signed())
                {
                    return builder_.create_sext(val, target_type, "sext");
                }
                else
                {
                    // Zero extension (including mixed signedness)
                    return builder_.create_zext(val, target_type, "zext");
                }
            }

            // Narrowing conversion (may lose data)
            if (src_int->bits() > dst_int->bits() && strict_mode && !is_explicit)
            {
                std::cerr << "Warning: implicit truncation from "
                          << src_int->bits() << " bits to "
                          << dst_int->bits() << " bits\n";
            }
            return builder_.create_trunc(val, target_type, "trunc");
        }
    }

    // ---------------------------
    // Floating-Point Conversions (C11 6.3.1.4-6.3.1.5)
    // ---------------------------
    if (source_type->is_float() && target_type->is_float())
    {
        if (source_type->bits() < target_type->bits())
        {
            return builder_.create_fpext(val, target_type, "fpext");
        }
        else
        {
            if (strict_mode && !is_explicit)
            {
                std::cerr << "Warning: implicit floating-point truncation\n";
            }
            return builder_.create_fptrunc(val, target_type, "fptrunc");
        }
    }

    // ---------------------------
    // Floating-Point <-> Integer (C11 6.3.1.4)
    // ---------------------------
    if (source_type->is_float() && target_type->is_integer())
    {
        IntegerType *target_int_type = target_type->as_integer();
        if (strict_mode && !is_explicit)
        {
            std::cerr << "Warning: implicit conversion from floating to integer\n";
        }
        CastInst *cast = target_int_type->is_signed()
                             ? static_cast<CastInst *>(builder_.create_fptosi(val, target_type, "fptosi"))
                             : static_cast<CastInst *>(builder_.create_fptoui(val, target_type, "fptoui"));
        return cast;
    }

    if (source_type->is_integer() && target_type->is_float())
    {
        IntegerType *source_int_type = source_type->as_integer();
        CastInst *cast = source_int_type->is_signed()
                             ? static_cast<CastInst *>(builder_.create_sitofp(val, target_type, "sitofp"))
                             : static_cast<CastInst *>(builder_.create_uitofp(val, target_type, "uitofp"));
        return cast;
    }

    // ---------------------------
    // Unhandled Conversions
    // ---------------------------
    std::cerr << "Error: no viable conversion from "
              << source_type->name() << " to "
              << target_type->name() << "\n";
    assert(false && "Invalid implicit conversion");
    return nullptr;
}

Type *IRGenerator::dominant_type(Type *t1, Type *t2)
{
    if (t1->is_pointer())
        return t1;
    if (t2->is_pointer())
        return t2;

    if (t1->is_integer() && t2->is_integer())
    {
        return t1->bits() > t2->bits() ? t1 : t2;
    }
    if (t1->is_float() && t2->is_float())
    {
        return t1->bits() > t2->bits() ? t1 : t2;
    }
    if (t1->is_integer() && t2->is_float())
        return t2;
    if (t1->is_float() && t2->is_integer())
        return t1;
    assert(false && "Type mismatch in binary operation");
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Control Flow Handling
//===----------------------------------------------------------------------===//
void IRGenerator::push_loop(BasicBlock *cond_bb, BasicBlock *end_bb)
{
    loop_cond_stack_.push(cond_bb);
    loop_end_stack_.push(end_bb);
}

void IRGenerator::pop_loop()
{
    loop_cond_stack_.pop();
    loop_end_stack_.pop();
}

//===----------------------------------------------------------------------===//
// Statement Generation
//===----------------------------------------------------------------------===//
void IRGenerator::generate_stmt(const ast::Statement &stmt)
{
    if (!current_block())
    {
        MO_WARN("No current block to generate statement. Generation is skipped. Maybe in unreachable code?");
        return;
    }

    if (auto block_stmt = dynamic_cast<const ast::BlockStmt *>(&stmt))
    {
        handle_block(*block_stmt);
    }
    else if (auto var_decl_stmt = dynamic_cast<const ast::VarDeclStmt *>(&stmt))
    {
        handle_var_decl(*var_decl_stmt);
    }
    else if (auto if_stmt = dynamic_cast<const ast::IfStmt *>(&stmt))
    {
        handle_if(*if_stmt);
    }
    else if (auto while_stmt = dynamic_cast<const ast::WhileStmt *>(&stmt))
    {
        handle_loop(*while_stmt);
    }
    else if (auto return_stmt = dynamic_cast<const ast::ReturnStmt *>(&stmt))
    {
        handle_return(*return_stmt);
    }
    else if (auto break_stmt = dynamic_cast<const ast::BreakStmt *>(&stmt))
    {
        handle_break(*break_stmt);
    }
    else if (auto continue_stmt = dynamic_cast<const ast::ContinueStmt *>(&stmt))
    {
        handle_continue(*continue_stmt);
    }
    else if (auto expr_stmt = dynamic_cast<const ast::ExprStmt *>(&stmt))
    {
        generate_expr(*expr_stmt->expr);
    }
    else
    {
        assert(false && "Unsupported statement type");
    }
}

//===----------------------------------------------------------------------===//
// Return Statement Handling
//===----------------------------------------------------------------------===//
void IRGenerator::store_struct_recursive(Value *src, Value *dest_ptr)
{
    StructType *st = dynamic_cast<StructType *>(src->type());
    assert(st && "Storing non-struct type");

    for (size_t i = 0; i < st->members().size(); ++i)
    {
        Value *src_member = builder_.create_extract_value(src, {i}, "extract");
        Value *dest_gep = builder_.create_struct_gep(dest_ptr, i, "membergep");

        if (src_member->type()->is_struct())
        {
            store_struct_recursive(src_member, dest_gep);
        }
        else
        {
            builder_.create_store(src_member, dest_gep);
        }
    }
}

void IRGenerator::handle_return(const ast::ReturnStmt &stmt)
{
    if (stmt.value)
    {
        Value *ret_val = generate_expr(*stmt.value);
        Type *ret_type = ret_val->type();

        // check if need to return by a hidden ptr
        if (current_func_->has_hidden_retval())
        {
            Value *hidden_ptr = current_func_->arg(0);

            // handle recursive struct
            if (ret_type->is_struct())
            {
                store_struct_recursive(ret_val, hidden_ptr);
            }
            else
            {
                builder_.create_store(ret_val, hidden_ptr);
            }

            builder_.create_ret_void();
        }
        else
        {
            if (ret_val->type()->is_struct())
            {
                builder_.create_ret(ret_val);
            }
            // else if (ret_val->type()->is_void())
            // {
            //     builder_.create_ret_void();
            // }
            else
            {
                Type *target_type = current_func_->return_type();
                printf("target_type ptr addr: %p\n", target_type);
                auto void_ty = module_->get_void_type();
                printf("void_ty ptr addr: %p\n", void_ty);
                auto i32x3_ty = module_->get_array_type(module_->get_integer_type(32), 3);
                printf("i32x3_ty ptr addr: %p\n", i32x3_ty);
                target_type->name();

                ret_val = handle_conversion(ret_val, target_type);
                builder_.create_ret(ret_val);
            }
        }
    }
    else
    {
        builder_.create_ret_void();
    }
}

void IRGenerator::handle_block(const ast::BlockStmt &block)
{
    push_scope();
    for (const auto &stmt : block.statements)
    {
        generate_stmt(*stmt);
    }
    pop_scope();
}

void IRGenerator::handle_var_decl(const ast::VarDeclStmt &decl)
{
    MO_DEBUG("Declaring variable %s of type %s", decl.name.c_str(), decl.type->to_string().c_str());
    assert(decl.type && "Missing type in variable declaration. This should been resolved by the type checker.");
    Type *ir_type = convert_type(*decl.type);
    MO_DEBUG("Converted type to %s", ir_type->name().c_str());

    if (decl.type->kind() == ast::Type::Kind::Tuple)
    {
        AllocaInst *alloca = builder_.create_alloca(ir_type, decl.name);
        declare_symbol(decl.name, alloca);

        if (decl.init_expr)
        {
            MO_DEBUG("Type of decl.init_expr: %s", decl.init_expr->type->to_string().c_str());
            Value *init_val = generate_expr(*decl.init_expr);
            MO_DEBUG("Type of init_val: %s", init_val->type()->name().c_str());
            builder_.create_store(init_val, alloca);
        }

        return;
    }

    AllocaInst *alloca = builder_.create_alloca(ir_type, decl.name);
    declare_symbol(decl.name, alloca);

    if (decl.init_expr)
    {
        MO_DEBUG("Type of decl.init_expr: %s", decl.init_expr->type->to_string().c_str());
        Value *init_val = generate_expr(*decl.init_expr);
        MO_DEBUG("Type of init_val: %s", init_val->type()->name().c_str());
        if (decl.type->kind() == ast::Type::Kind::Array)
        {
            // Handle array initialization
            generate_array_init(alloca, *decl.init_expr);
        }
        else
        {
            builder_.create_store(init_val, alloca);
        }
    }
}

//===----------------------------------------------------------------------===//
// If Statement Handling with Short-Circuit
//===----------------------------------------------------------------------===//
void IRGenerator::handle_if(const ast::IfStmt &if_stmt)
{
    Value *cond_val = generate_expr(*if_stmt.condition);

    BasicBlock *then_bb = current_func_->create_basic_block("if.then");
    BasicBlock *else_bb = if_stmt.else_branch ? current_func_->create_basic_block("if.else") : nullptr;
    BasicBlock *merge_bb = current_func_->create_basic_block("if.merge");

    // Branch based on condition
    Value *cond = handle_conversion(cond_val, module_->get_integer_type(1));
    builder_.create_cond_br(cond, then_bb, else_bb ? else_bb : merge_bb);

    // Generate then block
    builder_.set_insert_point(then_bb);
    generate_stmt(*if_stmt.then_branch);
    bool then_terminated = then_bb->get_terminator() != nullptr;
    if (!then_terminated)
    {
        builder_.create_br(merge_bb);
    }

    // Generate else block if exists
    bool else_terminated = false;
    if (else_bb)
    {
        builder_.set_insert_point(else_bb);
        generate_stmt(*if_stmt.else_branch);
        else_terminated = else_bb->get_terminator() != nullptr;

        if (!else_terminated && current_block()->get_terminator())
        {
            builder_.create_unreachable(); // 显式标记不可达
            else_terminated = true;
        }

        if (!else_terminated)
        {
            builder_.create_br(merge_bb);
        }
    }

    // Determine if merge_bb is needed
    bool merge_needed = !(then_terminated && (else_bb ? else_terminated : true));
    if (merge_needed)
    {
        builder_.set_insert_point(merge_bb);
    }
    else
    {
        current_func_->remove_basic_block(merge_bb);
        if (builder_.get_insert_block() == merge_bb)
        {
            // builder_.clear_insert_point();
        }
    }
}

//===----------------------------------------------------------------------===//
// Loop Handling with Break/Continue Support
//===----------------------------------------------------------------------===//
void IRGenerator::handle_loop(const ast::WhileStmt &loop)
{
    BasicBlock *cond_bb = current_func_->create_basic_block("loop.cond");
    BasicBlock *body_bb = current_func_->create_basic_block("loop.body");
    BasicBlock *end_bb = current_func_->create_basic_block("loop.end");

    // Setup loop context
    push_loop(cond_bb, end_bb);

    // Generate initial condition check
    builder_.create_br(cond_bb);

    // Condition block
    builder_.set_insert_point(cond_bb);
    Value *cond_val = generate_expr(*loop.condition);
    Value *cond = handle_conversion(cond_val, module_->get_integer_type(1));
    builder_.create_cond_br(cond, body_bb, end_bb);

    // Body block
    builder_.set_insert_point(body_bb);
    generate_stmt(*loop.body);
    builder_.create_br(cond_bb); // Loop back

    // Cleanup
    builder_.set_insert_point(end_bb);
    pop_loop();
}

void IRGenerator::handle_expr_stmt(const ast::ExprStmt &stmt)
{
    generate_expr(*stmt.expr); // Discard expression value
}

void IRGenerator::handle_break(const ast::BreakStmt &)
{
    assert(!loop_end_stack_.empty() && "break outside loop");
    builder_.create_br(loop_end_stack_.top());
    // Create unreachable block for following code
    BasicBlock *after_break = current_func_->create_basic_block("after_break");
    builder_.set_insert_point(after_break);
}

void IRGenerator::handle_continue(const ast::ContinueStmt &)
{
    assert(!loop_cond_stack_.empty() && "continue outside loop");
    builder_.create_br(loop_cond_stack_.top());
    // Create unreachable block for following code
    BasicBlock *after_continue = current_func_->create_basic_block("after_continue");
    builder_.set_insert_point(after_continue);
}

//===----------------------------------------------------------------------===//
// Expression Generation
//===----------------------------------------------------------------------===//
Value *IRGenerator::generate_expr(const ast::Expr &expr)
{
    if (const auto *binary_expr = dynamic_cast<const ast::BinaryExpr *>(&expr))
    {
        return handle_binary(*binary_expr);
    }
    else if (const auto *call_expr = dynamic_cast<const ast::CallExpr *>(&expr))
    {
        return handle_call(*call_expr);
    }
    else if (const auto *variable_expr = dynamic_cast<const ast::VariableExpr *>(&expr))
    {
        return handle_variable(*variable_expr);
    }
    else if (const auto *int_lit = dynamic_cast<const ast::IntegerLiteralExpr *>(&expr))
    {
        return handle_integer_literal(*int_lit);
    }
    else if (const auto *float_lit = dynamic_cast<const ast::FloatLiteralExpr *>(&expr))
    {
        return handle_float_literal(*float_lit);
    }
    else if (const auto *str_lit = dynamic_cast<const ast::StringLiteralExpr *>(&expr))
    {
        return handle_string_literal(*str_lit);
    }
    else if (const auto *unary_expr = dynamic_cast<const ast::UnaryExpr *>(&expr))
    {
        return handle_unary(*unary_expr);
    }
    else if (const auto *cast_expr = dynamic_cast<const ast::CastExpr *>(&expr))
    {
        return handle_cast(*cast_expr);
    }
    else if (const auto *sizeof_expr = dynamic_cast<const ast::SizeofExpr *>(&expr))
    {
        return handle_sizeof(*sizeof_expr);
    }
    else if (const auto *addr_expr = dynamic_cast<const ast::AddressOfExpr *>(&expr))
    {
        return handle_address_of(*addr_expr);
    }
    else if (const auto *deref_expr = dynamic_cast<const ast::DerefExpr *>(&expr))
    {
        return handle_deref(*deref_expr);
    }
    else if (const auto *init_list = dynamic_cast<const ast::InitListExpr *>(&expr))
    {
        return handle_init_list(*init_list);
    }
    else if (const auto *fp_expr = dynamic_cast<const ast::FunctionPointerExpr *>(&expr))
    {
        return handle_function_pointer(*fp_expr);
    }
    else if (const auto *member_expr = dynamic_cast<const ast::MemberAccessExpr *>(&expr))
    {
        return handle_member_access(*member_expr);
    }
    else if (const auto *array_expr = dynamic_cast<const ast::ArrayAccessExpr *>(&expr))
    {
        return handle_array_access(*array_expr);
    }
    else if (const auto *struct_lit = dynamic_cast<const ast::StructLiteralExpr *>(&expr))
    {
        return handle_struct_literal(*struct_lit);
    }

    assert(false && "Unsupported expression type");
    return nullptr;
}

Value *IRGenerator::generate_lvalue(const ast::Expr &expr)
{
    // Handle variable expressions, return the allocated address
    if (const auto *var_expr = dynamic_cast<const ast::VariableExpr *>(&expr))
    {
        Value *addr = lookup_symbol(var_expr->identifier);
        MO_ASSERT(addr != nullptr, "Undefined variable '%s' in lvalue", var_expr->identifier.c_str());
        return addr;
    }
    // Handle dereference expressions, generate the rvalue of the pointer as the address
    else if (const auto *deref_expr = dynamic_cast<const ast::DerefExpr *>(&expr))
    {
        Value *ptr_val = generate_expr(*deref_expr->operand);
        MO_ASSERT(ptr_val->type()->is_pointer(), "Dereference requires pointer type");
        return ptr_val;
    }
    // Handle array access, generate the element address
    else if (const auto *array_access = dynamic_cast<const ast::ArrayAccessExpr *>(&expr))
    {
        Value *array_ptr = generate_lvalue(*array_access->array);
        Value *index = generate_expr(*array_access->index);

        // Generate GEP instruction to get the element address
        std::vector<Value *> indices = {builder_.get_int32(0), index};
        Value *elem_ptr = builder_.create_gep(array_ptr, indices, "array.elem.ptr");
        return elem_ptr;
    }
    // Handle structure member access, generate the member address
    else if (const auto *member_access = dynamic_cast<const ast::MemberAccessExpr *>(&expr))
    {
        Value *base_ptr = generate_lvalue(*member_access->object);
        MO_ASSERT(base_ptr->type()->is_pointer(), "Member access requires pointer to struct");

        // Get the structure type and verify member existence
        StructType *struct_ty = dynamic_cast<StructType *>(base_ptr->type()->element_type());
        MO_ASSERT(struct_ty != nullptr, "Base type is not a struct");
        unsigned member_idx = struct_ty->get_member_index(member_access->member);
        MO_ASSERT(member_idx != (unsigned)-1, "Struct member '%s' does not exist", member_access->member.c_str());

        // Generate GEP instruction to get the member address
        Value *member_ptr = builder_.create_struct_gep(base_ptr, member_idx, "struct.member.ptr");
        return member_ptr;
    }
    // Handle parenthesized expressions, recursively process the inner expression
    // else if (const auto *paren_expr = dynamic_cast<const ast::ParenExpr *>(&expr))
    // {
    //     return generate_lvalue(*paren_expr->inner);
    // }
    // Other types are not supported as lvalues
    else
    {
        MO_ASSERT(false, "Invalid lvalue expression type: %s", expr.name().c_str());
        return nullptr;
    }
}

// `Value *array_ptr` is a alloca inst
void IRGenerator::generate_array_init(AllocaInst *array_ptr, const ast::Expr &init_expr)
{
    if (auto init_list = dynamic_cast<const ast::InitListExpr *>(&init_expr))
    {
        for (size_t i = 0; i < init_list->members.size(); ++i)
        {
            Value *elem_ptr = builder_.create_gep(
                array_ptr,
                {builder_.get_int32(0), builder_.get_int32(i)},
                "array.elem");
            Value *init_val = generate_expr(*init_list->members[i]);
            builder_.create_store(init_val, elem_ptr);
        }
    }
    else
    {
        assert(false && "Unsupported array initializer");
    }
}

bool is_assignment_op(TokenType op)
{
    return op >= TokenType::Assign && op <= TokenType::RSAssign;
}

Value *IRGenerator::handle_binary(const ast::BinaryExpr &bin)
{
    if (is_assignment_op(bin.op))
    {
        return handle_compound_assign(bin);
    }
    // Short-cuit special cases
    if (bin.op == TokenType::And)
        return handle_logical_and(bin);
    if (bin.op == TokenType::Or)
        return handle_logical_or(bin);

    Value *lhs = generate_expr(*bin.left);
    Value *rhs = generate_expr(*bin.right);

    // Apply implicit conversions
    Type *target_type = dominant_type(lhs->type(), rhs->type());
    lhs = handle_conversion(lhs, target_type);
    rhs = handle_conversion(rhs, target_type);
    Type *lhs_type = lhs->type();
    Type *rhs_type = rhs->type();
    assert(*lhs_type == *rhs_type && "Type mismatch in binary operation");

    switch (bin.op)
    {
    case TokenType::Plus:
        return builder_.create_add(lhs, rhs, "add");
    case TokenType::Minus:
        return builder_.create_sub(lhs, rhs, "sub");
    case TokenType::Star:
        return builder_.create_mul(lhs, rhs, "mul");
    case TokenType::Slash:
        return builder_.create_sdiv(lhs, rhs, "div");
    case TokenType::Lt:
        if (lhs_type->is_integer())
            return builder_.create_icmp(ICmpInst::SLT, lhs, rhs, "icmp");
        return builder_.create_fcmp(FCmpInst::OLT, lhs, rhs, "fcmp");
    case TokenType::Le:
        if (lhs_type->is_integer())
            return builder_.create_icmp(ICmpInst::SLE, lhs, rhs, "icmp");
        return builder_.create_fcmp(FCmpInst::OLE, lhs, rhs, "fcmp");
    case TokenType::Gt:
        if (lhs_type->is_integer())
            return builder_.create_icmp(ICmpInst::SGT, lhs, rhs, "icmp");
        return builder_.create_fcmp(FCmpInst::OGT, lhs, rhs, "fcmp");
    case TokenType::Ge:
        if (lhs_type->is_integer())
            return builder_.create_icmp(ICmpInst::SGE, lhs, rhs, "icmp");
        return builder_.create_fcmp(FCmpInst::OGE, lhs, rhs, "fcmp");
    case TokenType::Eq:
        if (lhs_type->is_integer())
            return builder_.create_icmp(ICmpInst::EQ, lhs, rhs, "icmp");
        return builder_.create_fcmp(FCmpInst::OEQ, lhs, rhs, "fcmp");
    case TokenType::Ne:
        if (lhs_type->is_integer())
            return builder_.create_icmp(ICmpInst::NE, lhs, rhs, "icmp");
        return builder_.create_fcmp(FCmpInst::ONE, lhs, rhs, "fcmp");
    case TokenType::Ampersand:
        return builder_.create_bitand(lhs, rhs, "and");
    case TokenType::Pipe:
        return builder_.create_bitor(lhs, rhs, "or");
    case TokenType::Caret:
        return builder_.create_bitxor(lhs, rhs, "xor");
    case TokenType::Modulo:
        if (lhs_type->is_integer())
        {
            return lhs_type->is_signed() ? builder_.create_srem(lhs, rhs, "srem") : builder_.create_urem(lhs, rhs, "urem");
        }
        assert(false && "Modulo on non-integer type");
        break;

    case TokenType::LShift:
        return builder_.create_shl(lhs, rhs, "shl");

    case TokenType::RShift:
        if (lhs_type->is_signed())
        {
            return builder_.create_ashr(lhs, rhs, "ashr");
        }
        else
        {
            return builder_.create_lshr(lhs, rhs, "lshr");
        }
    default:
        MO_ASSERT(false, "Unexpected binary operator");
        return nullptr;
    }
    return nullptr;
}

Value *IRGenerator::handle_compound_assign(const ast::BinaryExpr &bin)
{
    Value *addr = generate_lvalue(*bin.left);
    Value *loaded_val = builder_.create_load(addr, "load_val");

    Value *rhs = generate_expr(*bin.right);
    rhs = handle_conversion(rhs, loaded_val->type());

    Value *result = nullptr;
    switch (bin.op)
    {
    case TokenType::AddAssign:
        result = builder_.create_add(loaded_val, rhs, "add_tmp");
        break;
    case TokenType::SubAssign:
        result = builder_.create_sub(loaded_val, rhs, "sub_tmp");
        break;
    case TokenType::MulAssign:
        result = builder_.create_mul(loaded_val, rhs, "mul_tmp");
        break;
    case TokenType::DivAssign:
        result = loaded_val->type()->is_signed() ? builder_.create_sdiv(loaded_val, rhs, "sdiv_tmp") : builder_.create_udiv(loaded_val, rhs, "udiv_tmp");
        break;
    case TokenType::ModAssign:
        result = loaded_val->type()->is_signed() ? builder_.create_srem(loaded_val, rhs, "srem_tmp") : builder_.create_urem(loaded_val, rhs, "urem_tmp");
        break;
    case TokenType::LSAssign:
        result = builder_.create_shl(loaded_val, rhs, "shl_tmp");
        break;
    case TokenType::RSAssign:
        result = loaded_val->type()->is_signed() ? builder_.create_ashr(loaded_val, rhs, "ashr_tmp") : builder_.create_lshr(loaded_val, rhs, "lshr_tmp");
        break;
    case TokenType::AndAssign:
        result = builder_.create_bitand(loaded_val, rhs, "and_tmp");
        break;
    case TokenType::OrAssign:
        result = builder_.create_bitor(loaded_val, rhs, "or_tmp");
        break;
    case TokenType::XorAssign:
        result = builder_.create_bitxor(loaded_val, rhs, "xor_tmp");
        break;
    default:
        MO_ASSERT(false, "Unsupported compound assignment %s", token_type_to_string(bin.op).c_str());
        return nullptr;
    }

    builder_.create_store(result, addr);
    return result;
}

//===----------------------------------------------------------------------===//
// Short-Circuit Logical Operators
//===----------------------------------------------------------------------===//
Value *IRGenerator::handle_logical_and(const ast::BinaryExpr &and_expr)
{
    BasicBlock *rhs_bb = current_func_->create_basic_block("and.rhs");
    BasicBlock *merge_bb = current_func_->create_basic_block("and.merge");

    Value *lhs_val = generate_expr(*and_expr.left);
    Value *cond = handle_conversion(lhs_val, module_->get_integer_type(1));
    builder_.create_cond_br(cond, rhs_bb, merge_bb);

    // Generate RHS in new block
    BasicBlock *save_block = current_block();
    builder_.set_insert_point(rhs_bb);
    Value *rhs_val = generate_expr(*and_expr.right);
    Value *rhs_cond = handle_conversion(rhs_val, module_->get_integer_type(1));
    builder_.create_br(merge_bb);

    // Create PHI node
    builder_.set_insert_point(merge_bb);
    PhiInst *phi = builder_.create_phi(module_->get_integer_type(1), "and.result");
    phi->add_incoming(builder_.get_int1(false), save_block);
    phi->add_incoming(rhs_cond, rhs_bb);
    return phi;
}

Value *IRGenerator::handle_logical_or(const ast::BinaryExpr &or_expr)
{
    BasicBlock *rhs_bb = current_func_->create_basic_block("or.rhs");
    BasicBlock *merge_bb = current_func_->create_basic_block("or.merge");

    Value *lhs_val = generate_expr(*or_expr.left);
    Value *cond = handle_conversion(lhs_val, module_->get_integer_type(1));
    builder_.create_cond_br(cond, merge_bb, rhs_bb);

    // Generate RHS in new block
    BasicBlock *save_block = current_block();
    builder_.set_insert_point(rhs_bb);
    Value *rhs_val = generate_expr(*or_expr.right);
    Value *rhs_cond = handle_conversion(rhs_val, module_->get_integer_type(1));
    builder_.create_br(merge_bb);

    // Create PHI node
    builder_.set_insert_point(merge_bb);
    PhiInst *phi = builder_.create_phi(module_->get_integer_type(1), "or.result");
    phi->add_incoming(builder_.get_int1(true), save_block);
    phi->add_incoming(rhs_cond, rhs_bb);
    return phi;
}

Value *IRGenerator::handle_integer_literal(const ast::IntegerLiteralExpr &expr)
{
    return builder_.get_int32(expr.value);
}

Value *IRGenerator::handle_float_literal(const ast::FloatLiteralExpr &expr)
{
    return builder_.get_float(expr.value);
}

Value *IRGenerator::handle_string_literal(const ast::StringLiteralExpr &expr)
{
    ConstantString *const_str = module_->get_constant_string(expr.value);

    GlobalVariable *str = module_->create_global_variable(
        const_str->type(),
        true, // constant
        const_str,
        "str");

    return builder_.create_bitcast(str, module_->get_pointer_type(module_->get_integer_type(8)), "str.cast");
}

Value *IRGenerator::handle_unary(const ast::UnaryExpr &expr)
{
    Value *operand = generate_expr(*expr.operand);
    if (!operand)
    {
        return nullptr;
    }

    switch (expr.op)
    {
    case TokenType::Plus:
        return operand;

    case TokenType::Minus:
    {
        Type *ty = operand->type();
        if (ty->is_integer())
        {
            Value *zero = module_->get_constant_zero(ty);
            return builder_.create_sub(zero, operand, "neg");
        }
        else if (ty->is_float())
        {
            MO_ASSERT(false, "Unary minus for float is not supported yet");
            // return builder_.create_fneg(operand, "fneg");
        }
        else
        {
            MO_DEBUG("Invalid operand type for unary minus");
            return nullptr;
        }
    }

    case TokenType::Not:
    {
        Type *ty = operand->type();
        if (ty->is_integer())
        {
            Value *zero = module_->get_constant_zero(ty);
            return builder_.create_icmp(ICmpInst::EQ, operand, zero, "not");
        }
        else if (ty->is_float())
        {
            Value *zero = module_->get_constant_zero(ty);
            return builder_.create_fcmp(FCmpInst::OEQ, operand, zero, "fnot");
        }
        else
        {
            MO_DEBUG("Invalid operand type for logical not");
            return nullptr;
        }
    }

    case TokenType::Tilde:
    {
        if (operand->type()->is_integer())
        {
            unsigned bitwidth = operand->type()->as_integer()->bits();
            // Value *all_ones = ConstantInt::get(operand->type(), -1);
            Value *all_ones = module_->get_constant_int(bitwidth, -1);
            return builder_.create_bitxor(operand, all_ones, "bitnot");
        }
        else
        {
            MO_DEBUG("Bitwise complement requires integer operand");
            return nullptr;
        }
    }

    case TokenType::Ampersand:
    {
        Value *addr = generate_lvalue(*expr.operand);
        if (!addr)
        {
            MO_DEBUG("Cannot take address of non-lvalue");
            return nullptr;
        }
        return addr;
    }

    case TokenType::Star:
    {
        if (!operand->type()->is_pointer())
        {
            MO_DEBUG("Operand of dereference must be a pointer");
            return nullptr;
        }
        return builder_.create_load(operand, "deref");
    }

    default:
        MO_DEBUG("Invalid unary operator");
        return nullptr;
    }
}

Value *IRGenerator::handle_cast(const ast::CastExpr &expr)
{
    Value *src_val = generate_expr(*expr.expr);
    Type *target_type = convert_type(*expr.target_type);
    return builder_.create_cast(src_val, target_type, "cast");
}

Value *IRGenerator::handle_sizeof(const ast::SizeofExpr &expr)
{
    if (expr.kind == ast::SizeofExpr::Kind::Type)
    {
        Type *ty = convert_type(*expr.target_type);
        return builder_.get_int32(ty->size());
    }
    else if (expr.kind == ast::SizeofExpr::Kind::Expr)
    {
        Type *target_expr_type = convert_type(*expr.target_expr->type);
        return builder_.get_int32(target_expr_type->size());
    }
    else
    {
        assert(false && "Invalid sizeof expression");
        return nullptr;
    }
}

Value *IRGenerator::handle_address_of(const ast::AddressOfExpr &expr)
{
    Value *operand = generate_expr(*expr.operand);
    // Operand should be a load instruction for variables
    if (auto load = dynamic_cast<LoadInst *>(operand))
    {
        return load->pointer();
    }
    assert(false && "Can't take address of non-lvalue");
    return nullptr;
}

Value *IRGenerator::handle_deref(const ast::DerefExpr &expr)
{
    Value *ptr = generate_expr(*expr.operand);
    return builder_.create_load(ptr, "deref");
}

Value *IRGenerator::handle_init_list(const ast::InitListExpr &expr)
{
    MO_ASSERT(expr.type != nullptr, "InitListExpr must have a type");

    if (expr.type->kind() == ast::Type::Kind::Tuple)
    {
        AllocaInst *temp = builder_.create_alloca(convert_type(*expr.type), "tuple.tmp");

        for (size_t i = 0; i < expr.members.size(); ++i)
        {
            Value *elem_ptr = builder_.create_struct_gep(
                temp,
                i,
                "tuple.elem.ptr");
            Value *elem_val = generate_expr(*expr.members[i]);
            builder_.create_store(elem_val, elem_ptr);
        }

        return builder_.create_load(temp, "tuple.val");
    }
    // Only support constant initialization
    return generate_constant_initializer(expr);
}

Value *IRGenerator::handle_function_pointer(const ast::FunctionPointerExpr &expr)
{
    // Function pointers are treated as regular pointers
    Type *fn_type = convert_type(*expr.type);
    return module_->get_constant_pointer_null(static_cast<PointerType *>(fn_type));
}

Value *IRGenerator::handle_struct_literal(const ast::StructLiteralExpr &expr)
{
    assert(expr.type && "Struct type not resolved. There could be a bug in the type checker");
    StructType *st = static_cast<StructType *>(convert_type(*expr.type));
    AllocaInst *temp = builder_.create_alloca(st, "struct.tmp");

    for (const auto &member : expr.members)
    {
        Value *member_ptr = builder_.create_struct_gep(
            temp,
            st->get_member_index(member.first),
            "member.ptr");
        Value *value = generate_expr(*member.second);
        builder_.create_store(value, member_ptr);
    }

    return builder_.create_load(temp, "struct.val");
}

//===----------------------------------------------------------------------===//
// Function Declaration (Forward Declaration)
//===----------------------------------------------------------------------===//
void IRGenerator::declare_function(const ast::FunctionDecl &func)
{
    // 1. Convert return type and check struct return ABI requirements
    Type *orig_return_type = convert_type(*func.return_type);
    bool need_hidden_ptr = orig_return_type->is_struct() &&
                           (orig_return_type->size() > 16); // x86-64 SysV ABI

    // 2. Create parameter types list
    std::vector<Type *> param_types;

    // 2a. Add hidden struct return pointer if needed
    if (need_hidden_ptr)
    {
        param_types.push_back(module_->get_pointer_type(orig_return_type));
    }

    // 2b. Convert regular parameters
    for (const auto &param : func.params)
    {
        param_types.push_back(convert_type(*param.type));
    }

    // 3. Create function type
    Type *actual_return_type = need_hidden_ptr ? module_->get_void_type()
                                               : orig_return_type;
    FunctionType *func_type = module_->get_function_type(actual_return_type, param_types);

    // 4. Create function object with proper linkage
    Function *func_ir = module_->create_function(func.name, func_type);

    // 5. Set ABI attributes
    if (need_hidden_ptr)
    {
        func_ir->set_hidden_retval(orig_return_type);
    }

    // 6. Register in current scope (supports nested functions)
    declare_symbol(func.name, func_ir);
}

//===----------------------------------------------------------------------===//
// Function Body Generation
//===----------------------------------------------------------------------===//
void IRGenerator::generate_function_body(const ast::FunctionDecl &func)
{
    // 1. Lookup the pre-declared function
    Value *func_val = lookup_symbol(func.name);
    assert(func_val && "Function not declared. Call declare_function first!");

    current_func_ = dynamic_cast<Function *>(func_val);
    assert(current_func_ && "Symbol is not a function");

    // 2. Create entry basic block
    BasicBlock *entry_bb = current_func_->create_basic_block("entry");
    builder_.set_insert_point(entry_bb);

    // 3. Parameter handling
    size_t arg_idx = 0;

    // 3a. Handle hidden return pointer
    if (current_func_->has_hidden_retval())
    {
        Value *hidden_arg = current_func_->arg(arg_idx++);
        hidden_arg->set_name(".sret");
        // Not stored in symbol table (internal use only)
    }

    // 3b. Handle 'this' parameter for methods
    bool is_method = false;
    if (!func.params.empty() &&
        (func.params[0].name == "this" || func.params[0].name == "self"))
    {
        is_method = true;
        Value *this_arg = current_func_->arg(arg_idx++);
        this_arg->set_name("this");

        AllocaInst *this_alloca = builder_.create_alloca(
            this_arg->type(), "this.addr");
        builder_.create_store(this_arg, this_alloca);
        declare_symbol(func.params[0].name, this_alloca);
    }

    // 3c. Handle regular parameters
    for (size_t i = is_method ? 1 : 0; i < func.params.size(); ++i)
    {
        const auto &param = func.params[i];
        Value *arg = current_func_->arg(arg_idx++);
        arg->set_name(param.name);

        AllocaInst *alloca = builder_.create_alloca(
            arg->type(), param.name + ".addr");
        builder_.create_store(arg, alloca);
        declare_symbol(param.name, alloca);
    }

    // 4. Generate function body
    push_scope();
    for (const auto &stmt : func.body)
    {
        generate_stmt(*stmt);
    }
    pop_scope();

    // 5. Add implicit return if needed
    if (current_block() && !current_block()->get_terminator())
    {
        if (current_func_->return_type()->is_void())
        {
            builder_.create_ret_void();
        }
        else
        {
            MO_ASSERT(false, "Non-void function '{}' missing return statement %s", func.name.c_str());
        }
    }

    // 6. Cleanup
    current_func_ = nullptr;
}

//===----------------------------------------------------------------------===//
// Implementation Blocks
//===----------------------------------------------------------------------===//
void IRGenerator::generate_impl_block(const ast::ImplBlock &impl)
{
    for (const auto &method : impl.methods)
    {
        generate_function_body(*method);
    }
}

//===----------------------------------------------------------------------===//
// Function Call Handling
//===----------------------------------------------------------------------===//

Value *IRGenerator::handle_call(const ast::CallExpr &call)
{
    Value *callee_value = generate_expr(*call.callee);
    MO_ASSERT(callee_value->type()->is_pointer(), "Callee is not a pointer");
    PointerType *callee_ptr_type = static_cast<PointerType *>(callee_value->type());
    FunctionType *callee_func_type = static_cast<FunctionType *>(callee_ptr_type->element_type());

    if (call.args.size() != callee_func_type->num_params())
    {
        MO_ASSERT(false, "Invalid number of arguments in function call");
    }

    std::vector<Value *> args;
    for (const auto &arg_expr : call.args)
    {
        Value *arg_val = generate_expr(*arg_expr);

        // Handle pass-by-reference for arrays/structs
        if (arg_expr->type->is_aggregate())
        {
            Value *ptr = lookup_symbol(
                static_cast<const ast::VariableExpr &>(*arg_expr).identifier);
            args.push_back(ptr);
        }
        else
        {
            args.push_back(arg_val);
        }

        // FIXME: maybe handle method calls here?
    }

    Value *result = builder_.create_call(callee_value, args, "calltmp");
    return result;
}

//===----------------------------------------------------------------------===//
// Variable Handling
//===----------------------------------------------------------------------===//
Value *IRGenerator::handle_variable(const ast::VariableExpr &var)
{
    Value *alloca = lookup_symbol(var.identifier);
    MO_ASSERT(alloca != nullptr, "Undefined variable: %s", var.identifier.c_str());

    return builder_.create_load(alloca, "load.global");
}

//===----------------------------------------------------------------------===//
// Memory Operations
//===----------------------------------------------------------------------===//
Value *IRGenerator::handle_member_access(const ast::MemberAccessExpr &expr)
{
    if (expr.is_method_call)
    {
        return handle_method_invocation(expr);
    }
    else
    {
        return handle_field_access(expr);
    }
}

// Value* IRGenerator::handle_member_access(const ast::MemberAccessExpr& expr) {
//     Value* base = generate_expr(*expr.object);
//     PointerType* ptr_type = cast<PointerType>(base->type());
//     StructType* struct_type = cast<StructType>(ptr_type->element_type());

//     unsigned index = struct_type->get_member_index(expr.member);
//     Value* member_ptr = builder_.create_struct_gep(base, index, "memberptr");

//     if (expr.is_call) {
//         return member_ptr;  // 方法调用直接返回函数指针
//     }
//     return builder_.create_load(member_ptr, "memberval");
// }

Value *IRGenerator::handle_method_invocation(const ast::MemberAccessExpr &expr)
{
    Value *base = generate_expr(*expr.object);
    std::vector<Value *> args;
    ast::FunctionDecl *ast_func = expr.resolved_func;
    if (!ast_func->is_static)
    {
        Value *this_ptr = handle_this_pointer(base, expr.accessor);
        args.push_back(this_ptr);
    }

    for (const auto &arg : expr.args)
    {
        args.push_back(generate_expr(*arg));
    }

    if (ast_func)
    {
        auto func_val = lookup_symbol(ast_func->name);
        if (Function *func = dynamic_cast<Function *>(func_val))
        {
            return builder_.create_call(func, args, "method.call");
        }
        else
        {
            assert(false && "Invalid function type");
        }
    }
    else
    {
        Value *func_ptr = handle_field_load(base, expr.member);
        return builder_.create_indirect_call(func_ptr, args, "method.call");
    }
}

Value *IRGenerator::handle_field_load(Value *base, const std::string &member)
{
    // Get struct type from base pointer
    PointerType *base_ptr_type = dynamic_cast<PointerType *>(base->type());
    assert(base_ptr_type && "Field access on non-pointer type");

    StructType *struct_type = dynamic_cast<StructType *>(base_ptr_type->element_type());
    assert(struct_type && "Base type is not a struct");

    // Get member index and generate GEP
    unsigned member_idx = struct_type->get_member_index(member);
    assert(member_idx != (unsigned)-1 && "Invalid struct member access");

    Value *member_ptr = builder_.create_struct_gep(base, member_idx, "field");
    return builder_.create_load(member_ptr, "loaded_func_ptr");
}

Value *IRGenerator::handle_this_pointer(Value *base, TokenType accessor)
{
    // 处理不同访问符号的 this 指针
    switch (accessor)
    {
    case TokenType::Arrow:
        return builder_.create_load(base); // 解引用指针
    case TokenType::Dot:
        return base; // 直接使用对象地址
    default:
        assert(false && "Invalid accessor for method call");
    }
}

Value *IRGenerator::handle_field_access(const ast::MemberAccessExpr &access)
{
    Value *base = generate_expr(*access.object);
    Type *base_type = base->type()->element_type();

    if (auto struct_type = dynamic_cast<StructType *>(base_type))
    {
        // Verify access is for instance members
        assert(struct_type->has_member(access.member) &&
               "Struct member does not exist");

        unsigned index = struct_type->get_member_index(access.member);
        return builder_.create_struct_gep(base, index, "member");
    }

    assert(false && "Invalid member access type");
    return nullptr;
}

void IRGenerator::validate_receiver_type(
    const ast::FunctionDecl &method,
    StructType *expected_type)
{
    if (!method.params.empty() &&
        (method.params[0].name == "this" || method.params[0].name == "self"))
    {
        Type *declared_type = convert_type(*method.params[0].type);
        assert(declared_type->is_pointer() &&
               "Receiver must be pointer type");

        StructType *actual_struct = dynamic_cast<StructType *>(
            declared_type->element_type());
        assert(actual_struct == expected_type &&
               "Receiver type mismatch in impl block");
    }
}

Value *IRGenerator::handle_array_access(const ast::ArrayAccessExpr &access)
{
    Value *base_ptr = generate_expr(*access.array);
    Value *index_val = generate_expr(*access.index);

    std::vector<Value *> indices = {
        builder_.get_int32(0), // Array pointer decay
        index_val};

    // return builder_.create_gep(base_ptr, indices, "arrayidx");
    Value *elem_ptr = builder_.create_gep(base_ptr, indices, "arrayidx");
    return builder_.create_load(elem_ptr, "arrayval");
}

//===----------------------------------------------------------------------===//
// Global Variable Handling
//===----------------------------------------------------------------------===//
void IRGenerator::generate_global(const ast::GlobalDecl &global)
{
    Type *type = convert_type(*global.type);
    Constant *init = nullptr;

    if (global.init_expr)
    {
        init = generate_constant_initializer(*global.init_expr);
    }

    GlobalVariable *gv = module_->create_global_variable(
        type,
        global.is_const,
        init,
        global.name);

    sym_table_stack_[0][global.name] = gv; // Add to global scope
}

//===----------------------------------------------------------------------===//
// Program Entry Point
//===----------------------------------------------------------------------===//
void IRGenerator::generate(const ast::Program &program)
{
    // Process global variables first
    for (const auto &global : program.globals)
    {
        generate_global(*global);
    }

    // Process struct declarations
    for (const auto &struct_decl : program.structs)
    {
        convert_type(*struct_decl->type()); // Force struct type creation
    }

    // Stage 1. Declare all functions
    for (const auto &func : program.functions)
    {
        declare_function(*func);
    }
    for (const auto &impl : program.impl_blocks)
    {
        for (const auto &method : impl->methods)
        {
            declare_function(*method);
        }
    }

    // Stage 2. Generate function bodies
    for (const auto &func : program.functions)
    {
        generate_function_body(*func);
    }
    for (const auto &impl : program.impl_blocks)
    {
        generate_impl_block(*impl);
    }
}

//===----------------------------------------------------------------------===//
// Constant Initialization
//===----------------------------------------------------------------------===//
Constant *IRGenerator::generate_constant_initializer(const ast::Expr &expr)
{
    if (auto int_lit = dynamic_cast<const ast::IntegerLiteralExpr *>(&expr))
    {
        return module_->get_constant_int(
            module_->get_integer_type(32),
            int_lit->value);
    }

    if (auto float_lit = dynamic_cast<const ast::FloatLiteralExpr *>(&expr))
    {
        return module_->get_constant_fp(
            module_->get_float_type(FloatType::Single),
            float_lit->value);
    }

    if (auto struct_lit = dynamic_cast<const ast::StructLiteralExpr *>(&expr))
    {
        std::vector<Constant *> members;
        for (const auto &member : struct_lit->members)
        {
            members.push_back(generate_constant_initializer(*member.second));
        }
        return module_->get_constant_struct(
            static_cast<StructType *>(convert_type(*struct_lit->type)),
            members);
    }

    if (auto init_list = dynamic_cast<const ast::InitListExpr *>(&expr))
    {
        Type *target_type = convert_type(*init_list->type);

        // Handle array initialization
        if (auto array_type = dynamic_cast<ArrayType *>(target_type))
        {
            std::vector<Constant *> elements;
            for (const auto &member : init_list->members)
            {
                // Verify array size matches initialization list
                MO_ASSERT(elements.size() < array_type->size(),
                          "Too many elements in array initializer");

                elements.push_back(generate_constant_initializer(*member));
            }

            // Fill remaining elements with zeros
            while (elements.size() < array_type->size())
            {
                elements.push_back(module_->get_constant_zero(array_type->element_type()));
            }

            return module_->get_constant_array(array_type, elements);
        }
        // Handle struct initialization
        else if (auto struct_type = dynamic_cast<StructType *>(target_type))
        {
            std::vector<Constant *> members;
            for (const auto &member : init_list->members)
            {
                members.push_back(generate_constant_initializer(*member));
            }
            return module_->get_constant_struct(struct_type, members);
        }

        MO_ASSERT(false, "Unsupported init list type: %s",
                  target_type->name().c_str());
    }

    MO_ASSERT(false, "Unsupported constant initializer for '%s'", expr.name().c_str());
    return nullptr;
}
