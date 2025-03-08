#pragma once

#include "ir.h"
#include <sstream>

class IRPrinter
{
public:
    static void print_module(const Module &module, std::ostream &os)
    {
        for (const auto &struct_ty : module.struct_types())
        {
            print_struct_type(*struct_ty, os);
        }
        for (const auto &global_var : module.global_variables())
        {
            print_global_variable(*global_var, os);
        }
        for (const auto &function : module.functions())
        {
            print_function(*function, os);
        }
    }

    static void print_struct_type(const StructType &struct_ty, std::ostream &os)
    {
        os << struct_ty.name() << " = type { ";
        const auto &members = struct_ty.members();
        for (size_t i = 0; i < members.size(); ++i)
        {
            if (i != 0)
            {
                os << ", ";
            }
            os << members[i].type->name();
        }
        os << " }\n";
    }

    static void print_global_variable(const GlobalVariable &global_var, std::ostream &os)
    {
        os << "@" << global_var.name() << " = ";
        if (global_var.is_constant())
        {
            os << "constant ";
        }
        else
        {
            os << "global ";
        }
        os << global_var.type()->name() << " ";
        if (global_var.initializer())
        {
            os << global_var.initializer()->as_string();
        }
        else
        {
            os << "zeroinitializer";
        }
        os << "\n";
    }

    static void print_function(const Function &function, std::ostream &os)
    {
        os << "define " << function.return_type()->name() << " @" << function.name() << "(";
        for (size_t i = 0; i < function.num_args(); ++i)
        {
            if (i != 0)
            {
                os << ", ";
            }
            os << function.arg(i)->type()->name() << " " << format_value(function.arg(i));
        }
        os << ") {\n";
        for (const auto &bb : function.basic_blocks())
        {
            print_basic_block(*bb, os);
        }
        os << "}\n";
    }

    static void print_basic_block(const BasicBlock &bb, std::ostream &os)
    {
        os << bb.name() << ":\n";
        for (auto inst = bb.first_instruction(); inst; inst = inst->next())
        {
            print_instruction(*inst, os);
        }
    }

    static void print_instruction(const Instruction &inst, std::ostream &os)
    {
        switch (inst.opcode())
        {
        case Opcode::Alloca:
        {
            auto alloca_inst = static_cast<const AllocaInst &>(inst);
            os << "  " << format_value(&inst) << " = alloca " << alloca_inst.allocated_type()->name() << "\n";
            break;
        }
        case Opcode::Load:
        {
            auto load_inst = static_cast<const LoadInst &>(inst);
            os << "  " << format_value(&inst) << " = load " << load_inst.type()->name() << ", "
               << load_inst.pointer()->type()->name() << " " << format_value(load_inst.pointer()) << "\n";
            break;
        }
        case Opcode::Store:
        {
            auto store_inst = static_cast<const StoreInst &>(inst);
            os << "  store " << store_inst.value()->type()->name() << " " << format_value(store_inst.value()) << ", "
               << store_inst.pointer()->type()->name() << " " << format_value(store_inst.pointer()) << "\n";
            break;
        }
        case Opcode::Ret:
        {
            auto ret_inst = static_cast<const ReturnInst &>(inst);
            if (ret_inst.value())
            {
                os << "  ret " << ret_inst.value()->type()->name() << " " << format_value(ret_inst.value()) << "\n";
            }
            else
            {
                os << "  ret void\n";
            }
            break;
        }
        case Opcode::Br:
        case Opcode::CondBr:
        {
            auto br_inst = static_cast<const BranchInst &>(inst);
            if (br_inst.is_conditional())
            {
                os << "  br i1 " << format_value(br_inst.operand(0)) << ", label " << format_value(br_inst.get_true_successor())
                   << ", label " << format_value(br_inst.get_false_successor()) << "\n";
            }
            else
            {
                os << "  br label " << format_value(br_inst.get_true_successor()) << "\n";
            }
            break;
        }
        case Opcode::Unreachable:
        {
            os << "  unreachable\n";
            break;
        }
        case Opcode::Add:
        case Opcode::Sub:
        case Opcode::Mul:
        case Opcode::UDiv:
        case Opcode::SDiv:
        case Opcode::SRem:
        case Opcode::URem:
        {
            auto binary_inst = static_cast<const BinaryInst &>(inst);
            os << "  " << format_value(&inst) << " = " << get_opcode_str(inst.opcode()) << " "
               << binary_inst.left()->type()->name() << " ";
            os << format_value(binary_inst.left());
            os << ", ";
            os << format_value(binary_inst.right());
            os << "\n";
            break;
        }
        case Opcode::ICmp:
        {
            auto icmp_inst = static_cast<const ICmpInst &>(inst);
            os << "  " << format_value(&inst) << " = icmp " << get_icmp_predicate_str(icmp_inst.predicate()) << " "
               << icmp_inst.operand(0)->type()->name() << " " << format_value(icmp_inst.operand(0)) << ", " << format_value(icmp_inst.operand(1)) << "\n";
            break;
        }
        case Opcode::FCmp:
        {
            auto fcmp_inst = static_cast<const FCmpInst &>(inst);
            os << "  " << format_value(&inst) << " = fcmp " << get_fcmp_predicate_str(fcmp_inst.predicate()) << " "
               << fcmp_inst.operand(0)->type()->name() << " " << format_value(fcmp_inst.operand(0)) << ", " << format_value(fcmp_inst.operand(1)) << "\n";
            break;
        }
        case Opcode::GetElementPtr:
        {
            auto gep_inst = static_cast<const GetElementPtrInst &>(inst);
            auto *ptr_type = gep_inst.base_pointer()->type()->as_pointer();
            Type *element_type = ptr_type->element_type();
            os << "  " << format_value(&inst) << " = getelementptr " << element_type->name() << ", "
               << ptr_type->name() << " " << format_value(gep_inst.base_pointer()) << ", ";
            for (size_t i = 0; i < gep_inst.indices().size(); ++i)
            {
                if (i != 0)
                {
                    os << ", ";
                }
                os << gep_inst.indices()[i]->type()->name() << " " << format_value(gep_inst.indices()[i]);
            }
            os << "\n";
            break;
        }
        case Opcode::Phi:
        {
            auto phi_inst = static_cast<const PhiInst &>(inst);
            os << "  " << format_value(&inst) << " = phi " << phi_inst.type()->name() << " ";
            for (unsigned i = 0; i < phi_inst.num_incoming(); ++i)
            {
                if (i != 0)
                {
                    os << ", ";
                }
                os << "[ " << format_value(phi_inst.get_incoming_value(i)) << ", " << format_value(phi_inst.get_incoming_block(i)) << " ]";
            }
            os << "\n";
            break;
        }
        case Opcode::ZExt:
        case Opcode::SExt:
        case Opcode::Trunc:
        case Opcode::SIToFP:
        case Opcode::FPToSI:
        case Opcode::FPExt:
        case Opcode::FPTrunc:
        case Opcode::BitCast:
        case Opcode::PtrToInt:
        case Opcode::IntToPtr:
        case Opcode::FPToUI:
        case Opcode::UIToFP:
        {
            auto conv_inst = static_cast<const ConversionInst &>(inst);
            os << "  " << format_value(&inst) << " = " << get_opcode_str(inst.opcode()) << " "
               << conv_inst.get_source()->type()->name() << " " << format_value(conv_inst.get_source()) << " to " << conv_inst.get_dest_type()->name() << "\n";
            break;
        }
        case Opcode::Shl:
        case Opcode::LShr:
        case Opcode::AShr:
        case Opcode::BitAnd:
        case Opcode::BitOr:
        case Opcode::BitXor:
        {
            auto binary_inst = static_cast<const BinaryInst &>(inst);
            os << "  " << format_value(&inst) << " = " << get_opcode_str(inst.opcode()) << " "
               << binary_inst.left()->type()->name() << " ";
            os << format_value(binary_inst.left());
            os << ", ";
            os << format_value(binary_inst.right());
            os << "\n";
            break;
        }
        case Opcode::Call:
        {
            auto call_inst = static_cast<const CallInst &>(inst);
            os << "  " << format_value(&inst) << " = call " << call_inst.called_function()->return_type()->name() << " @" << call_inst.called_function()->name() << "(";
            auto args = call_inst.arguments();
            for (unsigned i = 0; i < args.size(); ++i)
            {
                if (i != 0)
                {
                    os << ", ";
                }
                os << args[i]->type()->name() << " " << format_value(args[i]);
            }
            os << ")\n";
            break;
        }
        case Opcode::Neg:
        case Opcode::Not:
        case Opcode::FNeg:
        case Opcode::BitNot:
        {
            auto unary_inst = static_cast<const UnaryInst &>(inst);
            os << "  " << format_value(&inst) << " = " << get_opcode_str(inst.opcode()) << " "
               << unary_inst.get_operand()->type()->name() << " " << format_value(unary_inst.get_operand()) << "\n";
            break;
        }
        }
        MO_ASSERT(false, "Invalid instruction %s", inst.name().c_str());
    }
    static std::string get_opcode_str(Opcode op)
    {
        switch (op)
        {
        case Opcode::Add:
            return "add";
        case Opcode::Sub:
            return "sub";
        case Opcode::Mul:
            return "mul";
        case Opcode::UDiv:
            return "udiv";
        case Opcode::SDiv:
            return "sdiv";
        case Opcode::URem:
            return "urem";
        case Opcode::SRem:
            return "srem";
        case Opcode::ZExt:
            return "zext";
        case Opcode::SExt:
            return "sext";
        case Opcode::Trunc:
            return "trunc";
        case Opcode::SIToFP:
            return "sitofp";
        case Opcode::FPToSI:
            return "fptosi";
        case Opcode::FPExt:
            return "fpext";
        case Opcode::FPTrunc:
            return "fptrunc";
        case Opcode::BitCast:
            return "bitcast";
        case Opcode::BitAnd:
            return "and";
        case Opcode::BitOr:
            return "or";
        case Opcode::BitXor:
            return "xor";
        case Opcode::Shl:
            return "shl";
        case Opcode::LShr:
            return "lshr";
        case Opcode::AShr:
            return "ashr";
        case Opcode::PtrToInt:
            return "ptrtoint";
        case Opcode::Alloca:
            return "alloca";
        case Opcode::Load:
            return "load";
        case Opcode::Store:
            return "store";
        case Opcode::GetElementPtr:
            return "getelementptr";
        case Opcode::ICmp:
            return "icmp";
        case Opcode::FCmp:
            return "fcmp";
        case Opcode::Br:
            return "br";
        case Opcode::CondBr:
            return "br";
        case Opcode::Unreachable:
            return "unreachable";
        case Opcode::Ret:
            return "ret";
        case Opcode::Phi:
            return "phi";
        case Opcode::Call:
            return "call";
        case Opcode::IntToPtr:
            return "inttoptr";
        case Opcode::FPToUI:
            return "fptoui";
        case Opcode::UIToFP:
            return "uitofp";
        case Opcode::Neg:
            return "neg";
        case Opcode::Not:
            return "not";
        case Opcode::FNeg:
            return "fneg";
        case Opcode::BitNot:
            return "bitnot";
        }

        assert(false && "Invalid predicate");
        return "<err>";
    }

    static std::string get_icmp_predicate_str(ICmpInst::Predicate pred)
    {
        switch (pred)
        {
        case ICmpInst::EQ:
            return "eq";
        case ICmpInst::NE:
            return "ne";
        case ICmpInst::SLT:
            return "slt";
        case ICmpInst::SLE:
            return "sle";
        case ICmpInst::SGT:
            return "sgt";
        case ICmpInst::SGE:
            return "sge";
        case ICmpInst::ULT:
            return "ult";
        case ICmpInst::ULE:
            return "ule";
        case ICmpInst::UGT:
            return "ugt";
        case ICmpInst::UGE:
            return "uge";
        }

        assert(false && "Invalid predicate");
        return "<err>";
    }

    static std::string get_fcmp_predicate_str(FCmpInst::Predicate pred)
    {
        switch (pred)
        {
        case FCmpInst::EQ:
            return "eq";
        case FCmpInst::NE:
            return "ne";
        case FCmpInst::OEQ:
            return "oeq";
        case FCmpInst::ONE:
            return "one";
        case FCmpInst::LT:
            return "lt";
        case FCmpInst::LE:
            return "le";
        case FCmpInst::GT:
            return "gt";
        case FCmpInst::GE:
            return "ge";
        case FCmpInst::OLT:
            return "olt";
        case FCmpInst::OLE:
            return "ole";
        case FCmpInst::OGT:
            return "ogt";
        case FCmpInst::OGE:
            return "oge";
        }

        assert(false && "Invalid predicate");
        return "<err>";
    }

    static std::string format_value(const Value *value)
    {
        MO_ASSERT(value, "Invalid value");
        if (auto *constant = dynamic_cast<const Constant *>(value))
        {
            return constant->as_string();
        }
        else
        {
            return "%" + value->name();
        }
    }
};
