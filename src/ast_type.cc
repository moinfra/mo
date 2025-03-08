#include "ast_type.h"

namespace ast
{
    //===----------------------------------------------------------------------===//
    // TypedField
    //===----------------------------------------------------------------------===//

    TypedField::TypedField(std::string name, TypePtr type)
        : name(std::move(name)), type(std::move(type)) {}

    TypedField::TypedField(const TypedField &other)
        : name(other.name), type(other.type ? other.type->clone() : nullptr)
    {
        assert(type != nullptr && "TypedField type cannot be null");
    }

    TypedField &TypedField::operator=(TypedField other) noexcept
    {
        swap(*this, other);
        return *this;
    }

    void swap(TypedField &a, TypedField &b) noexcept
    {
        using std::swap;
        swap(a.name, b.name);
        swap(a.type, b.type);
    }
    //===----------------------------------------------------------------------===//
    // Type
    //===----------------------------------------------------------------------===//
    TypePtr Type::create_placeholder()
    {
        return std::make_unique<PlaceholderType>();
    }

    TypePtr Type::create_void()
    {
        class VoidType : public Type
        {
        public:
            Kind kind() const noexcept override { return Kind::Void; }
            TypePtr clone() const override { return std::make_unique<VoidType>(); }
            bool equals(const Type *other) const noexcept override
            {
                return other->kind() == Kind::Void;
            }

            std::string to_string() const override { return "void"; }
        };
        return std::make_unique<VoidType>();
    }

    TypePtr Type::create_bool()
    {
        return std::make_unique<BoolType>();
    }

    TypePtr Type::create_int(uint8_t bit_width, bool unsigned_)
    {
        if (bit_width == 1)
        {
            MO_WARN("Use create_bool() instead of create_int(1, false)");
            return std::make_unique<BoolType>();
        }
        return std::make_unique<IntegerType>(bit_width, unsigned_);
    }

    TypePtr Type::create_float(uint8_t bit_width)
    {
        return std::make_unique<FloatType>(bit_width);
    }

    TypePtr Type::create_string()
    {
        return std::make_unique<StringType>();
    }

    TypePtr Type::create_pointer(TypePtr pointee)
    {
        return std::make_unique<PointerType>(std::move(pointee));
    }

    TypePtr Type::create_array(TypePtr element, int size)
    {
        return std::make_unique<ArrayType>(std::move(element), size);
    }

    TypePtr Type::create_tuple(std::vector<TypePtr> elem_types)
    {
        return std::make_unique<TupleType>(std::move(elem_types));
    }

    TypePtr Type::create_function(TypePtr return_type, std::vector<TypePtr> params)
    {
        if (!return_type)
        {
            return_type = create_void();
        }
        return std::make_unique<FunctionType>(std::move(return_type), std::move(params));
    }

    TypePtr Type::create_struct(std::string name, std::vector<TypedField> members)
    {
        return std::make_unique<StructType>(std::move(name), std::move(members));
    }

    TypePtr Type::create_alias(std::string name)
    {
        return std::make_unique<AliasType>(std::move(name));
    }

    TypePtr Type::create_qualified(Qualifier q, TypePtr base)
    {
        return std::make_unique<QualifiedType>(q, std::move(base));
    }

    std::string VoidType::to_string() const
    {
        return "()";
    }

    std::string PlaceholderType::to_string() const
    {
        return "!";
    }

    std::string IntegerType::to_string() const
    {
        return (unsigned_ ? "u" : "i") + std::to_string(bit_width_);
    }

    std::string FloatType::to_string() const
    {
        return "f" + std::to_string(bit_width_);
    }

    std::string BoolType::to_string() const
    {
        return "bool";
    }

    std::string StringType::to_string() const
    {
        return "str";
    }

    std::string PointerType::to_string() const
    {
        const Type *pointee_type = pointee_.get();
        if (const QualifiedType *q = pointee_type->as_qualified())
        {
            if (static_cast<uint8_t>(q->qualifiers() & Qualifier::Const))
            {
                return "*const " + q->base_type().to_string();
            }
            else
            {
                return "*" + q->base_type().to_string();
            }
        }
        else
        {
            return "*" + pointee_type->to_string();
        }
    }

    std::string ArrayType::to_string() const
    {
        if (size_ == -1)
        {
            return "[" + element_->to_string() + "]";
        }
        else
        {
            return "[" + element_->to_string() + "; " + std::to_string(size_) + "]";
        }
    }

    std::string TupleType::to_string() const
    {
        std::string result = "(";
        for (size_t i = 0; i < elements_.size(); ++i)
        {
            if (i > 0)
            {
                result += ", ";
            }
            result += elements_[i]->to_string();
        }
        if (elements_.size() == 1)
        {
            result += ",";
        }
        result += ")";
        return result;
    }

    std::string StructType::to_string() const
    {
        return name_;
    }

    std::string FunctionType::to_string() const
    {
        std::string params_str;
        for (size_t i = 0; i < params_.size(); ++i)
        {
            if (i > 0)
                params_str += ", ";
            params_str += params_[i]->to_string();
        }
        return "fn(" + params_str + ") -> " + return_type_->to_string();
    }

    std::string AliasType::to_string() const
    {
        return name_;
    }

    std::string QualifiedType::to_string() const
    {
        std::string qual_str;
        if ((qualifiers_ & Qualifier::Const) != Qualifier{0})
        {
            qual_str += "const ";
        }

        if ((qualifiers_ & Qualifier::Volatile) != Qualifier{0})
        {
            qual_str += "volatile ";
        }

        if ((qualifiers_ & Qualifier::Restrict) != Qualifier{0})
        {
            qual_str += "restrict ";
        }

        return qual_str + base_->to_string();
    }

}
