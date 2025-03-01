#include "ast_type.h"

namespace ast
{
    //===----------------------------------------------------------------------===//
    // TypedField
    //===----------------------------------------------------------------------===//

    TypedField::TypedField(std::string name, TypePtr type)
        : name(std::move(name)), type(std::move(type)) {}

    TypedField::TypedField(const TypedField &other)
        : name(other.name), type(other.type ? other.type->clone() : nullptr) {}

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
        };
        return std::make_unique<VoidType>();
    }

    TypePtr Type::create_bool()
    {
        return std::make_unique<BoolType>();
    }

    TypePtr Type::create_int(uint8_t bit_width)
    {
        return std::make_unique<IntType>(bit_width);
    }

    TypePtr Type::create_float(uint8_t precision_bits)
    {
        FloatType::Precision prec;
        switch (precision_bits)
        {
        case 16:
        case 32:
        case 64:
        case 128:
            prec = static_cast<FloatType::Precision>(precision_bits);
            break;
        default:
            throw std::invalid_argument("Unsupported float precision");
        }
        return std::make_unique<FloatType>(prec);
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

    TypePtr Type::create_alias(std::string name, TypePtr target)
    {
        return std::make_unique<AliasType>(std::move(name), std::move(target));
    }

    TypePtr Type::create_qualified(Qualifier q, TypePtr base)
    {
        return std::make_unique<QualifiedType>(q, std::move(base));
    }

}
