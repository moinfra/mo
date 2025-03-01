// ast_type.h - AST type nodes
#pragma once

#include <memory>
#include <vector>
#include <unordered_map>
#include <string>
#include <cassert>
#include <cstdint>
#include <stdexcept>

#define MO_DEFAULT_INT_BITWIDTH (32)
#define MO_DEFAULT_FLOAT_PRECISION (32)

namespace ast
{
    //===----------------------------------------------------------------------===//
    //                             Forward Declarations
    //===----------------------------------------------------------------------===//

    class Type;
    class ScalarType;
    class IntType;
    class FloatType;
    class BoolType;
    class StringType;
    class PointerType;
    class ArrayType;
    class FunctionType;
    class StructType;
    class AliasType;
    class QualifiedType;

    using TypePtr = std::unique_ptr<Type>;

    //===----------------------------------------------------------------------===//
    //                             Utility Classes
    //===----------------------------------------------------------------------===//
    enum class Qualifier : uint8_t
    {
        Const = 1,
        Volatile = 1 << 1,
        Restrict = 1 << 2
    };

    constexpr Qualifier operator|(Qualifier a, Qualifier b) noexcept
    {
        return static_cast<Qualifier>(static_cast<uint8_t>(a) | static_cast<uint8_t>(b));
    }

    constexpr Qualifier operator&(Qualifier a, Qualifier b) noexcept
    {
        return static_cast<Qualifier>(static_cast<uint8_t>(a) & static_cast<uint8_t>(b));
    }

    struct TypedField
    {
        std::string name;
        TypePtr type;

        TypedField(std::string name, TypePtr type);
        TypedField(const TypedField &other);
        TypedField &operator=(TypedField other) noexcept;

        friend void swap(TypedField &a, TypedField &b) noexcept;
    };

    //===----------------------------------------------------------------------===//
    //                            Abstract Base Class: Type
    //===----------------------------------------------------------------------===//
    class Type
    {
    public:
        enum class Kind
        {
            Placeholder,
            Void,
            Int,
            Float,
            Bool,
            String,
            Pointer,
            Array,
            Function,
            Struct,
            Alias,
            Qualified
        };

        virtual ~Type() = default;

        // Type characteristic queries
        virtual Kind kind() const noexcept = 0;
        virtual TypePtr clone() const = 0;
        virtual bool equals(const Type *other) const noexcept = 0;

        // Type conversion methods
        virtual IntType *as_int() noexcept { return nullptr; }
        virtual const IntType *as_int() const noexcept { return nullptr; }
        virtual FloatType *as_float() noexcept { return nullptr; }
        virtual const FloatType *as_float() const noexcept { return nullptr; }
        virtual BoolType *as_bool() noexcept { return nullptr; }
        virtual const BoolType *as_bool() const noexcept { return nullptr; }
        virtual StringType *as_string() noexcept { return nullptr; }
        virtual const StringType *as_string() const noexcept { return nullptr; }
        virtual PointerType *as_pointer() noexcept { return nullptr; }
        virtual const PointerType *as_pointer() const noexcept { return nullptr; }
        virtual ArrayType *as_array() noexcept { return nullptr; }
        virtual const ArrayType *as_array() const noexcept { return nullptr; }
        virtual FunctionType *as_function() noexcept { return nullptr; }
        virtual const FunctionType *as_function() const noexcept { return nullptr; }
        virtual StructType *as_struct() noexcept { return nullptr; }
        virtual const StructType *as_struct() const noexcept { return nullptr; }
        virtual AliasType *as_alias() noexcept { return nullptr; }
        virtual const AliasType *as_alias() const noexcept { return nullptr; }
        virtual QualifiedType *as_qualified() noexcept { return nullptr; }
        virtual const QualifiedType *as_qualified() const noexcept { return nullptr; }

        // Utility methods
        virtual bool is_scalar() const noexcept { return false; }
        bool is_aggregate() const noexcept
        {
            const auto k = kind();
            return k == Kind::Struct || k == Kind::Array;
        }

        // Factory methods
        static TypePtr create_placeholder();
        static TypePtr create_void();
        static TypePtr create_bool();
        static TypePtr create_int(uint8_t bit_width = MO_DEFAULT_INT_BITWIDTH);
        static TypePtr create_float(uint8_t precision_bits = MO_DEFAULT_FLOAT_PRECISION);
        static TypePtr create_string();
        static TypePtr create_pointer(TypePtr pointee);
        static TypePtr create_array(TypePtr element, int size);
        static TypePtr create_function(TypePtr return_type, std::vector<TypePtr> params);
        static TypePtr create_struct(std::string name, std::vector<TypedField> members);
        static TypePtr create_alias(std::string name, TypePtr target);
        static TypePtr create_qualified(Qualifier q, TypePtr base);
    };

    //===----------------------------------------------------------------------===//
    //                             Abstract Base Class for Scalar Types
    //===----------------------------------------------------------------------===//
    class PlaceholderType : public Type
    {
    public:
        Kind kind() const noexcept override { return Kind::Placeholder; }
        TypePtr clone() const override { return std::make_unique<PlaceholderType>(); }
        bool equals(const Type *other) const noexcept override
        {
            return other->kind() == Kind::Placeholder;
        }
    };

    class ScalarType : public Type
    {
    public:
        // Public method
        bool is_scalar() const noexcept override { return true; }

    protected:
        ~ScalarType() = default; // Prevent direct instantiation
    };

    //===----------------------------------------------------------------------===//
    //                             Integer Type
    //===----------------------------------------------------------------------===//
    class IntType : public ScalarType
    {
    public:
        explicit IntType(int bit_width)
            : bit_width_(bit_width)
        {
            assert(bit_width > 0 && "Invalid bit width");
        }

        // Type conversion
        IntType *as_int() noexcept override { return this; }
        const IntType *as_int() const noexcept override { return this; }

        Kind kind() const noexcept override { return Kind::Int; }
        size_t bit_width() const noexcept { return bit_width_; }

        TypePtr clone() const override
        {
            return std::make_unique<IntType>(bit_width_);
        }

        bool equals(const Type *other) const noexcept override
        {
            if (other->kind() != Kind::Int)
                return false;
            return bit_width_ == static_cast<const IntType *>(other)->bit_width_;
        }

    private:
        size_t bit_width_;
    };

    //===----------------------------------------------------------------------===//
    //                             // FloatiPoint Type
    //===----------------------------------------------------------------------===//
    class FloatType : public ScalarType
    {
    public:
        enum class Precision: uint8_t
        {
            Half = 1 << 4,   // 16-bit
            Single = 1 << 5, // 32-bit
            Double = 1 << 6, // 64-bit
            Quad = 1 << 7    // 128-bit
        };

        explicit FloatType(Precision prec)
            : precision_(prec) {}

        // Type conversion
        FloatType *as_float() noexcept override { return this; }
        const FloatType *as_float() const noexcept override { return this; }

        Kind kind() const noexcept override { return Kind::Float; }
        Precision precision() const noexcept { return precision_; }

        TypePtr clone() const override
        {
            return std::make_unique<FloatType>(precision_);
        }

        bool equals(const Type *other) const noexcept override
        {
            if (other->kind() != Kind::Float)
                return false;
            return precision_ == static_cast<const FloatType *>(other)->precision_;
        }

    private:
        Precision precision_;
    };

    //===----------------------------------------------------------------------===//
    //                             Boolean Type
    //===----------------------------------------------------------------------===//
    class BoolType : public ScalarType
    {
    public:
        BoolType() = default;

        // Type conversion
        BoolType *as_bool() noexcept override { return this; }
        const BoolType *as_bool() const noexcept override { return this; }

        Kind kind() const noexcept override { return Kind::Bool; }

        TypePtr clone() const override
        {
            return std::make_unique<BoolType>();
        }

        bool equals(const Type *other) const noexcept override
        {
            return other->kind() == Kind::Bool;
        }
    };

    //===----------------------------------------------------------------------===//
    //                             String Type
    //===----------------------------------------------------------------------===//
    class StringType : public Type
    {
    public:
        // Type conversion
        StringType *as_string() noexcept override { return this; }
        const StringType *as_string() const noexcept override { return this; }

        Kind kind() const noexcept override { return Kind::String; }

        TypePtr clone() const override
        {
            return std::make_unique<StringType>();
        }

        bool equals(const Type *other) const noexcept override
        {
            return other->kind() == Kind::String;
        }
    };

    //===----------------------------------------------------------------------===//
    //                             Pointer Type
    //===----------------------------------------------------------------------===//
    class PointerType : public Type
    {
    public:
        explicit PointerType(TypePtr pointee)
            : pointee_(std::move(pointee)) {}

        // Type conversion
        PointerType *as_pointer() noexcept override { return this; }
        const PointerType *as_pointer() const noexcept override { return this; }

        Kind kind() const noexcept override { return Kind::Pointer; }
        const Type &pointee() const noexcept { return *pointee_; }

        TypePtr clone() const override
        {
            return std::make_unique<PointerType>(pointee_->clone());
        }

        bool equals(const Type *other) const noexcept override
        {
            if (other->kind() != Kind::Pointer)
                return false;
            const auto *o = static_cast<const PointerType *>(other);
            return pointee_->equals(o->pointee_.get());
        }

    private:
        TypePtr pointee_;
    };

    //===----------------------------------------------------------------------===//
    //                             Array Type
    //===----------------------------------------------------------------------===//
    class ArrayType : public Type
    {
    public:
        ArrayType(TypePtr element, int size)
            : element_(std::move(element)), size_(size)
        {
            assert(size >= -1 && "Invalid array size");
        }

        // Type conversion
        ArrayType *as_array() noexcept override { return this; }
        const ArrayType *as_array() const noexcept override { return this; }

        // Type characteristics
        Kind kind() const noexcept override { return Kind::Array; }
        const Type &element_type() const noexcept { return *element_; }
        int size() const noexcept { return size_; } // -1 indicates unspecified size

        // Cloning operation
        TypePtr clone() const override
        {
            return std::make_unique<ArrayType>(element_->clone(), size_);
        }

        // Type comparison
        bool equals(const Type *other) const noexcept override
        {
            if (other->kind() != Kind::Array)
                return false;
            const auto *o = static_cast<const ArrayType *>(other);
            return size_ == o->size_ && element_->equals(o->element_.get());
        }

    private:
        TypePtr element_;
        int size_;
    };

    //===----------------------------------------------------------------------===//
    //                             Struct Type
    //===----------------------------------------------------------------------===//
    class StructType : public Type
    {
    public:
        StructType(std::string name, std::vector<TypedField> members)
            : name_(std::move(name)), members_(std::move(members))
        {
            validate_members();
            build_index();
        }

        const std::string &name() const noexcept { return name_; }

        size_t member_count() const noexcept { return members_.size(); }
        const TypedField &get_member(size_t index) const { return members_.at(index); }
        const Type *find_member(const std::string &name) const
        {
            if (auto it = member_indexes_.find(name); it != member_indexes_.end())
            {
                return members_[it->second].type.get();
            }
            return nullptr;
        }

        bool equals(const Type *other) const noexcept override
        {
            if (other->kind() != Kind::Struct)
                return false;
            const auto *o = static_cast<const StructType *>(other);

            if (name_ != o->name_)
                return false;
            if (members_.size() != o->members_.size())
                return false;

            for (size_t i = 0; i < members_.size(); ++i)
            {
                const auto &m = members_[i];
                const auto &om = o->members_[i];
                if (m.name != om.name)
                    return false;
                if (!m.type->equals(om.type.get()))
                    return false;
            }
            return true;
        }

        Kind kind() const noexcept override { return Kind::Struct; }

        TypePtr clone() const override
        {
            std::vector<TypedField> cloned_members;
            for (const auto &m : members_)
            {
                cloned_members.emplace_back(m.name, m.type->clone());
            }
            return std::make_unique<StructType>(name_, std::move(cloned_members));
        }

    private:
        void validate_members() const
        {
            for (const auto &m : members_)
            {
                if (m.name.empty())
                {
                    throw std::invalid_argument("Struct member must have name");
                }
                if (!m.type)
                {
                    throw std::invalid_argument("Struct member type cannot be null");
                }
            }
        }

        void build_index()
        {
            for (size_t i = 0; i < members_.size(); ++i)
            {
                const auto &name = members_[i].name;
                auto [it, inserted] = member_indexes_.emplace(name, i);
                if (!inserted)
                {
                    throw std::invalid_argument("Duplicate struct member: " + name);
                }
            }
        }

        std::string name_;
        std::vector<TypedField> members_;
        std::unordered_map<std::string, size_t> member_indexes_;
    };

    //===----------------------------------------------------------------------===//
    //                             Function Type
    //===----------------------------------------------------------------------===//
    class FunctionType : public Type
    {
    public:
        FunctionType(TypePtr return_type,
                     std::vector<TypePtr> params)
            : return_type_(std::move(return_type)),
              params_(std::move(params))
        {
            validate_parameters();
        }

        // Type conversion
        FunctionType *as_function() noexcept override { return this; }
        const FunctionType *as_function() const noexcept override { return this; }

        // Type characteristics
        Kind kind() const noexcept override { return Kind::Function; }
        const Type &return_type() const noexcept { return *return_type_; }
        const std::vector<TypePtr> &params() const noexcept { return params_; }

        // Cloning operation
        TypePtr clone() const override
        {
            std::vector<TypePtr> cloned_params;
            for (const auto &p : params_)
            {
                cloned_params.emplace_back(p->clone());
            }
            return std::make_unique<FunctionType>(
                return_type_->clone(),
                std::move(cloned_params));
        }

        // Type comparison
        bool equals(const Type *other) const noexcept override
        {
            if (other->kind() != Kind::Function)
                return false;
            const auto *o = static_cast<const FunctionType *>(other);

            if (!return_type_->equals(o->return_type_.get()))
                return false;
            if (params_.size() != o->params_.size())
                return false;

            for (size_t i = 0; i < params_.size(); ++i)
            {
                if (!params_[i]->equals(o->params_[i].get()))
                    return false;
            }
            return true;
        }

    private:
        void validate_parameters() const
        {
            for (const auto &p : params_)
            {
                assert(p && "Function parameter type cannot be null");
            }
        }

        TypePtr return_type_;
        std::vector<TypePtr> params_;
    };

    //===----------------------------------------------------------------------===//
    //                             Alias Type
    //===----------------------------------------------------------------------===//
    class AliasType : public Type
    {
    public:
        AliasType(std::string name, TypePtr target)
            : name_(std::move(name)),
              target_(std::move(target)) // target can be unresolved in the parsing stage
        {
            assert(!name_.empty() && "Alias name cannot be empty");
        }

        // Type conversion
        AliasType *as_alias() noexcept override { return this; }
        const AliasType *as_alias() const noexcept override { return this; }

        // Type characteristics
        Kind kind() const noexcept override { return Kind::Alias; }
        const std::string &name() const noexcept { return name_; }
        const Type &target() const noexcept { return *target_; }

        // Cloning operation
        TypePtr clone() const override
        {
            assert(target_ && "Alias target cannot be null. Is it unresolved?");
            return std::make_unique<AliasType>(name_, target_->clone());
        }

        // Type comparison (compares alias names directly)
        bool equals(const Type *other) const noexcept override
        {
            if (other->kind() != Kind::Alias)
                return false;
            const auto *o = static_cast<const AliasType *>(other);
            return name_ == o->name_;
        }

    private:
        std::string name_;
        TypePtr target_;
    };

    //===----------------------------------------------------------------------===//
    //                             Qualified Type
    //===----------------------------------------------------------------------===//
    class QualifiedType : public Type
    {
    public:
        QualifiedType(Qualifier q, TypePtr base)
            : qualifiers_(q), base_(std::move(base)) {}

        // Type conversion
        QualifiedType *as_qualified() noexcept override { return this; }
        const QualifiedType *as_qualified() const noexcept override { return this; }

        // Type characteristics
        Kind kind() const noexcept override { return Kind::Qualified; }
        Qualifier qualifiers() const noexcept { return qualifiers_; }
        const Type &base_type() const noexcept { return *base_; }
        Type &base_type() noexcept { return *base_; }

        // Cloning operation
        TypePtr clone() const override
        {
            return std::make_unique<QualifiedType>(qualifiers_, base_->clone());
        }

        // Type comparison
        bool equals(const Type *other) const noexcept override
        {
            if (other->kind() != Kind::Qualified)
                return false;
            const auto *o = static_cast<const QualifiedType *>(other);
            return qualifiers_ == o->qualifiers_ &&
                   base_->equals(o->base_.get());
        }

    private:
        Qualifier qualifiers_;
        TypePtr base_;
    };
} // namespace ast
