#pragma once

#include "ir.h"
#include <vector>
#include <string>
#include <map>

//===----------------------------------------------------------------------===//
// Stack Object Metadata
//===----------------------------------------------------------------------===//

struct FrameObjectMetadata
{
    enum Flags : uint8_t
    {
        IsFixedSize = 1 << 0,    // Fixed-size object (normal local variable)
        IsVariableSize = 1 << 1, // Variable-size object (e.g., alloca)
        IsSpillSlot = 1 << 2,    // Register spill slot
        NeedsRealign = 1 << 3,   // Requires special alignment (e.g., SIMD type)
        IsStackGuard = 1 << 4    // Stack guard region
    };

    int64_t size;                      // Object size in bytes (-1 for runtime)
    unsigned alignment;                // Minimum required alignment
    uint8_t flags;                     // Attribute flags
    Value *associated_value = nullptr; // Associated IR object
    unsigned spill_rc_id = 0;          // 溢出目标的寄存器类别 ID
    bool spill_needs_reload = true;    // 是否需重新加载

    // Validity check
    bool validate(std::string *err = nullptr) const;
};

class MachineFrame
{
private:
    std::unordered_map<int, std::unique_ptr<FrameObjectMetadata>> frame_objects_; // Key is frame index
    int next_frame_idx_ = 0;                                                      // Stack object index counter

    // Stack frame layout cache
    mutable bool is_frame_layout_dirty_ = true;             // Layout cache status
    mutable std::vector<int> layout_order_;                 // Layout order cache
    mutable std::unordered_map<int, size_t> frame_offsets_; // 缓存各对象的偏移量
    mutable size_t total_frame_size_ = 0;                   // 缓存的总堆栈大小

    int create_impl(Value *value, int64_t size, unsigned alignment, int flags)
    {
        FrameObjectMetadata meta;
        meta.size = size;
        meta.alignment = alignment;
        meta.flags = static_cast<uint8_t>(flags);
        meta.associated_value = value;
        if (!meta.validate())
        {
            throw std::runtime_error("Invalid frame object metadata");
        }
        return create_frame_object(std::move(meta));
    }

    FrameObjectMetadata *get_metadata(int index)
    {
        auto it = frame_objects_.find(index);
        return (it != frame_objects_.end()) ? it->second.get() : nullptr;
    }

public:
    MachineFrame() = default;

    std::vector<std::pair<int, FrameObjectMetadata>> objects() const;

    // Frame index management
    int create_frame_object(Value *value);
    int get_frame_index(Value *value) const;
    bool has_frame_index(Value *value) const;
    int create_frame_object(FrameObjectMetadata frame_object_info);
    const FrameObjectMetadata *get_frame_object(int index) const;
    size_t get_frame_index_offset(int index) const;
    size_t get_total_frame_size() const;
    bool is_valid_index(int index) const;
    // Stack frame layout
    const std::vector<int> &frame_layout() const;

    // factory methods
    int create_fixed_size(Value *value, int64_t size, unsigned alignment);

    // 创建可变大小对象（如 alloca）
    int create_variable_size(Value *value, unsigned alignment);

    // 创建 SIMD 变量（固定大小 + 高对齐）
    int create_simd_variable(Value *value, int64_t size, unsigned simd_alignment);

    // 创建溢出槽（固定大小 + 溢出标记）
    int create_spill_slot(unsigned regClassID, int64_t size, unsigned alignment);
};
