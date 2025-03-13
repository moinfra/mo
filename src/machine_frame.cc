#include "machine_frame.h"

//===----------------------------------------------------------------------===//
// FrameObjectMetadata Implementation
//===----------------------------------------------------------------------===//

bool FrameObjectMetadata::validate(std::string *err) const
{
    if (size < -1)
    {
        if (err)
            *err = "Invalid size value";
        return false;
    }
    if ((flags & IsVariableSize) && size != -1)
    {
        if (err)
            *err = "Variable-size object must have size=-1";
        return false;
    }
    return true;
}
//===----------------------------------------------------------------------===//
// MachineFrame Implementation
//===----------------------------------------------------------------------===//

int MachineFrame::create_frame_object(FrameObjectMetadata info)
{
    if (!info.validate())
        return -1;

    int index = next_frame_idx_++;
    frame_objects_[index] = std::make_unique<FrameObjectMetadata>(std::move(info));
    is_frame_layout_dirty_ = true;
    return index;
}

const FrameObjectMetadata *MachineFrame::get_frame_object(int idx) const
{
    auto it = frame_objects_.find(idx);
    return it != frame_objects_.end() ? it->second.get() : nullptr;
}

const std::vector<int> &MachineFrame::frame_layout() const
{
    if (is_frame_layout_dirty_)
    {
        // 1. 清空缓存
        layout_order_.clear();
        frame_offsets_.clear();
        total_frame_size_ = 0;

        // 2. 生成排序策略（示例：按对齐降序排列）
        std::vector<std::pair<int, FrameObjectMetadata *>> sorted_objects;
        for (const auto &[idx, obj] : frame_objects_)
        {
            sorted_objects.emplace_back(idx, obj.get());
        }
        std::sort(sorted_objects.begin(), sorted_objects.end(),
                  [](const auto &a, const auto &b)
                  {
                      return a.second->alignment > b.second->alignment;
                  });

        // 3. 计算偏移量和总大小（假设栈向下增长）
        size_t current_offset = 0;
        for (const auto &[idx, obj] : sorted_objects)
        {
            // 对齐调整
            size_t aligned_offset = (current_offset + obj->alignment - 1) & ~(obj->alignment - 1);
            // 分配空间
            frame_offsets_[idx] = aligned_offset;
            current_offset = aligned_offset + obj->size;
            layout_order_.push_back(idx);
        }
        total_frame_size_ = current_offset;

        // 4. 更新缓存状态
        is_frame_layout_dirty_ = false;
    }
    return layout_order_;
}

// Enhanced frame index management
bool MachineFrame::has_frame_index(Value *value) const
{
    for (const auto &[idx, obj] : frame_objects_)
    {
        if (obj->associated_value == value)
        {
            return true;
        }
    }
    return false;
}

// Frame index management
int MachineFrame::create_frame_object(Value *value)
{
    FrameObjectMetadata info;
    info.size = 4;                                 // Default size, adjust as needed
    info.alignment = 4;                            // Default alignment, adjust as needed
    info.flags = FrameObjectMetadata::IsFixedSize; // Default flags
    info.associated_value = value;

    return create_frame_object(info);
}
int MachineFrame::get_frame_index(Value *value) const
{
    for (const auto &[idx, obj] : frame_objects_)
    {
        if (obj->associated_value == value)
        {
            return idx;
        }
    }
    assert(false && "Frame object not found!");
    return -1;
}

std::vector<std::pair<int, FrameObjectMetadata>> MachineFrame::objects() const
{
    std::vector<std::pair<int, FrameObjectMetadata>> objects;
    objects.reserve(frame_objects_.size());

    for (const auto &[idx, obj] : frame_objects_)
    {
        objects.emplace_back(idx, *obj);
    }

    std::sort(objects.begin(), objects.end(),
              [](const auto &a, const auto &b)
              {
                  return a.first < b.first; // asc
              });

    return objects;
}

size_t MachineFrame::get_frame_index_offset(int index) const
{
    frame_layout();
    auto it = frame_offsets_.find(index);
    if (it != frame_offsets_.end())
    {
        return it->second;
    }
    throw std::out_of_range("Invalid frame index");
}

size_t MachineFrame::get_total_frame_size() const
{
    frame_layout();
    return total_frame_size_;
}

bool MachineFrame::is_valid_index(int index) const
{
    return frame_objects_.find(index) != frame_objects_.end();
}
// factory methods
int MachineFrame::create_fixed_size(Value *value, int64_t size, unsigned alignment)
{
    return create_impl(value, size, alignment, FrameObjectMetadata::Flags::IsFixedSize);
}

// 创建可变大小对象（如 alloca）
int MachineFrame::create_variable_size(Value *value, unsigned alignment)
{
    return create_impl(value, -1, alignment, FrameObjectMetadata::Flags::IsVariableSize);
}

// 创建 SIMD 变量（固定大小 + 高对齐）
int MachineFrame::create_simd_variable(Value *value, int64_t size, unsigned simd_alignment)
{
    auto flags = FrameObjectMetadata::Flags::IsFixedSize | FrameObjectMetadata::Flags::NeedsRealign;
    return create_impl(value, size, simd_alignment, flags);
}

// 创建溢出槽（固定大小 + 溢出标记）
int MachineFrame::create_spill_slot(unsigned regClassID, int64_t size, unsigned alignment)
{
    auto flags = FrameObjectMetadata::IsFixedSize | FrameObjectMetadata::IsSpillSlot;
    int idx = create_impl(nullptr, size, alignment, flags);

    // 关键：检查元数据是否存在
    if (FrameObjectMetadata *meta = get_metadata(idx))
    {
        meta->spill_rc_id = regClassID;
        return idx;
    }
    else
    {
        throw std::runtime_error("Failed to create spill slot: invalid frame index");
    }
}
