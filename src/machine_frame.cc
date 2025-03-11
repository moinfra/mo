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

size_t MachineFrame::get_frame_size() const
{
    // 确保布局已更新
    frame_layout();
    return total_frame_size_;
}

size_t MachineFrame::get_frame_index_offset(int index) const
{
    // 确保布局已更新
    frame_layout();
    auto it = frame_offsets_.find(index);
    if (it != frame_offsets_.end())
    {
        return it->second;
    }
    // 错误处理：无效的 index
    throw std::out_of_range("Invalid frame index");
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

size_t MachineFrame::get_frame_size() const
{
    frame_layout();
    return total_frame_size_;
}
