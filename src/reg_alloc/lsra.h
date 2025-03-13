// lsra.h - Linear Scan Register Allocation
#pragma once

#include "../reg_alloc.h"
#include "../lra.h"
#include <queue>
#include <vector>
#include <unordered_map>

// Linear Scan allocator implementation
class LinearScanRegisterAllocator : public RegisterAllocator
{
private:
    // 比较函数对象
    struct CompareByIntervalStartAsc
    {
        bool operator()(const LiveRange *a,
                        const LiveRange *b) const
        {
            return a->atomized_interval().start() > b->atomized_interval().start(); // 最小堆
        }
    };

    struct CompareByIntervalEndAsc
    {
        bool operator()(const LiveRange *a,
                        const LiveRange *b) const
        {
            return a->atomized_interval().end() > b->atomized_interval().end(); // 最小堆
        }
    };

private:
    // 活动区间队列（按起始位置排序）
    std::priority_queue<LiveRange *,
                        std::vector<LiveRange *>,
                        CompareByIntervalStartAsc>
        unhandled_;

    std::priority_queue<LiveRange *,
                        std::vector<LiveRange *>,
                        CompareByIntervalEndAsc>
        active_;

    std::vector<std::unique_ptr<LiveRange>> ranges_;

    // 物理寄存器状态跟踪（并非全局状态，是一个动态变化的表）
    struct PhysRegState
    {
        bool available = true;
        LiveRange *assigned_range = nullptr;
        unsigned last_use_pos = 0;
    };

    std::unordered_map<unsigned, PhysRegState> phys_reg_states_;

    // 溢出决策相关
    std::unordered_map<unsigned, float> spill_costs_;

public:
    explicit LinearScanRegisterAllocator(MachineFunction &mf) : RegisterAllocator(mf) {}

    RegAllocResult allocate_registers() override;

private:
    // 核心分配步骤
    void initialize();
    void expire_old_intervals(unsigned current_pos);
    bool allocate_register_for(LiveRange *lr);
    LiveRange *choose_register_to_spill(unsigned current_pos);

    // 辅助方法
    void update_phys_reg_usage(unsigned preg, unsigned use_pos);
    std::vector<unsigned> get_allocatable_regs(unsigned vreg) const;
    std::vector<unsigned> collect_allocatable_regs() const;
    bool has_conflict(LiveRange *lr, unsigned preg) const;
    void spill_live_range(LiveRange *lr);
};
