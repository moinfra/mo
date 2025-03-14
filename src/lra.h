// lra.h - Liveness Analysis
#pragma once
#include <unordered_map>
#include <iostream>
#include <optional>
#include <unordered_set>
#include <set>
#include <cassert>
#include <limits>
#include <map>
#include <vector>
#include <string>
#include <sstream>
#include <functional>
#include <memory>

using AliasCheckFn = std::function<bool(unsigned reg1, unsigned reg2)>;
class MachineFunction;
class MachineBasicBlock;
class MachineInst;

class LiveRange;

//===----------------------------------------------------------------------===//
// Live Range Representation
//===----------------------------------------------------------------------===//

// Represents a continuous live interval, immutable and guaranteed to be valid.
struct LiveInterval
{
private:
    unsigned start_; // Inclusive
    unsigned end_;   // Exclusive
    LiveRange *parent_;
    mutable std::unordered_set<MachineInst *> def_insts_;
    mutable std::unordered_set<MachineInst *> use_insts_;

public:
    LiveInterval(unsigned start, unsigned end, LiveRange *parent = nullptr);

    // Accessors
    unsigned start() const;
    unsigned end() const;
    LiveRange *parent() { return parent_; }

    void add_def_inst(MachineInst *inst) const { def_insts_.insert(inst); }
    void add_use_inst(MachineInst *inst) const { use_insts_.insert(inst); }
    const auto &def_insts() const { return def_insts_; }
    const auto &use_insts() const { return use_insts_; }

    // Interval overlap check (including adjacent intervals)
    bool overlaps(const LiveInterval &other) const;

    // Interval merge (must ensure overlap or adjacency)
    LiveInterval merge(const LiveInterval &other) const;

    // Comparison for sorting
    bool operator<(const LiveInterval &rhs) const;

    std::string to_string() const
    {
        std::ostringstream oss;
        oss << "LiveInterval [" << start_ << ", " << end_ << ")";
        return oss.str();
    }

    friend class LiveRange;
};

// A collection of live intervals that are related to a register.
class LiveRange
{
private:
    std::vector<LiveInterval> intervals_;

    unsigned preg_ = std::numeric_limits<unsigned>::max(); // Currently assigned register (physical or virtual)
    unsigned vreg_ = std::numeric_limits<unsigned>::max(); // Original virtual register
    bool is_allocated_ = false;                            // Whether it is bound to a physical register
    bool is_spilled_ = false;                              // Whether it has been spilled to the stack
    int spill_slot_ = -1;                                  // Stack slot index
    mutable std::unordered_set<MachineInst *> def_insts_;
    mutable std::unordered_set<MachineInst *> use_insts_;

private:
    // Internal function to merge overlapping intervals
    void merge_intervals();

public:
    LiveRange(unsigned vreg) : vreg_(vreg) {}
    LiveRange(unsigned vreg, std::vector<LiveInterval> intervals)
        : intervals_(std::move(intervals)), vreg_(vreg) {}

    // Add an interval and automatically merge overlaps
    void add_interval(unsigned start, unsigned end);

    void add_def_inst(size_t pos, MachineInst *inst) const
    {
        def_insts_.insert(inst);
        for (auto &interval : intervals_)
        {
            if (interval.start() <= pos && pos < interval.end())
            {
                interval.add_def_inst(inst);
            }
        }
    }

    void add_use_inst(size_t pos, MachineInst *inst) const
    {
        use_insts_.insert(inst);
        for (auto &interval : intervals_)
        {
            if (interval.start() <= pos && pos < interval.end())
            {
                interval.add_use_inst(inst);
            }
        }
    }
    std::unordered_set<MachineInst *> &def_insts() const { return def_insts_; }
    std::unordered_set<MachineInst *> &use_insts() const { return use_insts_; }

    // Check if it is live at the specified position
    bool live_at(unsigned pos) const;

    // Fast conflict detection (linear two-pointer scan)
    bool interferes_with(const LiveRange &other) const;

    std::string to_string() const;
    // --- Register allocation related ---
    void assign(unsigned preg);
    void mark_spilled(int slot);

    // --- Accessors ---
    unsigned vreg() const { return vreg_; }
    unsigned preg() const { return preg_; }
    bool is_allocated() const { return is_allocated_; }
    bool is_spilled() const { return is_spilled_; }
    int spill_slot() const { return spill_slot_; }
    const auto &intervals() const { return intervals_; }

    bool is_atomized() const { return intervals_.size() == 1; }

    LiveInterval &atomized_interval()
    {
        assert(is_atomized() && "Live range is not atomized.");
        return intervals_[0];
    }
    const LiveInterval &atomized_interval() const
    {
        assert(is_atomized() && "Live range is not atomized.");
        return intervals_[0];
    }

    std::vector<std::unique_ptr<LiveRange>> atomized_ranges(MachineFunction &mf);
};

const AliasCheckFn none_alias = [](unsigned reg1, unsigned reg2)
{
    return false;
};

class LiveRangeAnalyzer
{
private:
    const MachineFunction &mf_;
    mutable bool live_ranges_dirty_ = true;
    mutable std::unordered_map<unsigned, std::unique_ptr<LiveRange>> reg_live_ranges_;
    mutable size_t *compute_metric_counter_ = nullptr;
    AliasCheckFn is_alias_;
    struct BlockLiveness
    {
        std::unordered_set<unsigned> in;
        std::unordered_set<unsigned> out;
        std::unordered_set<unsigned> use;
        std::unordered_set<unsigned> def;
    };

    mutable std::unordered_map<MachineBasicBlock *, BlockLiveness> block_info_;

private:
    LiveRange *live_range_of(unsigned reg) const;
    void compute_data_flow() const;
    size_t find_first_def_pos(MachineBasicBlock *bb, unsigned reg, size_t after_pos_exclusive) const;
    size_t find_last_def_pos(MachineBasicBlock *bb, unsigned reg, size_t before_pos_exclusive) const;
    void compute_local_use_def(MachineBasicBlock *bb, std::unordered_set<unsigned> &use, std::unordered_set<unsigned> &def) const;

public:
    explicit LiveRangeAnalyzer(const MachineFunction &mf, AliasCheckFn is_alias = none_alias)
        : mf_(mf), is_alias_(std::move(is_alias)) {}

    bool has_conflict(unsigned reg1, unsigned reg2) const;

    void compute() const;

    LiveRange *get_live_range(unsigned reg) const;
    LiveRange *get_physical_live_range(unsigned phys_reg) const;

    std::map<unsigned, std::set<unsigned>> build_interference_graph() const;

    void mark_dirty() { live_ranges_dirty_ = true; }

    std::unordered_map<unsigned, std::unique_ptr<LiveRange>> &get_all_live_ranges() const { return reg_live_ranges_; }
    BlockLiveness &block_info(MachineBasicBlock *bb) const { return block_info_[bb]; }
    void dump(std::ostream &os);
    void export_to_gantt_chart(std::ostream &os) const;
    void export_to_json(std::ostream &os) const;

public:
    void set_compute_metric_counter(size_t &counter) const { compute_metric_counter_ = &counter; }
};
