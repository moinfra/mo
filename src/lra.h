// lra.h - Liveness Analysis
#pragma once
#include <unordered_map>
#include <iostream>
#include <optional>
#include <unordered_set>
#include <set>
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

public:
    LiveInterval(unsigned start, unsigned end, LiveRange *parent = nullptr);

    // Accessors
    unsigned start() const;
    unsigned end() const;
    LiveRange *parent() const { return parent_; }

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
};

// A collection of live intervals that are related to a register.
class LiveRange
{
private:
    std::vector<LiveInterval> intervals_;

    unsigned reg_;             // Assigned register (physical or virtual)
    bool is_physical_ = false; // Whether it is bound to a physical register
    bool is_spilled_ = false;  // Whether it has been spilled to the stack
    int spill_slot_ = -1;      // Stack slot index
    mutable std::unordered_set<MachineInst *> def_insts_;
    mutable std::unordered_set<MachineInst *> use_insts_;

private:
    // Internal function to merge overlapping intervals
    void merge_intervals();

public:
    LiveRange(unsigned reg) : reg_(reg) {}
    LiveRange(unsigned reg, std::vector<LiveInterval> intervals)
        : intervals_(std::move(intervals)), reg_(reg) {}

    // Add an interval and automatically merge overlaps
    void add_interval(unsigned start, unsigned end);
    void add_def_inst(MachineInst *inst) const { def_insts_.insert(inst); }
    void add_use_inst(MachineInst *inst) const { use_insts_.insert(inst); }
    std::unordered_set<MachineInst *> &def_insts() const { return def_insts_; }
    std::unordered_set<MachineInst *> &use_insts() const { return use_insts_; }

    // Check if it is live at the specified position
    bool live_at(unsigned pos) const;

    // Fast conflict detection (linear two-pointer scan)
    bool interferes_with(const LiveRange &other) const;

    std::string to_string() const;
    // --- Register allocation related ---
    void assign_physical_reg(unsigned phys_reg);
    void spill(int slot);

    // --- Accessors ---
    unsigned reg() const { return reg_; }
    bool is_physical() const { return is_physical_; }
    bool is_spilled() const { return is_spilled_; }
    int spill_slot() const { return spill_slot_; }
    const auto &intervals() const { return intervals_; }
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

    mutable std::unordered_map<MachineBasicBlock *, BlockLiveness> block_info;

private:
    LiveRange *live_range_of(unsigned reg) const;
    void compute_data_flow() const;
    unsigned find_first_def_pos(MachineBasicBlock *bb, unsigned reg) const;
    unsigned find_last_def_pos(MachineBasicBlock *bb, unsigned reg) const;
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

public:
    void set_compute_metric_counter(size_t &counter) const { compute_metric_counter_ = &counter; }
    void dump_cfg_graphviz(std::ostream &out) const;
};
