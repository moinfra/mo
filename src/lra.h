#pragma once
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <map>
#include <vector>
#include <functional>

using AliasCheckFn = std::function<bool(unsigned reg1, unsigned reg2)>;
class MachineFunction;

//===----------------------------------------------------------------------===//
// Live Range Representation
//===----------------------------------------------------------------------===//

// Represents a continuous live interval, immutable and guaranteed to be valid.
struct LiveInterval
{
private:
    unsigned start_; // Inclusive
    unsigned end_;   // Exclusive

public:
    LiveInterval(unsigned start, unsigned end);

    // Accessors
    unsigned start() const;
    unsigned end() const;

    // Interval overlap check (including adjacent intervals)
    bool overlaps(const LiveInterval &other) const;

    // Interval merge (must ensure overlap or adjacency)
    LiveInterval merge(const LiveInterval &other) const;

    // Comparison for sorting
    bool operator<(const LiveInterval &rhs) const;
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

    // Internal function to merge overlapping intervals
    void merge_intervals();

public:
    LiveRange(unsigned reg) : reg_(reg) {}

    // Add an interval and automatically merge overlaps
    void add_interval(unsigned start, unsigned end);

    // Check if it is live at the specified position
    bool live_at(unsigned pos) const;

    // Fast conflict detection (linear two-pointer scan)
    bool interferes_with(const LiveRange &other) const;

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

class LiveRangeAnalysis
{
    const MachineFunction &mf_;
    mutable bool live_ranges_dirty_ = true;
    mutable std::unordered_map<unsigned, LiveRange> vreg_live_ranges_;
    AliasCheckFn is_alias_;

public:
    explicit LiveRangeAnalysis(
        const MachineFunction &mf,
        AliasCheckFn is_alias = [](unsigned, unsigned)
        { return false; }) : mf_(mf), is_alias_(std::move(is_alias)) {}

    bool has_conflict(unsigned reg1, unsigned reg2) const;

    void compute() const;

    const LiveRange &get_live_range(unsigned vreg) const;
    std::map<unsigned, std::set<unsigned>> build_interference_graph() const;

    void mark_dirty() { live_ranges_dirty_ = true; }
};
