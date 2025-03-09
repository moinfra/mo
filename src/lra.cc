#include "lra.h"
#include "machine.h"
#include <algorithm>
#include <stdexcept>

#include <algorithm>
#include <cassert>
#include <iostream>
#include <vector>

//===----------------------------------------------------------------------===//
// Live Interval Implementation
//===----------------------------------------------------------------------===//

LiveInterval::LiveInterval(unsigned start, unsigned end)
    : start_(start), end_(end)
{
    assert(start < end && "Invalid interval range");
}

unsigned LiveInterval::start() const { return start_; }
unsigned LiveInterval::end() const { return end_; }

bool LiveInterval::overlaps(const LiveInterval &other) const
{
    return (start() < other.end() && other.start() < end()) ||
           (end() == other.start() || start() == other.end());
}

LiveInterval LiveInterval::merge(const LiveInterval &other) const
{
    assert((overlaps(other) || end() == other.start() || other.end() == start()) &&
           "Merging non-overlapping intervals");
    return LiveInterval(std::min(start(), other.start()),
                        std::max(end(), other.end()));
}

bool LiveInterval::operator<(const LiveInterval &rhs) const
{
    return start() < rhs.start() ||
           (start() == rhs.start() && end() < rhs.end());
}

void LiveRange::add_interval(unsigned start, unsigned end)
{
    intervals_.emplace_back(start, end);
    merge_intervals(); // Ensure intervals are ordered and non-overlapping
}

bool LiveRange::live_at(unsigned pos) const
{
    // Binary search optimization: find the first interval whose end position > pos
    auto it = std::upper_bound(
        intervals_.begin(), intervals_.end(), pos,
        [](unsigned val, const LiveInterval &interval)
        {
            return val < interval.end();
        });
    return it != intervals_.begin() && (--it)->start() <= pos;
}

bool LiveRange::interferes_with(const LiveRange &other) const
{
    const auto &a = intervals_;
    const auto &b = other.intervals_;
    size_t i = 0, j = 0;

    while (i < a.size() && j < b.size())
    {
        if (a[i].overlaps(b[j]))
            return true;
        a[i].start() < b[j].start() ? ++i : ++j;
    }
    return false;
}

void LiveRange::assign_physical_reg(unsigned phys_reg)
{
    is_physical_ = true;
    reg_ = phys_reg;
    is_spilled_ = false;
}

void LiveRange::spill(int slot)
{
    is_spilled_ = true;
    spill_slot_ = slot;
    is_physical_ = false;
}

void LiveRange::merge_intervals()
{
    if (intervals_.empty())
        return;

    std::sort(intervals_.begin(), intervals_.end());
    std::vector<LiveInterval> merged;
    merged.push_back(intervals_[0]);

    for (size_t i = 1; i < intervals_.size(); ++i)
    {
        LiveInterval &last = merged.back();
        if (intervals_[i].overlaps(last))
        {
            last = last.merge(intervals_[i]); // Merge to create a new interval
        }
        else
        {
            merged.push_back(intervals_[i]);
        }
    }
    intervals_.swap(merged);
}

//===----------------------------------------------------------------------===//
// Live Range Analysis Implementation
//===----------------------------------------------------------------------===//
void LiveRangeAnalysis::compute() const
{
    if (!live_ranges_dirty_)
        return;

    // Ensure global instruction positions are computed
    mf_.ensure_global_positions_computed();

    // Step 1: Collect all virtual register use positions (definition + use points)
    std::unordered_map<unsigned, std::set<unsigned>> vreg_positions;

    for (const auto &bb : mf_.get_basic_blocks())
    {
        for (const auto &inst : bb->instructions())
        {
            const unsigned global_pos = mf_.get_global_instr_pos(inst.get());

            for (const auto &op : inst->operands())
            {
                if (!op.is_reg())
                    continue;
                const unsigned reg = op.reg();

                // Only process virtual registers
                if (reg < MachineFunction::FIRST_VIRTUAL_REGISTER)
                    continue;

                // Definition points need special handling: start position = definition
                // point, end position = next instruction
                if (op.is_def())
                {
                    vreg_positions[reg].insert(global_pos);
                }
                // Use points: end position = after the current instruction
                vreg_positions[reg].insert(global_pos + 1);
            }
        }
    }

    // Step 2: Generate continuous intervals for each virtual register
    vreg_live_ranges_.clear();
    for (const auto &[vreg, positions] : vreg_positions)
    {
        if (positions.empty())
            continue;

        // Convert ordered positions to continuous intervals
        std::vector<unsigned> sorted(positions.begin(), positions.end());
        std::sort(sorted.begin(), sorted.end());

        LiveRange lr(vreg);
        unsigned start = sorted.front();
        for (size_t i = 1; i < sorted.size(); ++i)
        {
            if (sorted[i] != sorted[i - 1] + 1)
            {
                lr.add_interval(start, sorted[i - 1] + 1);
                start = sorted[i];
            }
        }
        // Add the last interval
        lr.add_interval(start, sorted.back() + 1);

        vreg_live_ranges_.emplace(vreg, std::move(lr));
    }

    live_ranges_dirty_ = false;
}

const LiveRange &LiveRangeAnalysis::get_live_range(unsigned vreg) const
{
    if (live_ranges_dirty_)
        compute();

    auto it = vreg_live_ranges_.find(vreg);
    if (it == vreg_live_ranges_.end())
    {
        throw std::runtime_error("Virtual register " + std::to_string(vreg) +
                                 " has no live range");
    }
    return it->second;
}

std::map<unsigned, std::set<unsigned>>
LiveRangeAnalysis::build_interference_graph() const
{
    compute(); // Ensure data is up-to-date

    std::map<unsigned, std::set<unsigned>> graph;
    const auto &ranges = vreg_live_ranges_;

    // Double-iterate over all virtual register pairs
    for (auto it1 = ranges.begin(); it1 != ranges.end(); ++it1)
    {
        const unsigned vreg1 = it1->first;
        const LiveRange &lr1 = it1->second;

        for (auto it2 = std::next(it1); it2 != ranges.end(); ++it2)
        {
            const unsigned vreg2 = it2->first;
            const LiveRange &lr2 = it2->second;

            if (lr1.interferes_with(lr2))
            {
                graph[vreg1].insert(vreg2);
                graph[vreg2].insert(vreg1);
            }
        }
    }

    return graph;
}

bool LiveRangeAnalysis::has_conflict(unsigned reg1, unsigned reg2) const
{
    // Step 1: Check alias relationships

    // Physical register alias conflict (no need for overlapping live intervals)
    if (reg1 < MachineFunction::FIRST_VIRTUAL_REGISTER &&
        reg2 < MachineFunction::FIRST_VIRTUAL_REGISTER)
    {
        return is_alias_(reg1, reg2);
    }

    // Step 2: Check for overlapping live intervals
    const LiveRange &lr1 = get_live_range(reg1);
    const LiveRange &lr2 = get_live_range(reg2);

    for (const auto &i1 : lr1.intervals())
    {
        for (const auto &i2 : lr2.intervals())
        {
            if (i1.overlaps(i2))
            {
                return true;
            }
        }
    }

    // Step 3: Handle alias conflicts between virtual and physical registers
    if (reg1 < MachineFunction::FIRST_VIRTUAL_REGISTER)
    {
        return is_alias_(reg1, reg2);
    }
    if (reg2 < MachineFunction::FIRST_VIRTUAL_REGISTER)
    {
        return is_alias_(reg2, reg1);
    }

    return false;
}
