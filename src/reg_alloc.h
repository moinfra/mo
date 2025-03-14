// reg_alloc.h - Register Allocation Interface
#pragma once

#include <map>
#include <set>
#include <vector>
#include <memory>
#include <string>
#include <optional>

// Forward declarations
class MachineFunction;
class LiveRange;
class LiveRangeAnalyzer;
class TargetRegisterInfo;
class TargetInstInfo;
class MachineInst;
class MachineBasicBlock;
class PressureTracker;
class MachineModule;
using MI_iterator = std::vector<std::unique_ptr<MachineInst>>::iterator;

// Register allocation result statistics
struct RegAllocResult
{
    unsigned num_spills = 0;     // Number of spilled registers
    unsigned num_copies = 0;     // Number of register copies inserted
    unsigned max_stack_size = 0; // Maximum stack size used
    bool successful = false;     // Whether allocation succeeded

    std::string error_message; // Error message if failed

    std::string to_string() const;
};

// Base register allocator interface
class RegisterAllocator
{
protected:
    MachineFunction &mf_;
    LiveRangeAnalyzer &lra_;
    const TargetRegisterInfo &tri_;
    const TargetInstInfo &tii_;

    // Track physical register assignments
    std::map<unsigned, unsigned> vreg_to_preg_map_;
    std::map<unsigned, unsigned> vreg_to_tmp_preg_map_; // used for spilling

    // Track spill slots
    std::map<unsigned, int> vreg_to_spill_slot_;

    // Allocation statistics
    unsigned num_spills_ = 0;
    unsigned num_copies_ = 0;

    // Utility methods for derived allocators
    virtual void initialize_allocation();
    virtual bool assign_physical_reg(unsigned vreg, unsigned preg);
    virtual bool assign_temp_physical_reg(unsigned vreg, unsigned preg);
    virtual int allocate_spill_slot(unsigned vreg);

public:
    RegisterAllocator(MachineFunction &mf);
    virtual ~RegisterAllocator() = default;

    // Main allocation method to be implemented by derived classes
    virtual RegAllocResult allocate_registers() = 0;
    virtual void apply();
    //  TODO: 1. Check if each vregs is assigned to a preg or spill slot
    //  TODO: 2. Check if no vreg is assigned to two different pregs or spill slots
    //  TODO: 3. Check if any two conflicting vregs are assigned to different pregs or spill slots
    virtual void validate_allocation() {}
    // Common interface methods
    std::optional<unsigned> get_assigned_reg(unsigned vreg) const;
    bool is_spilled(unsigned vreg) const;
    int get_spill_slot(unsigned vreg) const;

    std::map<unsigned, unsigned> get_vreg_to_preg_map() const { return vreg_to_preg_map_; }
    std::map<unsigned, unsigned> get_vreg_to_tmp_preg_map() const { return vreg_to_tmp_preg_map_; }
    std::map<unsigned, int> get_vreg_to_spill_slot() const { return vreg_to_spill_slot_; }
    unsigned get_num_spills() const { return num_spills_; }
    unsigned get_num_copies() const { return num_copies_; }

    // Final cleanup and remapping of instructions
    // virtual void finalize_allocation();

    // Calculate spill cost for a virtual register
    virtual float calculate_spill_cost(unsigned vreg, const LiveRange &live_range);
};
