#include "lra.h"
#include "machine.h"
#include <algorithm>
#include <stdexcept>
#include <limits>
#include <iomanip>
#include <algorithm>
#include <cassert>
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <map>
#include <sstream>
#include <memory>

//===----------------------------------------------------------------------===//
// Live Interval Implementation
//===----------------------------------------------------------------------===//

LiveInterval::LiveInterval(unsigned start, unsigned end, LiveRange *parent)
    : start_(start), end_(end), parent_(parent)
{
    // assert(start < end && "Invalid interval range");
    MO_ASSERT(start < end, "Invalid interval range [%u, %u)", start, end);
    if (!parent)
    {
        MO_WARN("Live interval has no parent");
    }
}

unsigned LiveInterval::start() const { return start_; }
unsigned LiveInterval::end() const { return end_; }

bool LiveInterval::overlaps(const LiveInterval &other) const
{
    return start() < other.end() && other.start() < end();
}

LiveInterval LiveInterval::merge(const LiveInterval &other) const
{
    MO_ASSERT((overlaps(other) || end() == other.start() || other.end() == start()),
              "Merging non-overlapping intervals");
    MO_ASSERT(parent_ == other.parent_, "Merging intervals with different parents");

    return LiveInterval(std::min(start(), other.start()),
                        std::max(end(), other.end()), parent_);
}

bool LiveInterval::operator<(const LiveInterval &rhs) const
{
    return start() < rhs.start() ||
           (start() == rhs.start() && end() < rhs.end());
}

void LiveRange::add_interval(unsigned start, unsigned end)
{
    MO_DEBUG("Extend reg %u live range with [%u, %u)", preg_, start, end);
    intervals_.emplace_back(start, end, this);
    merge_intervals(); // Ensure intervals are ordered and non-overlapping
}

bool LiveRange::live_at(unsigned pos) const
{
    // Binary search optimization: find the first interval with start > pos
    auto it = std::upper_bound(
        intervals_.begin(), intervals_.end(), pos,
        [](unsigned val, const LiveInterval &interval)
        {
            return val < interval.start();
        });

    if (it == intervals_.begin())
        return false;

    --it;
    return pos < it->end();
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

std::string LiveRange::to_string() const
{
    std::ostringstream oss;
    oss << "LiveRange {";
    oss << " preg: " << preg_;
    oss << " vreg: " << vreg_;
    oss << " alloc: " << is_allocated_;
    oss << " spill: " << is_spilled_;
    oss << " slot: " << spill_slot_;
    oss << " subs: { ";
    for (const auto &interval : intervals_)
    {
        oss << "[" << interval.start() << ", " << interval.end() << ") ";
    }
    oss << " } ";
    oss << "}";
    return oss.str();
}

void LiveRange::assign(unsigned preg)
{
    MO_ASSERT(!is_spilled(), "Live range is already spilled. Cannot assign a register.");
    is_allocated_ = true;
    preg_ = preg;
    is_spilled_ = false;
}

void LiveRange::mark_spilled(int slot)
{
    MO_ASSERT(is_allocated(), "Live range is not allocated. Cannot spill.");
    is_spilled_ = true;
    spill_slot_ = slot;
    preg_ = std::numeric_limits<unsigned>::max();
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
        if (intervals_[i].start() <= last.end()) // Allow adjacent intervals to be merged
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
std::vector<std::unique_ptr<LiveRange>> LiveRange::atomized_ranges(MachineFunction &mf)
{

    std::vector<std::unique_ptr<LiveRange>> ret;
    ret.reserve(intervals_.size());

    bool is_first_interval = true;
    for (LiveInterval interval_ : intervals_)
    {
        LiveInterval copied(interval_);

        // 区间分裂之后，要创建新的变量，这样能确保以 Interval 为单位的分配器能够正确分配

        auto new_vreg = is_first_interval ? vreg_ : mf.clone_vreg(vreg_);
        std::unique_ptr<LiveRange> new_range(new LiveRange(new_vreg));
        copied.parent_ = new_range.get();
        new_range->intervals_.push_back(copied);
        new_range->preg_ = preg_;
        new_range->is_allocated_ = is_allocated_;
        new_range->is_spilled_ = is_spilled_;
        new_range->spill_slot_ = spill_slot_;

        new_range->def_insts_ = copied.def_insts();
        new_range->use_insts_ = copied.use_insts();

        if (!is_first_interval) // 替换新区间对变量的使用
        {
            MO_DEBUG("New vreg cloned: %u -> %u", vreg_, new_vreg);
            for (auto &bb : mf.basic_blocks())
            {
                for (auto &it : bb->instructions())
                {
                    it->replace_reg(vreg_, new_vreg);
                }
            }
        }

        assert(new_range->is_atomized() && "New range is not atomized.");
        ret.emplace_back(std::move(new_range));

        is_first_interval = false;
    }

    return ret;
}

//===----------------------------------------------------------------------===//
// Live Range Analysis Implementation
//===----------------------------------------------------------------------===//
void LiveRangeAnalyzer::compute() const
{
    if (!live_ranges_dirty_)
        return;

    MO_DEBUG("Computing live ranges for function");
    mf_.ensure_global_positions_computed();
    compute_data_flow();

    reg_live_ranges_.clear();

    for (auto &bb : mf_.basic_blocks())
    {
        MO_DEBUG("处理基本块入口处的活跃性");
        // 如果在入口活跃，那么从这个点到最后一次（本定义）使用都活跃
        // for (auto reg : block_info_[bb.get()].in)
        // {
        //     LiveRange *lr = live_range_of(reg);
        //     lr->add_interval(bb->global_start(), bb->global_start() + 1);
        // }

        MO_DEBUG("处理指令级活跃区间");
        for (auto &inst : bb->instructions())
        {
            size_t pos = mf_.get_global_instr_pos(inst.get());
            MO_DEBUG("current pos: %zu", pos);
            MO_DEBUG("处理定义");
            // 如果当前程序点定义了一个变量，那么从这次定义直到最后一次（本定义）使用都活跃。
            for (auto &reg : inst->defs())
            {
                LiveRange *lr = live_range_of(reg);
                lr->add_interval(pos, pos + 1);
                lr->add_def_inst(pos, inst.get());
            }

            MO_DEBUG("处理使用");
            // 如果当前程序点用到了一个变量，那么从变量上次定义到这次使用都是活跃的。
            // 怎么寻找上次的定义呢？尝试在本指令块中找，如果没找到，说明变量在之前的基本块中定义过，
            // 那么暂且从开头到当前点都添加给这个变量的活跃区间。
            // 至于之前基本块的区间，交给那个块的 live out 集合处理，这里不管。
            auto uses = inst->uses();
            MO_DEBUG("%s uses: %s", inst->to_string().c_str(), mo_join(uses, ", ").c_str());
            for (auto &reg : uses)
            {
                LiveRange *lr = live_range_of(reg);
                size_t in_block_last_def = find_last_def_pos(bb.get(), reg, pos);
                lr->add_interval(in_block_last_def, std::max(pos, in_block_last_def) + 1);
                lr->add_use_inst(pos, inst.get());
            }
        }
    }

    MO_DEBUG("处理跨块活跃区间");
    for (auto &bb : mf_.basic_blocks())
    {
        auto &info = block_info_[bb.get()];
        // for (auto reg : info.in)
        // {
        //     // 如果一个变量在入口活跃，则从块头到首次定义都活跃
        //     LiveRange *lr = live_range_of(reg);
        //     auto first_def_pos = find_first_def_pos(bb.get(), reg);
        //     lr->add_interval(bb->global_start(), first_def_pos + 1);
        // }

        for (auto reg : info.out)
        {
            // 如果一个变量在出口活跃，则从上次定义点到块尾都活跃
            LiveRange *lr = live_range_of(reg);
            size_t first_def_pos = find_last_def_pos(bb.get(), reg, -1);
            // auto reg_def_pos = first_def_pos != bb->global_end_inclusive() ? first_def_pos : bb->global_start();
            lr->add_interval(first_def_pos, bb->global_end_inclusive() + 1);
            // for (auto *succ : bb->successors())
            // {
            //     auto &succ_info = block_info_[succ];
            //     if (succ_info.in.count(reg))
            //     {
            //         unsigned bb_end = bb->global_end_inclusive();
            //         unsigned succ_start = succ->global_start();

            //         if (succ_start > bb_end)
            //         {
            //             // 后继基本块的入口指令编号大于当前基本块的出口指令编号
            //             lr->add_interval(bb_end + 1, succ_start + 1);
            //         }
            //         else
            //         {
            //             // 后继基本块的入口指令编号小于当前基本块的出口指令编号
            //             lr->add_interval(bb_end, bb_end + 1);         // 扩展到当前基本块的出口
            //             lr->add_interval(succ_start, succ_start + 1); // 在后继基本块中重新开始活跃区间
            //         }
            //     }
            // }
        }
    }

    live_ranges_dirty_ = false;
    if (compute_metric_counter_)
    {
        *compute_metric_counter_ += 1;
    }
}

LiveRange *LiveRangeAnalyzer::live_range_of(unsigned reg) const
{
    if (!reg_live_ranges_.count(reg))
    {
        reg_live_ranges_[reg] = std::make_unique<LiveRange>(reg);
        if (MachineFunction::is_physical_reg(reg))
        {
            reg_live_ranges_[reg]->assign(reg);
        }
    }
    return reg_live_ranges_[reg].get();
};

std::vector<MachineBasicBlock *> compute_reverse_post_order(MachineBasicBlock *entry)
{
    std::vector<MachineBasicBlock *> order;
    std::unordered_set<MachineBasicBlock *> visited;
    std::function<void(MachineBasicBlock *)> dfs = [&](MachineBasicBlock *bb)
    {
        if (visited.count(bb))
            return;
        visited.insert(bb);
        for (auto *succ : bb->successors())
        {
            dfs(succ);
        }
        order.push_back(bb);
    };
    dfs(entry);
    return order;
}

// 关键修正1: 正确的数据流方程
void LiveRangeAnalyzer::compute_data_flow() const
{
    block_info_.clear();

    // 初始化 USE/DEF 集合
    for (auto &bb : mf_.basic_blocks())
    {
        auto &info = block_info_[bb.get()];
        compute_local_use_def(bb.get(), info.use, info.def);
    }

    // 获取逆后序序列（反向遍历）
    std::vector<MachineBasicBlock *> rpo_order = compute_reverse_post_order(mf_.basic_blocks().front().get());

    // 迭代直到收敛
    bool changed;
    unsigned iter = 0;
    do
    {
        changed = false;
        for (auto *bb : rpo_order)
        { // 按逆后序处理
            auto &info = block_info_[bb];

            // 计算 OUT[B] = ∪ IN[S] (S 是 B 的后继)
            std::unordered_set<unsigned> new_out;
            for (auto *succ : bb->successors())
            {
                new_out.insert(block_info_[succ].in.begin(),
                               block_info_[succ].in.end());
            }

            // 计算 IN[B] = use[B] ∪ (OUT[B] - def[B])
            auto new_in = info.use;
            for (auto reg : new_out)
            {
                if (!info.def.count(reg))
                {
                    new_in.insert(reg);
                }
            }

            // 检测变化
            if (new_in != info.in || new_out != info.out)
            {
                changed = true;
                info.in = std::move(new_in);
                info.out = std::move(new_out);
            }
        }
        iter++;
    } while (changed);
    MO_DEBUG("Data flow analysis finished in %u iterations", iter);
}

// 查找块内首次定义位置
size_t LiveRangeAnalyzer::find_first_def_pos(MachineBasicBlock *bb, unsigned reg, size_t after_pos_exclusive) const
{
    for (auto &inst : bb->instructions())
    {
        size_t pos = mf_.get_global_instr_pos(inst.get());
        if (after_pos_exclusive != std::numeric_limits<size_t>::max() && pos <= after_pos_exclusive)
        {
            continue;
        }
        if (inst->defs().size())
        {
            return pos;
        }
    }
    return bb->global_end_inclusive(); // 无定义则延续到块尾
}

size_t LiveRangeAnalyzer::find_last_def_pos(MachineBasicBlock *bb, unsigned reg, size_t before_pos_exclusive) const
{
    for (auto it = bb->instructions().rbegin(); it != bb->instructions().rend(); ++it)
    {
        size_t pos = mf_.get_global_instr_pos((*it).get());
        if (before_pos_exclusive != std::numeric_limits<size_t>::max() && pos >= before_pos_exclusive)
        {
            continue;
        }
        if ((*it)->defs().count(reg))
        {
            return pos;
        }
    }
    return bb->global_start(); // 无定义则延续到块头
}

// 只考虑直接冲突
bool LiveRangeAnalyzer::has_conflict(unsigned reg1, unsigned reg2) const
{
    if (reg1 == reg2)
    {
        return false;
    }

    // FIXME: 对于 RISCV 架构，0 寄存器不参与冲突，但是其它架构得换个方式处理，比如 ia64 的 0 号是 rax，并非零值寄存器
    if (reg1 == 0 || reg2 == 0)
    {
        return false;
    }

    // 处理物理寄存器别名
    auto check_alias = [&](unsigned phys, unsigned virt)
    {
        if (!MachineFunction::is_physical_reg(phys))
        {
            return false;
        }
        if (auto *lr = get_live_range(virt))
        {
            return lr->is_allocated() && is_alias_(phys, lr->vreg());
        }
        return is_alias_(phys, virt);
    };

    if (MachineFunction::is_physical_reg(reg1) && check_alias(reg1, reg2))
    {
        return true;
    }

    if (MachineFunction::is_physical_reg(reg2) && check_alias(reg2, reg1))
    {
        return true;
    }

    // 获取活跃区间
    LiveRange *lr1 = get_live_range(reg1);
    LiveRange *lr2 = get_live_range(reg2);
    if (!lr1 || !lr2)
    {
        return false;
    }

    // 优化后的区间遍历
    auto &intervals1 = lr1->intervals();
    auto &intervals2 = lr2->intervals();
    auto it1 = intervals1.begin();
    auto it2 = intervals2.begin();

    while (it1 != intervals1.end() && it2 != intervals2.end())
    {
        if (it1->overlaps(*it2))
        {
            MO_DEBUG("has_confict: %s overlaps %s", it1->to_string().c_str(), it2->to_string().c_str());
            return true;
        }
        it1->start() < it2->start() ? ++it1 : ++it2;
    }

    return false;
}

// 辅助函数：计算基本块局部USE/DEF
void LiveRangeAnalyzer::compute_local_use_def(MachineBasicBlock *bb, std::unordered_set<unsigned> &use, std::unordered_set<unsigned> &def) const
{

    use.clear();
    def.clear();
    std::unordered_set<unsigned> temp_use;

    for (auto &inst : bb->instructions())
    {
        // 记录所有def寄存器
        for (auto &op : inst->operands())
        {
            if (op.is_reg() && op.is_def())
            {
                def.insert(op.reg());
            }
        }
        // 记录use寄存器（排除在def之后使用的）
        for (auto &op : inst->operands())
        {
            if (op.is_reg() && !op.is_def())
            {
                if (!def.count(op.reg()))
                {
                    temp_use.insert(op.reg());
                }
            }
        }
    }
    use = temp_use;
}

LiveRange *LiveRangeAnalyzer::get_live_range(unsigned reg) const
{
    if (live_ranges_dirty_)
    {
        compute();
    }

    auto it = reg_live_ranges_.find(reg);
    if (it == reg_live_ranges_.end())
    {
        return nullptr;
    }
    return it->second.get();
}

LiveRange *LiveRangeAnalyzer::get_physical_live_range(unsigned reg) const
{
    auto range = get_live_range(reg);
    if (range && range->is_allocated())
    {
        return range;
    }
    return nullptr;
}

std::map<unsigned, std::set<unsigned>> LiveRangeAnalyzer::build_interference_graph() const
{
    compute(); // 确保活跃区间数据最新

    std::map<unsigned, std::set<unsigned>> graph;
    const auto &ranges = reg_live_ranges_;

    // 遍历所有寄存器对（包括虚拟和物理）
    for (auto it1 = ranges.begin(); it1 != ranges.end(); ++it1)
    {
        const unsigned reg1 = it1->first;

        for (auto it2 = std::next(it1); it2 != ranges.end(); ++it2)
        {
            const unsigned reg2 = it2->first;

            // 使用 has_conflict 判断冲突
            if (has_conflict(reg1, reg2))
            {
                graph[reg1].insert(reg2);
                graph[reg2].insert(reg1);
            }
        }
    }

    return graph;
}

void LiveRangeAnalyzer::dump(std::ostream &os)
{
    os << "Live Range Analysis Result:\n";
    for (auto &lr : reg_live_ranges_)
    {
        os << lr.first << ":\n";
        os << lr.second->to_string() << "\n";
        os << "-----------\n";
    }
}

void LiveRangeAnalyzer::export_to_gantt_chart(std::ostream &os) const
{
    if (live_ranges_dirty_)
    {
        compute();
    }

    // Mermaid 甘特图头部
    os << "```";
    os << "gantt\n";
    os << "    title Active Range\n";
    os << "    dateFormat X\n";  // 使用自定义格式，X 表示指令位置
    os << "    axisFormat %s\n"; // 显示数字格式

    // 为每个寄存器创建一个部分
    for (const auto &[reg, live_range_ptr] : reg_live_ranges_)
    {
        const LiveRange &lr = *live_range_ptr;
        std::string reg_type = lr.is_allocated() ? "pr" : "vr";
        std::string reg_status = lr.is_spilled() ? " (spill to " + std::to_string(lr.spill_slot()) + ")" : "";

        os << "    section " << reg_type << " " << reg << reg_status << "\n";

        // 为每个区间创建一个任务
        int interval_idx = 0;
        for (const auto &interval : lr.intervals())
        {
            os << "    " << "R" << reg << "_" << interval_idx
               << " :active, " << interval.start() << ", " << interval.end() << "\n";
            interval_idx++;
        }
    }

    os << "```\n";
}

void LiveRangeAnalyzer::export_to_json(std::ostream &os) const
{
    if (live_ranges_dirty_)
    {
        compute();
    }

    std::stringstream json;
    json << "{\n";
    json << "  \"title\": \"Active Range\",\n";
    json << "  \"axisFormat\": \"X\",\n";
    json << "  \"sections\": [\n";

    bool first_section = true;
    for (const auto &[reg, live_range_ptr] : reg_live_ranges_)
    {
        const LiveRange &lr = *live_range_ptr;
        if (!first_section)
        {
            json << ",\n";
        }
        first_section = false;

        std::string reg_type = lr.is_allocated() ? "pr" : "vr";
        bool spilled = lr.is_spilled();
        int spill_slot = spilled ? lr.spill_slot() : -1;

        json << "    {\n";
        json << "      \"regType\": \"" << reg_type << "\",\n";
        json << "      \"reg\": " << reg << ",\n";
        json << "      \"spilled\": " << (spilled ? "true" : "false") << ",\n";
        json << "      \"spillSlot\": " << spill_slot << ",\n";
        json << "      \"intervals\": [\n";

        bool first_interval = true;
        for (const auto &interval : lr.intervals())
        {
            if (!first_interval)
            {
                json << ",\n";
            }
            first_interval = false;

            json << "        { \"start\": " << interval.start()
                 << ", \"end\": " << interval.end() << " }";
        }

        json << "\n      ]\n";
        json << "    }";
    }

    json << "\n  ]\n";
    json << "}\n";

    os << json.str();
}
