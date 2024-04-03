// CRDTs

/*
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
*/

#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <stdexcept>
#include <atomic>
#include <utility>
#include <mutex>
#include <unordered_set>
#include <unordered_map>

enum class Operation { Increment, Decrement };

struct Counter {
    int value;
    std::vector<Operation> ops;

    Counter() : value(0), ops() {}

    Counter apply() const {
        Counter result = *this;
        for (const auto& op : ops) {
            switch (op) {
                case Operation::Increment:
                    result.value++;
                    break;
                case Operation::Decrement:
                    result.value--;
                    break;
            }
        }
        result.ops.clear();
        return result;
    }

    Counter merge(const Counter& other) const {
        Counter result;
        result.value = std::max(value, other.value);
        result.ops = ops;
        result.ops.insert(result.ops.end(), other.ops.begin(), other.ops.end());
        for (const auto& op : result.ops) {
            switch (op) {
                case Operation::Increment:
                    result.value++;
                    break;
                case Operation::Decrement:
                    result.value--;
                    break;
            }
        }
        result.ops.clear();
        return result;
    }

    static std::vector<Operation> downstream() {
        return {};
    }

    Counter update(Operation op) const {
        Counter result = *this;
        result.ops.push_back(op);
        return result;
    }
};


// State based increment-only counter
class GCounter {
public:
    GCounter(int size) : data(size, 0) {}

    void update(int i) {
        if (i < 0 || i >= static_cast<int>(data.size())) {
            throw std::out_of_range("Index out of bounds");
        }
        ++data[i];
    }

    int query(int i) const {
        if (i < 0 || i >= static_cast<int>(data.size())) {
            throw std::out_of_range("Index out of bounds");
        }
        return data[i];
    }

    int compare(const GCounter& other) const {
        if (data.size() != other.data.size()) {
            throw std::invalid_argument("Vectors have different lengths");
        }
        return std::lexicographical_compare(data.begin(), data.end(), other.data.begin(), other.data.end());
    }

    GCounter merge(const GCounter& other) const {
        if (data.size() != other.data.size()) {
            throw std::invalid_argument("Vectors have different lengths");
        }
        std::vector<int> result(data.size());
        std::transform(data.begin(), data.end(), other.data.begin(), result.begin(),
                       [](int a, int b) { return std::max(a, b); });
        return Counter(result);
    }

private:
    std::vector<int> data;
};


// State-based PN Counter
class PNCounter {
public:
    PNCounter(int size) : p(size, 0), n(size, 0) {}

    static PNCounter initialize(int size, const std::vector<int>& p, const std::vector<int>& q) {
        return {p, q};
    }

    void increment() {
        int g = myID();
        p[g]++;
    }

    void decrement() {
        int g = myID();
        n[g]++;
    }

    int value() const {
        int sum_p = std::accumulate(p.begin(), p.end(), 0);
        int sum_n = std::accumulate(n.begin(), n.end(), 0);
        return sum_p - sum_n;
    }

    bool compare(const PNCounter& other) const {
        return std::equal(p.begin(), p.end(), other.p.begin(), std::less_equal<int>()) &&
               std::equal(n.begin(), n.end(), other.n.begin(), std::less_equal<int>());
    }

    PNCounter merge(const PNCounter& other) const {
        if (p.size() != other.p.size() || n.size() != other.n.size()) {
            throw std::invalid_argument("Vectors have different lengths");
        }
        PNCounter z(p.size());
        std::transform(p.begin(), p.end(), other.p.begin(), z.p.begin(), std::max<int>);
        std::transform(n.begin(), n.end(), other.n.begin(), z.n.begin(), std::max<int>);
        return z;
    }

private:
    std::vector<int> p;
    std::vector<int> n;
};


// State-based last-writer-wins register
template <typename T>
class LastWriterWinsRegister {
private:
    std::atomic<T> value;
    std::atomic<double> timestamp;

public:
    LastWriterWinsRegister(T initialValue) : value(initialValue), timestamp(0.0) {}

    T read() const {
        return value.load(std::memory_order_acquire);
    }

    void write(T newValue, double newTimestamp) {
        T oldValue = value.load(std::memory_order_relaxed);
        double oldTimestamp = timestamp.load(std::memory_order_relaxed);

        while (newTimestamp > oldTimestamp &&
               !std::atomic_compare_exchange_strong(
                   &value, &oldValue, newValue,
                   std::memory_order_acq_rel,
                   std::memory_order_relaxed)) {
            oldTimestamp = timestamp.load(std::memory_order_relaxed);
        }

        if (newTimestamp > oldTimestamp) {
            timestamp.store(newTimestamp, std::memory_order_release);
        }
    }

    bool compareAndSwap(T expectedValue, double expectedTimestamp, T newValue, double newTimestamp) {
        T oldValue = value.load(std::memory_order_acquire);
        double oldTimestamp = timestamp.load(std::memory_order_acquire);

        if (oldValue == expectedValue && oldTimestamp == expectedTimestamp) {
            write(newValue, newTimestamp);
            return true;
        }

        return false;
    }
};


// Operation-based last-writer-wins register
struct OpBasedLWWValue {
    int val;
    double ts;

    OpBasedLWWValue(int val, double ts) : val(val), ts(ts) {}
};

enum class OpType { Update, Reset };

class OpBasedLWWRegister {
private:
    std::atomic<OpBasedLWWValue> value;
    std::vector<std::pair<OpType, OpBasedLWWValue>> pending;
    std::mutex mtx;

public:
    OpBasedLWWRegister(int initialValue)
        : value(OpBasedLWWValue(initialValue, 0.0)), pending() {}

    int read() const {
        return value.load(std::memory_order_acquire).val;
    }

    void update(int newValue, double newTimestamp) {
        std::lock_guard<std::mutex> lock(mtx);
        OpBasedLWWValue oldValue = value.load(std::memory_order_relaxed);
        if (newTimestamp > oldValue.ts) {
            value.store(OpBasedLWWValue(newValue, newTimestamp), std::memory_order_release);
            pending.clear();
        } else {
            pending.emplace_back(OpType::Update, OpBasedLWWValue(newValue, newTimestamp));
        }
    }

    void reset() {
        std::lock_guard<std::mutex> lock(mtx);
        pending.emplace_back(OpType::Reset, OpBasedLWWValue(0, 0.0));
    }

    void applyPending() {
        std::vector<std::pair<OpType, OpBasedLWWValue>> newPending;
        for (const auto& [op, arg] : pending) {
            switch (op) {
            case OpType::Update:
                if (arg.ts > value.load(std::memory_order_relaxed).ts) {
                    value.store(arg, std::memory_order_release);
                } else {
                    newPending.emplace_back(OpType::Update, arg);
                }
                break;
            case OpType::Reset:
                value.store(OpBasedLWWValue(0, 0.0), std::memory_order_release);
                break;
            }
        }
        pending = std::move(newPending);
    }

    void downstream() {
        std::lock_guard<std::mutex> lock(mtx);
        applyPending();
    }
};

// State-based multi-value register
struct MVRegisterValue {
    int x;
    std::vector<int64_t> v;

    MVRegisterValue(int x, const std::vector<int64_t>& v) : x(x), v(v) {}
};

class MVRegister {
private:
    std::vector<MVRegisterValue> payload;

public:
    MVRegister() {
        payload.emplace_back(MVRegisterValue(-1, std::vector<int64_t>{}));
    }

    std::vector<int64_t> queryIncrementVV(int processID) {
        int64_t maxVersion = 0;
        for (const auto& entry : payload) {
            for (const auto& v : entry.v) {
                maxVersion = std::max(maxVersion, v);
            }
        }
        maxVersion++;

        std::vector<int64_t> newVersion(payload.size(), maxVersion);
        newVersion[processID]++;
        return newVersion;
    }

    void updateAssign(const std::vector<int>& set_r, int processID) {
        std::vector<int64_t> newVersion = queryIncrementVV(processID);
        for (int x : set_r) {
            payload.emplace_back(MVRegisterValue(x, newVersion));
        }
    }

    const std::vector<MVRegisterValue>& queryValue() const {
        return payload;
    }

    bool compare(const MVRegister& other) const {
        for (const auto& entryA : payload) {
            for (const auto& entryB : other.payload) {
                if (entryA.x == entryB.x) {
                    for (const auto& vA : entryA.v) {
                        if (std::all_of(entryB.v.begin(), entryB.v.end(), [&vA](int64_t vB) { return vA > vB; })) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    MVRegister merge(const MVRegister& other) const {
        MVRegister merged;
        auto includeEntry = [&](const MVRegisterValue& entry, const std::vector<MVRegisterValue>& otherPayload) {
            return std::any_of(otherPayload.begin(), otherPayload.end(), [&entry](const MVRegisterValue& otherEntry) {
                const auto& [x, v] = entry;
                const auto& [x_, w] = otherEntry;
                if (x == x_) {
                    return *std::max_element(w.begin(), w.end()) >= v.back() ||
                           std::any_of(v.begin(), v.end(), [&w](int64_t vEntry) {
                               return std::all_of(w.begin(), w.end(), [vEntry](int64_t wEntry) {
                                   return vEntry > wEntry;
                               });
                           });
                }
                return false;
            });
        };

        for (const auto& entry : payload) {
            if (includeEntry(entry, other.payload)) {
                merged.payload.push_back(entry);
            }
        }

        for (const auto& otherEntry : other.payload) {
            if (includeEntry(otherEntry, payload)) {
                merged.payload.push_back(otherEntry);
            }
        }

        return merged;
    }
};


// State-based grow-only set
template <typename T>
class GSet {
public:
  GSet() : data(std::unordered_set<T>()) {}

  void add(const T& element) {
    data.insert(element);
  }

  bool lookup(const T& element) const {
    return data.count(element) > 0;
  }

  bool compare(const GSet<T>& other) const {
    return data == other.data;
  }

  GSet<T> merge(const GSet<T>& other) const {
    GSet<T> merged;
    merged.data.insert(data.begin(), data.end());
    merged.data.insert(other.data.begin(), other.data.end());
    return merged;
  }

private:
  std::unordered_set<T> data;
};


// State-based 2P set
template <typename T>
class StateBased2PSet {
private:
    std::set<T> added;
    std::set<T> removed;

public:
    StateBased2PSet() = default;

    bool lookup(const T& e) const {
        return (added.count(e) > 0) && (removed.count(e) == 0);
    }

    void add(const T& e) {
        if (!lookup(e)) {
            added.insert(e);
        }
    }

    void remove(const T& e) {
        if (lookup(e)) {
            removed.insert(e);
        }
    }

    bool compare(const StateBased2PSet& other) const {
        std::set<T> temp_added, temp_removed;
        std::set_intersection(added.begin(), added.end(), other.added.begin(), other.added.end(),
                              std::inserter(temp_added, temp_added.begin()));
        std::set_intersection(removed.begin(), removed.end(), other.removed.begin(), other.removed.end(),
                              std::inserter(temp_removed, temp_removed.begin()));
        return (temp_added == added) && (temp_removed == removed);
    }

    StateBased2PSet merge(const StateBased2PSet& other) const {
        StateBased2PSet<T> merged;
        std::set_union(added.begin(), added.end(), other.added.begin(), other.added.end(),
                       std::inserter(merged.added, merged.added.begin()));
        std::set_union(removed.begin(), removed.end(), other.removed.begin(), other.removed.end(),
                       std::inserter(merged.removed, merged.removed.begin()));
        return merged;
    }
};


// Op based 2p set with unique elements
template <typename T>
class USet {
public:
    USet(const std::vector<T>& p, const std::vector<T>& q) : p(p), n(q) {}

    static USet initialize(int size, const std::vector<T>& p, const std::vector<T>& q) {
        return USet(p, q);
    }

    void add(const T& element) {
        p.push_back(element);
    }

    bool lookup(const T& element) const {
        return std::find(p.begin(), p.end(), element) != p.end() &&
               std::find(n.begin(), n.end(), element) == n.end();
    }

    void remove(const T& element) {
        if (lookup(element)) {
            n.push_back(element);
        }
    }

    bool compare(const USet& other) const {
        return std::is_permutation(p.begin(), p.end(), other.p.begin(), other.p.end()) &&
               std::is_permutation(n.begin(), n.end(), other.n.begin(), other.n.end());
    }

    USet merge(const USet& other) const {
        std::vector<T> merged_p(p), merged_n(n);
        merged_p.insert(merged_p.end(), other.p.begin(), other.p.end());
        merged_n.insert(merged_n.end(), other.n.begin(), other.n.end());
        return USet(merged_p, merged_n);
    }

private:
    std::vector<T> p;
    std::vector<T> n;
};


// Molli, Weiss, Skaf set
template <typename T>
class MWSSet {
private:
    std::unordered_map<T, int> data;

public:
    MWSSet() {}

    bool lookup(const T& e) const {
        auto it = data.find(e);
        return it != data.end() && it->second > 0;
    }

    void add(const T& e) {
        auto it = data.find(e);
        int j;
        if (it != data.end() && it->second < 0) {
            j = -it->second + 1;
        } else {
            j = 1;
        }
        data[e] = j;
    }

    void remove(const T& e) {
        auto it = data.find(e);
        if (it != data.end() && it->second > 0) {
            data[e] = it->second - 1;
            if (data[e] == 0) {
                data.erase(it);
            }
        }
    }
};


// Operation based observed-remove set

template <typename T>
class ORSet {
private:
    std::unordered_set<T> added;
    std::unordered_set<T> removed;

public:
    ORSet() {}

    bool lookup(const T& e) const {
        return added.count(e) && !removed.count(e);
    }

    void add(const T& e) {
        if (!lookup(e)) {
            added.insert(e);
        }
    }

    void remove(const T& e) {
        if (lookup(e)) {
            removed.insert(e);
        }
    }
};


// Operation based 2P2P graph
using Vertex = int;
using Edge = std::pair<Vertex, Vertex>;

class TwoPGraph {
private:
    std::unordered_set<Vertex> va, vr;
    std::unordered_set<Edge> ea, er;

public:
    TwoPGraph() = default;
};