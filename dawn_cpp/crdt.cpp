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