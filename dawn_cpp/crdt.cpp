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