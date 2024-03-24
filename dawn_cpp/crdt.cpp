// CRDTs

/*
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
*/

#include <iostream>
#include <vector>
#include <algorithm>
#include <stdexcept>

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