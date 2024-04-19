#include <bitset>
#include <functional>
#include <string>
#include <vector>

class BloomFilter {
private:
    std::bitset<1000000> bits;
    std::vector<std::function<std::size_t(std::string)>> hash_funcs;

public:
    BloomFilter(const std::vector<std::function<std::size_t(std::string)>>& hash_funcs)
        : hash_funcs(hash_funcs) {}

    void add(const std::string& element) {
        for (const auto& hash_func : hash_funcs) {
            bits.set(hash_func(element) % bits.size());
        }
    }

    bool contains(const std::string& element) const {
        for (const auto& hash_func : hash_funcs) {
            if (!bits.test(hash_func(element) % bits.size())) {
                return false;
            }
        }
        return true;
    }
};