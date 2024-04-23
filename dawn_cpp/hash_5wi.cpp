#include <cstdint>
#include <random>

uint32_t universal_hash(uint32_t a, uint32_t b, uint32_t c, uint32_t d, uint32_t e, uint32_t key) {
    const uint32_t m = 1U << 32;
    uint64_t hash = static_cast<uint64_t>(a) * key
                    + static_cast<uint64_t>(b) * key * key
                    + static_cast<uint64_t>(c) * key * key * key
                    + static_cast<uint64_t>(d) * key * key * key * key
                    + static_cast<uint64_t>(e) * key * key * key * key * key;
    return static_cast<uint32_t>(hash % m);
}

size_t linear_probe(std::vector<std::optional<int>>& table, uint32_t key) {
    const size_t m = table.size();
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<uint32_t> dis(0, m - 1);
    const uint32_t a = dis(gen);
    const uint32_t b = dis(gen);
    const uint32_t c = dis(gen);
    const uint32_t d = dis(gen);
    const uint32_t e = dis(gen);
    const uint32_t hash = universal_hash(a, b, c, d, e, key);
    for (size_t i = 0; ; ++i) {
        const size_t index = (hash + i) % m;
        if (!table[index].has_value()) {
            return index;
        }
    }
}