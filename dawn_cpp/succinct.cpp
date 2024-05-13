#include <algorithm>
#include <array>
#include <string>
#include <vector>
#include <cmath>

class SuffixArray {
public:
    std::vector<int> construct(const std::string& text) {
        int n = text.length();
        std::array<int, 256> rankOfChar = computeRankOfChar(text);
        std::vector<int> ranks(n);
        for (int i = 0; i < n; i++) {
            ranks[i] = rankSuffix(text, rankOfChar, n, i);
        }
        std::vector<int> indices(n);
        std::iota(indices.begin(), indices.end(), 0);
        std::sort(indices.begin(), indices.end(), [&ranks](int i, int j) {
            return ranks[i] < ranks[j];
        });
        return indices;
    }

private:
    int rankSuffix(const std::string& text, const std::array<int, 256>& rankOfChar, int n, int i) {
        int r = 0;
        for (int j = i; j < n; j++) {
            r = r * 256 + rankOfChar[text[j]];
        }
        return r;
    }

    std::array<int, 256> computeRankOfChar(const std::string& text) {
        std::array<int, 256> rankOfChar = {};
        std::array<bool, 256> used = {};
        int rank = 0;
        for (char c : text) {
            if (!used[c]) {
                rankOfChar[c] = rank++;
                used[c] = true;
            }
        }
        return rankOfChar;
    }
};


// Succinct static data structures, Jacobsen '89
class BitVector {
public:
    BitVector(const std::vector<bool>& bv) : bits(bv) {
        int n = bits.size();
        rank_table.resize(n + 1);
        select_table.resize(n + 1);

        for (int i = 0; i <= n; i++) {
            rank_table[i] = (i % (1 << k)) ? rank_table[i - (i % (1 << k))] + rank_naive(i) - rank_naive(i - (i % (1 << k))) : 0;
            select_table[i] = (i % (1 << k)) ? select_table[i - (i % (1 << k))] + select_naive(i) - select_naive(i - (i % (1 << k))) : -1;
        }
    }

    int rank(int i) {
        return rank_table[i];
    }

    int select(int i) {
        int l = 0, r = bits.size();
        while (l <= r) {
            int m = l + (r - l) / 2;
            if (rank(m) < i)
                l = m + 1;
            else
                r = m - 1;
        }
        return l;
    }
private:
    std::vector<bool> bits;
    std::vector<int> rank_table;
    std::vector<int> select_table;
    const int k = 6;

    int rank_naive(int i) {
        int cnt = 0;
        for (int j = 0; j <= i; j++)
            if (bits[j])
                cnt++;
        return cnt;
    }

    int select_naive(int i) {
        int cnt = 0, j;
        for (j = 0; cnt < i; j++)
            if (bits[j])
                cnt++;
        return (cnt == i) ? j - 1 : -1;
    }
};