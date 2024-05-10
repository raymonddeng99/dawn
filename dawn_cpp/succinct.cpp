#include <algorithm>
#include <array>
#include <string>
#include <vector>

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