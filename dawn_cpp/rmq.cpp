#include <algorithm>
#include <cmath>
#include <vector>

struct Tree {
    std::vector<int> values;
    std::vector<int> euler;
    std::vector<int> first;
    std::vector<std::vector<int>> rmq;
};

int log2(int n) {
    return static_cast<int>(std::ceil(std::log2(n)));
}

void precomputeRMQ(Tree& tree) {
    int n = tree.euler.size();
    int k = log2(n) + 1;
    tree.rmq = std::vector<std::vector<int>>(k, std::vector<int>(n));

    for (int i = 0; i < n; ++i) {
        tree.rmq[0][i] = i;
    }

    for (int j = 1; j < k; ++j) {
        for (int i = 0; i + (1 << (j - 1)) < n; ++i) {
            int x = tree.rmq[j - 1][i];
            int y = tree.rmq[j - 1][i + (1 << (j - 1))];
            tree.rmq[j][i] = (tree.euler[x] < tree.euler[y]) ? x : y;
        }
    }
}

int query(const Tree& tree, int l, int r) {
    l = std::min(l, r);
    r = std::max(l, r);
    int k = log2(r - l + 1);
    int x = tree.rmq[k][tree.first[l]];
    int y = tree.rmq[k][tree.first[r] - (1 << k) + 1];
    return std::min(tree.values[tree.euler[x]], tree.values[tree.euler[y]]);
}

Tree fromTree(const std::vector<int>& values) {
    int n = values.size();
    std::vector<int> euler(2 * n);
    std::vector<int> first(2 * n);

    auto buildEuler = [&](int tree, int i) {
        int j = i + 1;
        euler[i] = tree;
        first[i] = j;
        if (j < 2 * n) {
            buildEuler(tree + 1, j);
        }
    };
    buildEuler(0, 0);

    Tree tree = {values, euler, first, {}};
    precomputeRMQ(tree);
    return tree;
}