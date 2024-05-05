from math import ceil, log2

class Tree:
    def __init__(self, values):
        self.values = values
        self.n = len(values)
        self.euler = [0] * (2 * self.n)
        self.first = [0] * (2 * self.n)
        self.rmq = []
        self._build_euler(0, 0)
        self._precompute_rmq()

    def _build_euler(self, tree, i):
        j = i + 1
        self.euler[i] = tree
        self.first[i] = j
        if j < 2 * self.n:
            self._build_euler(tree + 1, j)

    def _precompute_rmq(self):
        n = len(self.euler)
        k = int(ceil(log2(n))) + 1
        self.rmq = [[0] * n for _ in range(k)]

        for i in range(n):
            self.rmq[0][i] = i

        for j in range(1, k):
            for i in range(n - (1 << (j - 1))):
                x = self.rmq[j - 1][i]
                y = self.rmq[j - 1][i + (1 << (j - 1))]
                self.rmq[j][i] = x if self.euler[x] < self.euler[y] else y

    def query(self, l, r):
        l, r = min(l, r), max(l, r)
        k = int(log2(r - l + 1))
        x = self.rmq[k][self.first[l]]
        y = self.rmq[k][self.first[r] - (1 << k) + 1]
        return min(self.values[self.euler[x]], self.values[self.euler[y]])