class CompactSuffixArray:
    def __init__(self, text):
        self.text = text + '$'
        self.rank_of_char = self.compute_rank_of_char()
        self.ranks = [self.rank_suffix(i) for i in range(len(self.text))]
        self.indices = sorted(range(len(self.text)), key=lambda i: self.ranks[i])

    def rank_suffix(self, i):
        n = len(self.text)
        r = 0
        for j in range(i, n):
            r = r * len(self.rank_of_char) + self.rank_of_char[ord(self.text[j])]
        return r

    def compute_rank_of_char(self):
        rank_of_char = [0] * 256
        used = [False] * 256
        rank = 0
        for c in self.text:
            if not used[ord(c)]:
                rank_of_char[ord(c)] = rank
                rank += 1
                used[ord(c)] = True
        return rank_of_char

    def compact_suffix_array(self):
        return self.indices


# Succinct static data structures, Jacobsen '89
class BitVector:
    k = 6
    
    def __init__(self, bits):
        self.bits = bits
        n = len(bits)
        self.rank_table = [0] * (n + 1)
        self.select_table = [-1] * (n + 1)

        for i in range(n + 1):
            if i % (1 << self.k) == 0:
                self.rank_table[i] = 0
                self.select_table[i] = -1
            else:
                prev = i - (i % (1 << self.k))
                self.rank_table[i] = self.rank_table[prev] + self.rank_naive(bits, i) - self.rank_naive(bits, prev)
                self.select_table[i] = self.select_table[prev] + self.select_naive(bits, i) - self.select_naive(bits, prev)

    def rank_naive(self, bits, i):
        return sum(1 for b in bits[:i + 1] if b)

    def select_naive(self, bits, i):
        cnt, j = 0, 0
        for j in range(len(bits)):
            if bits[j]:
                cnt += 1
                if cnt == i + 1:
                    return j
        return -1

    def rank(self, i):
        return self.rank_table[i]

    def select(self, i):
        l, r = 0, len(self.bits)
        while l <= r:
            m = (l + r) // 2
            if self.rank(m) < i:
                l = m + 1
            else:
                r = m - 1
        return l if l < len(self.bits) and self.rank(l) == i else -1