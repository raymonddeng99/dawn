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