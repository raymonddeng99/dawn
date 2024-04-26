class XFastTrie:
    def __init__(self, w):
        u = 1 << w
        self.bits = [False] * (2 * u - 1)
        self.ones = []
        self.size = 0

    def insert(self, x):
        self._insert_aux(x, 0)

    def _insert_aux(self, x, i):
        if i >= len(self.bits):
            self.size += 1
            return
        bit = (x & (1 << i)) != 0
        index = 1 << (len(self.bits) - 1 - i)
        self.bits[index - 1] = self.bits[index - 1] or bit
        if bit:
            self.ones.append(index)
            self._insert_aux(x, i + 1)
        else:
            self._insert_aux(x, i + 1)

    def predecessor(self, x):
        return self._predecessor_aux(x, 0)

    def _predecessor_aux(self, x, i):
        if i >= len(self.bits):
            return None
        index = 1 << (len(self.bits) - 1 - i)
        if self.bits[index - 1]:
            left_child = 2 * index
            right_child = left_child + 1
            if self.bits[right_child - 1]:
                return self._predecessor_aux(x, i + 1)
            else:
                return self.ones[0]
        else:
            return self._predecessor_aux(x, i + 1)