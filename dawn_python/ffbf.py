from bitarray import bitarray
from functools import reduce

class BloomFilter:
    def __init__(self, size, hash_funcs):
        self.size = size
        self.vector = bitarray(size)
        self.vector.setall(0)
        self.hash_funcs = hash_funcs

    def add(self, element):
        for hash_func in self.hash_funcs:
            bit_index = hash_func(element) % self.size
            self.vector[bit_index] = 1

    def contains(self, element):
        return all(self.vector[hash_func(element) % self.size] for hash_func in self.hash_funcs)