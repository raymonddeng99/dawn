import random

def universal_hash(a, b, c, d, e, key):
    m = 2 ** 32
    hash_value = (a * key + b * key ** 2 + c * key ** 3 + d * key ** 4 + e * key ** 5) % m
    return hash_value

def linear_probe(table, key):
    m = len(table)
    a = random.randint(0, m - 1)
    b = random.randint(0, m - 1)
    c = random.randint(0, m - 1)
    d = random.randint(0, m - 1)
    e = random.randint(0, m - 1)
    hash_value = universal_hash(a, b, c, d, e, key)
    for i in range(m):
        index = (hash_value + i) % m
        if table[index] is None:
            return index