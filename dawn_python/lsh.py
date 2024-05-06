class LSHHash:
    def hash_str(s):
        return sum(map(ord, s))

    def hamming_distance(s1, s2):
        return sum(c1 != c2 for c1, c2 in zip(s1, s2))

    def lsh_func(k, l, s):
        n = len(s)
        hashes = []
        for i in range(k):
            start = i * n // k
            end = start + l * n // k
            hashes.append(LSHHash.hash_str(s[start:end]))
        return hashes

    def build_lsh_table(k, l, strings):
        table = {}
        for s in strings:
            hashes = LSHHash.lsh_func(k, l, s)
            for h in hashes:
                table.setdefault(h, []).append(s)
        return table

    def query_lsh_table(k, l, table, query_str):
        candidates = []
        hashes = LSHHash.lsh_func(k, l, query_str)
        for h in hashes:
            for s in table.get(h, []):
                if LSHHash.hamming_distance(s, query_str) <= l:
                    candidates.append(s)
        return candidates