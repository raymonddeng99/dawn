class BWT:
    def encode(s):
        n = len(s)
        rotations = sorted([''.join([s[i:], s[:i]]) for i in range(n)])
        encoded = ''.join([r[-1] for r in rotations])
        idx = rotations.index(s)
        return (encoded, idx)

    def decode(encoded, idx):
        n = len(encoded)
        rotations = sorted([''.join([c, BWT.last_char(encoded[:i] + encoded[i+1:])]) for i, c in enumerate(encoded)])
        decoded = ''.join([r[-1] for r in rotations])
        return decoded[idx:] + decoded[:idx]

    @staticmethod
    def last_char(s):
        return sorted(s)[-1]