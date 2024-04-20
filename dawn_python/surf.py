from collections import defaultdict

def build(keys):
    root = defaultdict(lambda: defaultdict())
    for key in keys:
        curr = root
        for char in key:
            curr = curr.setdefault(char, defaultdict())
    return root

def lookup(trie, key):
    curr = trie
    for char in key:
        if char not in curr:
            return False
        curr = curr[char]
    return True