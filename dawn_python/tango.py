class AuxTree:
    def __init__(self, key):
        self.key = key
        self.left = None
        self.right = None

    def insert(self, key):
        if key < self.key:
            if self.left is None:
                self.left = AuxTree(key)
            else:
                self.left.insert(key)
        else:
            if self.right is None:
                self.right = AuxTree(key)
            else:
                self.right.insert(key)

    def find(self, key):
        if key == self.key:
            return True
        elif key < self.key:
            return self.left is not None and self.left.find(key)
        else:
            return self.right is not None and self.right.find(key)

class TangoTree:
    def __init__(self):
        self.aux_trees = [AuxTree(0)]

    def find(self, key):
        return any(aux.find(key) for aux in self.aux_trees)

    def update(self, key):
        left, right = [], []
        for aux in self.aux_trees:
            if aux.key < key:
                left.append(aux)
            else:
                right.append(aux)
        self.aux_trees = left + [AuxTree(key)] + right