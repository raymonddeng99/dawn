# On RAM priority queues, Thorup '96 

class VEBTree:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None
        self.isLeaf = True

    def min(self):
        if self.isLeaf:
            return self.value
        minLeft = float('inf') if self.left is None else self.left.min()
        minRight = float('inf') if self.right is None else self.right.min()
        return min(minLeft, minRight)

    def insert(self, value):
        if self.isLeaf:
            if value < self.value:
                self.right = VEBTree(self.value)
                self.value = value
            else:
                self.right = VEBTree(value)
            self.isLeaf = False
        else:
            if value < self.min():
                if self.left is None:
                    self.left = VEBTree(value)
                else:
                    self.left.insert(value)
            else:
                if self.right is None:
                    self.right = VEBTree(value)
                else:
                    self.right.insert(value)

    def extract_min(self):
        if self.isLeaf:
            return self.value, None

        minVal, minTree = self.min(), None
        if self.left is not None and self.left.min() == minVal:
            minVal, self.left = self.left.extract_min()
        else:
            minVal, self.right = self.right.extract_min()

        if self.left is None and self.right is None:
            self.isLeaf = True

        return minVal, minTree

    def decrease_key(self, old_value, new_value):
        if self.isLeaf:
            if self.value == old_value:
                self.value = new_value
            return

        if old_value < self.min():
            if self.left is not None:
                self.left.decrease_key(old_value, new_value)
        else:
            if self.right is not None:
                self.right.decrease_key(old_value, new_value)

        if new_value < self.min():
            self.value = new_value