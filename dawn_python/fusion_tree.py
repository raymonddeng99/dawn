class FusionTree:
    def __init__(self, value=0, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    @classmethod
    def empty(cls):
        return cls(0)

    def is_empty(self):
        return self.value == 0 and self.left is None and self.right is None

    def insert(self, x):
        if self.is_empty():
            return FusionTree(x)
        else:
            left = self.left.insert(x) if self.left else None
            right = self.right
            return FusionTree(self.value, left, right)

    def find(self, x):
        if self.is_empty():
            return 0
        elif self.value == x:
            left_sum = self.left.find(x) if self.left else 0
            right_sum = self.right.find(x) if self.right else 0
            return self.value + left_sum + right_sum
        else:
            left_sum = self.left.find(x) if self.left else 0
            right_sum = self.right.find(x) if self.right else 0
            return left_sum + right_sum

    @staticmethod
    def union(left, right):
        if left.is_empty():
            return right
        elif right.is_empty():
            return left
        else:
            new_left = FusionTree.union(left.left, right.left)
            new_right = FusionTree.union(left.right, right.right)
            return FusionTree(left.value + right.value, new_left, new_right)