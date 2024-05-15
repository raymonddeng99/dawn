class PriorityQueue:
    CLUSTER_SIZE = 2

    def __init__(self):
        self.size = 0
        self.root = None

    def is_empty(self):
        return self.size == 0

    def insert(self, value):
        if self.root is None:
            self.root = Cluster(value)
        else:
            self.root = self.root.insert(value)
        self.size += 1

    def find_min(self):
        if self.root is None:
            raise Exception("Priority queue is empty")
        return self.root.find_min()

    def remove_min(self):
        if self.root is None:
            raise Exception("Priority queue is empty")
        min_val, new_root = self.root.remove_min()
        self.root = new_root
        self.size -= 1
        return min_val

class Cluster:
    def __init__(self, value=None):
        self.value = value
        self.children = [None] * PriorityQueue.CLUSTER_SIZE

    def insert(self, value):
        if self.value is None:
            return Cluster(value)
        for i in range(PriorityQueue.CLUSTER_SIZE):
            if self.children[i] is None:
                self.children[i] = Cluster(value)
                return self
            new_child = self.children[i].insert(value)
            if new_child is not None:
                self.children[i] = new_child
                return self
        children = [child if i < len(self.children) else Cluster(value) for i, child in enumerate([None] * PriorityQueue.CLUSTER_SIZE)]
        return Cluster(children=children)

    def find_min(self):
        if self.value is not None:
            return self.value
        min_val = None
        for child in self.children:
            if child is not None:
                child_min = child.find_min()
                if min_val is None or child_min < min_val:
                    min_val = child_min
        return min_val

    def remove_min(self):
        if self.value is not None:
            return self.value, None
        min_child_idx = None
        min_child = None
        for i, child in enumerate(self.children):
            if child is not None:
                if min_child is None or child.find_min() < min_child.find_min():
                    min_child_idx = i
                    min_child = child
        min_val, new_child = min_child.remove_min()
        if new_child is None:
            self.children[min_child_idx] = None
        else:
            self.children[min_child_idx] = new_child
        return min_val, self if any(child is not None for child in self.children) else None