from typing import Generic, TypeVar, List, Tuple, Optional
import heapq

T = TypeVar('T')

class Deque(Generic[T]):
    def __init__(self):
        self.front: List[Tuple[T, ...]] = []
        self.back: List[Tuple[T, ...]] = []

    def is_empty(self) -> bool:
        return len(self.front) == 0 and len(self.back) == 0

    def add_front(self, x: T) -> None:
        new_front = []
        for node in self.front:
            if len(node) == 1:
                new_front.append((node[0], node[0], node[0], x))
                new_front.append((node[0],))
            elif len(node) == 2:
                new_front.append((node[0], node[0], node[1]))
                new_front.append((x, x, x, node[1]))
            elif len(node) == 3:
                new_front.append((node[0], node[1]))
                new_front.append((node[1], node[2]))
                new_front.append((node[2], node[2], node[2], node[2]))
                new_front.append((x,))
            else:
                new_front.append(node[:3])
                new_front.extend([(y,) for y in node[3:]])
                new_front.append((x,))
        new_front.append((x, x, x, x))
        self.front = new_front
        self.back = []

    def add_back(self, x: T) -> None:
        new_back = []
        for node in self.back:
            if len(node) == 1:
                new_back.append((node[0],))
                new_back.append((x, x, x, x))
            elif len(node) == 2:
                new_back.append((node[0], node[1], node[1]))
                new_back.append((node[0], node[0], node[0], x))
            elif len(node) == 3:
                new_back.append((node[0], node[1]))
                new_back.append((node[2], node[2]))
                new_back.append((node[1], node[1], node[1], node[1]))
                new_back.append((x,))
            else:
                new_back.extend([(y,) for y in node[:3]])
                new_back.append(node[3:])
                new_back.append((x,))
        new_back.append((x, x, x, x))
        self.front = []
        self.back = new_back

    def take_front(self) -> Optional[T]:
        if not self.front:
            return None
        node = self.front.pop(0)
        if len(node) == 1:
            x = node[0]
        elif len(node) == 2:
            x = node[0]
            self.front.insert(0, (node[1],))
        elif len(node) == 3:
            x = node[0]
            self.front.insert(0, (node[1], node[2]))
        else:
            x = node[0]
            self.front.insert(0, node[1:])
        if not self.front:
            self.front = list(reversed(self.back))
            self.back = []
        return x

    def take_back(self) -> Optional[T]:
        if not self.back:
            return None
        node = self.back.pop()
        if len(node) == 1:
            x = node[0]
        elif len(node) == 2:
            x = node[1]
            self.back.append((node[0],))
        elif len(node) == 3:
            x = node[2]
            self.back.append((node[0], node[1]))
        else:
            x = node[-1]
            self.back.append(node[:-1])
        return x


# Partially retroactive priority queue
class PriorityQueue:
    def __init__(self):
        self.data = []
        self.time = 0
        self.retrodata = []

    def is_empty(self):
        return len(self.data) == 0

    def insert(self, x):
        self.time += 1
        heapq.heappush(self.data, (self.time, x))

    def find_min(self):
        if self.is_empty():
            return None
        else:
            return self.data[0][1]

    def delete_min(self):
        if self.is_empty():
            return None
        else:
            min_val = heapq.heappop(self.data)[1]
            return min_val

    def retroactive_update(self, t, x):
        self.retrodata = [(self.data[i][0], x) if self.data[i][0] <= t else self.data[i] for i in range(len(self.data))]
        self.data = self.retrodata
        heapq.heapify(self.data)
        self.retrodata = []



# Simple Confluently Persistent Catenable Lists, Tarjan et al
class Node:
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

class PersistentList:
    def __init__(self):
        self.left = None
        self.right = None

    @staticmethod
    def singleton(value):
        node = Node(value)
        return PersistentList(node, node)

    def is_empty(self):
        return self.left is None and self.right is None

    def cons(self, value):
        node = Node(value, None, self.left)
        return PersistentList(node, self.right)

    def head(self):
        if self.left is not None:
            return self.left.value
        raise ValueError("Empty list")

    def tail(self):
        if self.left is self.right:
            return PersistentList()
        if self.right is not None:
            return PersistentList(self.right.left, self.right.right)
        raise ValueError("Empty list")

    def catenate(self, other):
        if self.right is other.left:
            return PersistentList(self.left, other.right)
        node = Node(None, self.right, other.left)
        return PersistentList(self.left, other.right)

    def __init__(self, left=None, right=None):
        self.left = left
        self.right = right