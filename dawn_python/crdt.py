# CRDTs

'''
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
'''

from enum import Enum
from typing import List, Tuple, TypeVar, Generic
from threading import Lock

class Operation(Enum):
    Increment = 1
    Decrement = 2

Counter = Tuple[int, List[Operation]]

def initial() -> Counter:
    return (0, [])

def apply(counter: Counter) -> Counter:
    value, ops = counter
    for op in ops:
        if op == Operation.Increment:
            value += 1
        elif op == Operation.Decrement:
            value -= 1
    return (value, [])

def merge(counter1: Counter, counter2: Counter) -> Counter:
    value1, ops1 = counter1
    value2, ops2 = counter2
    merged_ops = ops1 + ops2
    final_value = max(value1, value2)
    for op in merged_ops:
        if op == Operation.Increment:
            final_value += 1
        elif op == Operation.Decrement:
            final_value -= 1
    return (final_value, [])

def downstream() -> List[Operation]:
    return []

def update(counter: Counter, operation: Operation) -> Counter:
    value, ops = counter
    new_ops = ops + [operation]
    return (value, new_ops)

# State based increment-only counter
class GCounter:
    def __init__(self, size):
        self.data = [0] * size

    def update(self, i):
        if i < 0 or i >= len(self.data):
            raise IndexError("Index out of bounds")
        self.data[i] += 1

    def query(self, i):
        if i < 0 or i >= len(self.data):
            raise IndexError("Index out of bounds")
        return self.data[i]

    def compare(self, other):
        if len(self.data) != len(other.data):
            raise ValueError("Vectors have different lengths")
        for a, b in zip(self.data, other.data):
            if a < b:
                return -1
            elif a > b:
                return 1
        return 0

    def merge(self, other):
        if len(self.data) != len(other.data):
            raise ValueError("Vectors have different lengths")
        result = [max(a, b) for a, b in zip(self.data, other.data)]
        return Counter(result)

# State-based PN Counter
class PNCounter:
    def __init__(self, size):
        self.p = [0] * size
        self.n = [0] * size

    @classmethod
    def initialize(cls, n, p, q):
        return cls(p, q)

    def increment(self):
        g = my_id()
        self.p[g] += 1

    def decrement(self):
        g = my_id()
        self.n[g] += 1

    def value(self):
        return sum(self.p) - sum(self.n)

    @staticmethod
    def compare(x, y):
        for i in range(len(x.p)):
            if x.p[i] > y.p[i] or x.n[i] > y.n[i]:
                return False
        return True

    @staticmethod
    def merge(x, y):
        z = PNCounter(len(x.p))
        for i in range(len(x.p)):
            z.p[i] = max(x.p[i], y.p[i])
            z.n[i] = max(x.n[i], y.n[i])
        return z

# State-based last-writer-wins register
class LastWriterWinsRegister:
    def __init__(self, initial_value):
        self.value = initial_value
        self.timestamp = 0.0
        self.lock = Lock()

    def read(self):
        with self.lock:
            return self.value

    def write(self, new_value, new_timestamp):
        with self.lock:
            if new_timestamp > self.timestamp:
                self.value = new_value
                self.timestamp = new_timestamp

    def compare_and_swap(self, expected_value, expected_timestamp, new_value, new_timestamp):
        with self.lock:
            if self.value == expected_value and self.timestamp == expected_timestamp:
                self.value = new_value
                self.timestamp = new_timestamp
                return True
            return False


# Operation-based last-writer-wins register
class OpBasedLWWValue:
    def __init__(self, val, ts):
        self.val = val
        self.ts = ts

class OpBasedLWWRegister:
    def __init__(self, initial_value):
        self.value = OpBasedLWWValue(initial_value, 0.0)
        self.pending = []
        self.lock = Lock()

    def read(self):
        with self.lock:
            return self.value.val

    def update(self, new_value, new_timestamp):
        with self.lock:
            if new_timestamp > self.value.ts:
                self.value = OpBasedLWWValue(new_value, new_timestamp)
                self.pending.clear()
            else:
                self.pending.append(("update", OpBasedLWWValue(new_value, new_timestamp)))

    def reset(self):
        with self.lock:
            self.pending.append(("reset", None))

    def apply_pending(self):
        with self.lock:
            for op, arg in self.pending:
                if op == "update":
                    if arg.ts > self.value.ts:
                        self.value = arg
                elif op == "reset":
                    self.value = OpBasedLWWValue(0, 0.0)
            self.pending.clear()

    def downstream(self):
        with self.lock:
            self.apply_pending()


# State-based multi-value register
class MVRegisterValue:
    def __init__(self, x, v):
        self.x = x
        self.v = v

class MVRegister:
    def __init__(self):
        self.payload = [MVRegisterValue(-1, [])]
        self.lock = Lock()

    def query_increment_vv(self, process_id):
        with self.lock:
            max_version = max(max(v, default=0) for _, v in self.payload) + 1
            new_version = [max_version] * len(self.payload)
            new_version[process_id] += 1
            return new_version

    def update_assign(self, set_r, process_id):
        with self.lock:
            new_version = self.query_increment_vv(process_id)
            self.payload.extend(
                MVRegisterValue(x, new_version.copy()) for x in set_r
            )

    def query_value(self):
        with self.lock:
            return self.payload

    def compare(self, other):
        with self.lock:
            return any(
                any(
                    all(v_entry > v_entry_ for v_entry_ in v_)
                    for v in other.payload
                    if x_ == x
                )
                for x, v in self.payload
            )

    def merge(self, other):
        with self.lock:
            merged = set()
            for x, v in self.payload:
                if any(
                    max(w, default=0) >= v[-1]
                    or any(v_entry > w_entry for w_entry in w)
                    for _, w in other.payload
                    if x_ == x
                ):
                    merged.add((x, tuple(v)))
            for y, w in other.payload:
                if any(
                    max(v, default=0) >= w[-1]
                    or any(w_entry < v_entry for v_entry in v)
                    for _, v in self.payload
                    if x == y
                ):
                    merged.add((y, tuple(w)))
            return [MVRegisterValue(x, list(v)) for x, v in merged]


# State-based grow-only set
T = TypeVar('T')
class GSet(Generic[T]):
  def __init__(self):
    self.data = set()

  def add(self, element: T) -> None:
    self.data.add(element)

  def lookup(self, element: T) -> bool:
    return element in self.data

  def compare(self, other: GSet[T]) -> bool:
    return self.data == other.data

  def merge(self, other: GSet[T]) -> GSet[T]:
    merged_set = GSet()
    merged_set.data.update(self.data)
    merged_set.data.update(other.data)
    return merged_set


# State-based 2P set
class StateBased2PSet:
    def __init__(self):
        self.A = set()
        self.R = set()

    def lookup(self, e):
        return e in self.A and e not in self.R

    def add(self, e):
        self.A.add(e)

    def remove(self, e):
        if self.lookup(e):
            self.R.add(e)

    def compare(self, other):
        return self.A.issubset(other.A) and self.R.issubset(other.R)

    def merge(self, other):
        U = StateBased2PSet()
        U.A = self.A.union(other.A)
        U.R = self.R.union(other.R)
        return U


# Op based 2P set with unique elements
class StateBased2PSet:
    def __init__(self):
        self.A = set()
        self.R = set()

    def lookup(self, e):
        return e in self.A and e not in self.R

    def add(self, e):
        self.A.add(e)

    def remove(self, e):
        if self.lookup(e):
            self.R.add(e)

    def compare(self, other):
        return self.A.issubset(other.A) and self.R.issubset(other.R)

    def merge(self, other):
        U = StateBased2PSet()
        U.A = self.A.union(other.A)
        U.R = self.R.union(other.R)
        return U


# Molli, Weiss, Skaf set
class MWSSet(Generic[T]):
    def __init__(self):
        self.data = {}

    def lookup(self, e: T) -> bool:
        return e in self.data and self.data[e] > 0

    def add(self, e: T):
        if e in self.data and self.data[e] < 0:
            self.data[e] = -self.data[e] + 1
        else:
            self.data[e] = 1

    def remove(self, e: T):
        if self.lookup(e):
            self.data[e] -= 1
            if self.data[e] == 0:
                del self.data[e]


# Operation based observed-remove set
class ORSet:
    def __init__(self):
        self.added = set()
        self.removed = set()

    def lookup(self, e):
        return e in self.added and e not in self.removed

    def add(self, e):
        if not self.lookup(e):
            self.added.add(e)

    def remove(self, e):
        if self.lookup(e):
            self.removed.add(e)


# Operation based 2P2P graph
Vertex = int
Edge = Tuple[Vertex, Vertex]
Graph = Tuple[Set[Vertex], Set[Vertex], Set[Edge], Set[Edge]]

def initial_graph() -> Graph:
    return set(), set(), set(), set()