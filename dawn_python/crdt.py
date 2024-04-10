# CRDTs

'''
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
'''

from enum import Enum
from typing import List, Set, Tuple, Dict, Callable, TypeVar, Generic
from threading import Lock
import random
import hashlib

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

    def compare(x, y):
        for i in range(len(x.p)):
            if x.p[i] > y.p[i] or x.n[i] > y.n[i]:
                return False
        return True

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
class Graph2P:
    Vertex = int
    Edge = Tuple[Vertex, Vertex]
    Graph = Tuple[Set[Vertex], Set[Vertex], Set[Edge], Set[Edge]]

    def __init__(self):
        self.va: Set[self.Vertex] = set()
        self.vr: Set[self.Vertex] = set()
        self.ea: Set[self.Edge] = set()
        self.er: Set[self.Edge] = set()

    def initial_graph(self) -> Graph:
        return set(), set(), set(), set()

    def lookup_vertex(self, v: Vertex) -> bool:
        return v in self.va and v not in self.vr

    def lookup_edge(self, e: Edge) -> bool:
        u, v = e
        return u in self.va and v in self.va and e in self.ea.union(self.er)

    def add_vertex(self, w: Vertex) -> None:
        self.va.add(w)

    def add_edge(self, u: Vertex, v: Vertex) -> None:
        if u in self.va and v in self.va:
            self.ea.add((u, v))

    def remove_vertex(self, w: Vertex) -> None:
        if w in self.va and all(u != w and v != w for u, v in self.ea.union(self.er)):
            self.va.remove(w)
            self.vr.add(w)

    def remove_edge(self, u: Vertex, v: Vertex) -> None:
        if u in self.va and v in self.va:
            self.er.add((u, v))


# Op-based add only monotonic DAG
class MonotonicDAG:
    Vertex = int
    Edge = Tuple[Vertex, Vertex]
    Graph = Tuple[Set[Vertex], Set[Vertex], Set[Edge], Set[Edge]]

    def __init__(self):
        self.va: Set[self.Vertex] = set()
        self.vr: Set[self.Vertex] = set()
        self.ea: Set[self.Edge] = set()
        self.er: Set[self.Edge] = set()

    def initial_graph(self) -> Graph:
        return set(), set(), set(), set()

    def lookup_vertex(self, v: Vertex) -> bool:
        return v in self.va and v not in self.vr

    def lookup_edge(self, e: Edge) -> bool:
        u, v = e
        return u in self.va and v in self.va and e in self.ea.union(self.er)

    def add_vertex(self, w: Vertex) -> None:
        self.va.add(w)

    def add_edge(self, u: Vertex, v: Vertex) -> None:
        if u in self.va and v in self.va:
            self.ea.add((u, v))

    def remove_vertex(self, w: Vertex) -> None:
        if w in self.va and all(u != w and v != w for u, v in self.ea.union(self.er)):
            self.va.remove(w)
            self.vr.add(w)

    def remove_edge(self, u: Vertex, v: Vertex) -> None:
        if u in self.va and v in self.va:
            self.er.add((u, v))


# Add remove partial order
class AddRemovePartialOrder:
    def __init__(self):
        self.vertices = [-1, 1]
        self.removed = []
        self.edges = [(-1, 1)]

    def lookup(self, v):
        return v in self.vertices

    def before(self, u, v):
        for w in self.vertices:
            if (w == u and self.lookup(v)) or (w == v and self.lookup(u)):
                return True
            if self.lookup(w) and (w, u) in self.edges and (w, v) in self.edges:
                return True
        return False

    def add_between(self, u, v, w):
        if not self.lookup(w) or not self.before(u, w) or not self.before(w, v):
            raise ValueError("addBetween precondition violated")
        self.vertices.append(w)
        self.edges.append((u, w))
        self.edges.append((w, v))

    def remove(self, v):
        if not self.lookup(v) or v == -1 or v == 1:
            raise ValueError("remove precondition violated")
        self.removed.append(v)
        self.vertices = [x for x in self.vertices if x != v]
        self.edges = [(x, y) for x, y in self.edges if x != v and y != v]


# Replicable growth array
Vertex = Tuple[int, int]
Edge = Tuple[Vertex, Vertex]

class RGA:
    def __init__(self, now: Callable[[], int]):
        self.va: Set[Vertex] = {(-1, -1)}
        self.vr: Set[Vertex] = {(-1, 0)}
        self.edges: Dict[Vertex, Set[Vertex]] = {(-1, -1): {(-1, 0)}}
        self.now = now

    def lookup(self, v: Vertex) -> bool:
        return v in self.va and v not in self.vr

    def before(self, u: Vertex, v: Vertex) -> bool:
        if not (self.lookup(u) and self.lookup(v)):
            return False

        for w in self.va:
            if (w == u and self.lookup(v)) or \
               (w == v and self.lookup(u)) or \
               (self.lookup(w) and u in self.edges[w] and v in self.edges[w]):
                return True

        return False

    def successor(self, u: Vertex) -> Vertex:
        if not self.lookup(u):
            raise ValueError("Vertex not found")

        for v in self.va:
            if self.before(u, v) and not self.before(v, u):
                return v

        raise ValueError("No successor found")

    def decompose(self, u: Vertex) -> Vertex:
        return u

    def add_right(self, u: Vertex, a: int) -> None:
        t = self.now()
        w = (a, t)

        if self.lookup(w):
            raise ValueError("Timestamp conflict")

        self.va.add(w)
        self.edges.setdefault(u, set()).add(w)

    def remove(self, w: Vertex) -> None:
        if not self.lookup(w):
            raise ValueError("Vertex not found")

        self.vr.add(w)
        self.va.remove(w)

        for u, neighbors in self.edges.items():
            self.edges[u] = neighbors - {w}


# Continuous sequence
class ContSeq:
    class Node:
        def __init__(self, value, identifier):
            self.left_child = None
            self.right_child = None
            self.data = value
            self.id = identifier

        def cont_seq_insert(self, value, identifier):
            if identifier < self.id:
                if self.left_child is None:
                    self.left_child = ContSeq.Node(value, identifier)
                else:
                    self.left_child.cont_seq_insert(value, identifier)
            else:
                if self.right_child is None:
                    self.right_child = ContSeq.Node(value, identifier)
                else:
                    self.right_child.cont_seq_insert(value, identifier)

        def cont_seq_lookup(self, identifier):
            if identifier == self.id:
                return self
            elif identifier < self.id:
                if self.left_child is None:
                    return None
                else:
                    return self.left_child.cont_seq_lookup(identifier)
            else:
                if self.right_child is None:
                    return None
                else:
                    return self.right_child.cont_seq_lookup(identifier)

    def __init__(self):
        self.root = None

    def cont_seq_insert(self, value, identifier):
        if self.root is None:
            self.root = ContSeq.Node(value, identifier)
        else:
            self.root.cont_seq_insert(value, identifier)

    def cont_seq_lookup(self, identifier):
        if self.root is None:
            return None
        return self.root.cont_seq_lookup(identifier)

    def cont_seq_allocate_identifier_between(self, id1, id2):
        if len(id1) != 1 or len(id2) != 1:
            return None

        min_char = min(id1, id2)
        max_char = max(id1, id2)

        while True:
            random_char = random.choice(string.ascii_letters)
            if min_char < random_char < max_char:
                return random_char

    def cont_seq_add_between(self, value, id1, id2):
        if self.root is None:
            self.root = ContSeq.Node(value, self.cont_seq_allocate_identifier_between(id1, id2))
            return

        new_id = self.cont_seq_allocate_identifier_between(id1, self.root.id)
        if new_id is not None:
            self.root.left_child = self.cont_seq_add_between_recursive(self.root.left_child, value, id1, new_id)
            return

        new_id = self.cont_seq_allocate_identifier_between(self.root.id, id2)
        if new_id is not None:
            self.root.right_child = self.cont_seq_add_between_recursive(self.root.right_child, value, new_id, id2)
            return

    def cont_seq_add_between_recursive(self, current, value, id1, id2):
        if current is None:
            return ContSeq.Node(value, self.cont_seq_allocate_identifier_between(id1, id2))

        new_id = self.cont_seq_allocate_identifier_between(id1, current.id)
        if new_id is not None:
            current.left_child = self.cont_seq_add_between_recursive(current.left_child, value, id1, new_id)

        new_id = self.cont_seq_allocate_identifier_between(current.id, id2)
        if new_id is not None:
            current.right_child = self.cont_seq_add_between_recursive(current.right_child, value, new_id, id2)

        return current

    def cont_seq_remove(self, identifier):
        node = self.cont_seq_lookup(identifier)
        if node is None:
            raise ValueError("Element not found in set")

        self.cont_seq_remove_node(node)

        if self.cont_seq_lookup(identifier) is not None:
            raise RuntimeError("Removal failed")

    def cont_seq_remove_node(self, node):
        if node.left_child is None:
            node.data, node.id = node.right_child.data, node.right_child.id
            node.right_child = node.right_child.right_child
        elif node.right_child is None:
            node.data, node.id = node.left_child.data, node.left_child.id
            node.left_child = node.left_child.left_child
        else:
            successor = self.find_successor(node.right_child)
            node.data, node.id, successor.data, successor.id = successor.data, successor.id, node.data, node.id
            self.cont_seq_remove_node(successor)

    def find_successor(self, node):
        if node.left_child is None:
            return node
        return self.find_successor(node.left_child)


# Op-based observed-remove shopping cart
class ORCart:
    ISBN = str
    UniqueTag = str
    Payload = List[Tuple[ISBN, int, UniqueTag]]

    def generate_unique_tag() -> UniqueTag:
        random_bytes = bytes(random.getrandbits(8) for _ in range(8))
        return hashlib.sha1(random_bytes).hexdigest()

    def empty_payload() -> Payload:
        return []

    def get_quantity(payload: Payload, k: ISBN) -> int:
        return sum(n for isbn, n, _ in payload if isbn == k)

    def add(payload: Payload, k: ISBN, n: int) -> Payload:
        existing_quantity = ORCart.get_quantity(payload, k)
        new_unique_tag = ORCart.generate_unique_tag()
        new_payload = [(isbn, quantity, tag) for isbn, quantity, tag in payload if isbn != k]
        new_payload.append((k, n + existing_quantity, new_unique_tag))
        return new_payload

    def remove(payload: Payload, k: ISBN) -> Payload:
        return [(isbn, quantity, tag) for isbn, quantity, tag in payload if isbn != k]

    def upsert_precondition(payload: Payload, k: ISBN, n: int, alpha: UniqueTag, r: Payload) -> Payload:
        r_set = [(isbn, quantity, tag) for isbn, quantity, tag in r if isbn == k]
        union = payload + r_set + [(k, n, alpha)]
        union.sort(key=lambda x: x[:2])
        return list(dict.fromkeys(union))

    def remove_elements_observed_at_source(payload: Payload, r: Payload) -> Payload:
        r_set = set(r)
        return [item for item in payload if item not in r_set]