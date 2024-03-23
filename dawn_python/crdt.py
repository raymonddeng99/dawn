# CRDTs

'''
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
'''

from enum import Enum
from typing import List, Tuple

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