from enum import Enum
from typing import Optional

class Color(Enum):
    RED = 1
    BLACK = 2

class RBTree:
    def __init__(self, color: Color, left: Optional['RBTree'], data: int, right: Optional['RBTree']):
        self.color = color
        self.left = left
        self.data = data
        self.right = right

    def __repr__(self):
        return f"RBTree({self.color.name}, {self.left}, {self.data}, {self.right})"

def balance(color: Color, a: Optional[RBTree], x: int, b: Optional[RBTree]) -> RBTree:
    if color == Color.BLACK:
        if a and a.color == Color.RED and (a.left and a.left.color == Color.RED):
            return RBTree(
                Color.RED,
                a.left.left,
                x,
                RBTree(Color.BLACK, a.right, a.data, b)
            )
        if a and a.color == Color.RED and (a.right and a.right.color == Color.RED):
            return RBTree(
                Color.RED,
                RBTree(Color.BLACK, a.left, a.data, a.right.left),
                x,
                RBTree(Color.BLACK, a.right.right, b.data if b else None, b.right if b else None)
            )
        if b and b.color == Color.RED and (b.right and b.right.color == Color.RED):
            return RBTree(
                Color.RED,
                RBTree(Color.BLACK, a, x, b.left),
                b.data,
                b.right
            )
        if b and b.color == Color.RED and (b.left and b.left.color == Color.RED):
            return RBTree(
                Color.RED,
                RBTree(Color.BLACK, a, x, b.left.left),
                b.left.data,
                RBTree(Color.BLACK, b.left.right, b.data, b.right)
            )
    return RBTree(color, a, x, b)

def insert(t: Optional[RBTree], x: int) -> RBTree:
    if not t:
        return RBTree(Color.RED, None, x, None)
    if x < t.data:
        t.left = insert(t.left, x)
    elif x > t.data:
        t.right = insert(t.right, x)
    else:
        return t
    return balance(t.color, t.left, t.data, t.right)