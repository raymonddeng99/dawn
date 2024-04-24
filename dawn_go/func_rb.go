package main

import "fmt"

type Color bool

const (
	Red   Color = true
	Black Color = false
)

type RBTree struct {
	color Color
	left  *RBTree
	data  int
	right *RBTree
}

func balance(color Color, a, b *RBTree, x int) *RBTree {
	if color == Black {
		if isRed(a) && isRed(a.left) {
			return &RBTree{Red, a.left.copy(), x, &RBTree{Black, a.right.copy(), a.data, b.copy()}}
		}
		if isRed(a) && isRed(a.right) {
			return &RBTree{Red, &RBTree{Black, a.left.copy(), a.data, a.right.left.copy()}, x, &RBTree{Black, a.right.right.copy(), b.data, b.right.copy()}}
		}
		if isRed(b) && isRed(b.right) {
			return &RBTree{Red, &RBTree{Black, a.copy(), x, b.left.copy()}, b.data, b.right.copy()}
		}
		if isRed(b) && isRed(b.left) {
			return &RBTree{Red, &RBTree{Black, a.copy(), x, b.left.left.copy()}, b.left.data, &RBTree{Black, b.left.right.copy(), b.data, b.right.copy()}}
		}
	}
	return &RBTree{color, a.copy(), x, b.copy()
}

func isRed(t *RBTree) bool {
	if t == nil {
		return false
	}
	return t.color == Red
}

func (t *RBTree) copy() *RBTree {
	if t == nil {
		return nil
	}
	return &RBTree{t.color, t.left.copy(), t.data, t.right.copy()}
}

func insert(t *RBTree, x int) *RBTree {
	if t == nil {
		return &RBTree{Red, nil, x, nil}
	}
	if x < t.data {
		t.left = insert(t.left, x)
	} else if x > t.data {
		t.right = insert(t.right, x)
	} else {
		return t
	}
	return balance(t.color, t.left, t.right, t.data)
}