package main

import "fmt"

type auxTree struct {
	left  *auxTree
	key   int
	right *auxTree
}

func newAuxTree(key int) *auxTree {
	return &auxTree{key: key}
}

func (t *auxTree) insert(key int) {
	if key < t.key {
		if t.left == nil {
			t.left = newAuxTree(key)
		} else {
			t.left.insert(key)
		}
	} else {
		if t.right == nil {
			t.right = newAuxTree(key)
		} else {
			t.right.insert(key)
		}
	}
}

func (t *auxTree) find(key int) bool {
	if key == t.key {
		return true
	} else if key < t.key {
		if t.left == nil {
			return false
		}
		return t.left.find(key)
	} else {
		if t.right == nil {
			return false
		}
		return t.right.find(key)
	}
}

type tangoTree []*auxTree

func newTangoTree() tangoTree {
	return tangoTree{newAuxTree(0)}
}

func (t tangoTree) find(key int) bool {
	for _, aux := range t {
		if aux.find(key) {
			return true
		}
	}
	return false
}

func (t *tangoTree) update(key int) {
	leftAux, rightAux := t.split(key)
	*t = append(leftAux, append([]*auxTree{newAuxTree(key)}, rightAux...)...)
}

func (t tangoTree) split(key int) (left, right tangoTree) {
	leftAux, rightAux := new(auxTree), new(auxTree)
	for _, aux := range t {
		if aux.key < key {
			leftAux.insert(aux.key)
		} else {
			rightAux.insert(aux.key)
		}
	}
	left = tangoTree{leftAux}
	right = tangoTree{rightAux}
	return
}