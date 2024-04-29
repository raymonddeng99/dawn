package main

import "fmt"

// Sleator-Tarjan one-finger model
type Node struct {
	prev   *Node
	next   *Node
	data   interface{}
}

type SelfAdjustingList struct {
	head   *Node
	tail   *Node
	finger *Node
}

func CreateList() *SelfAdjustingList {
	return &SelfAdjustingList{}
}

func (list *SelfAdjustingList) InsertFront(data interface{}) {
	newNode := &Node{data: data}
	if list.head == nil {
		list.head = newNode
		list.tail = newNode
		list.finger = newNode
	} else {
		newNode.next = list.head
		list.head.prev = newNode
		list.head = newNode
		list.finger = newNode
	}
}

func (list *SelfAdjustingList) traverseFromFinger(target interface{}, node *Node) *Node {
	if node.data == target {
		return node
	}
	if node.next == nil {
		list.finger = list.tail
		return nil
	}
	found := list.traverseFromFinger(target, node.next)
	if found != nil {
		return found
	}
	list.finger = node
	return node
}

func (list *SelfAdjustingList) Find(target interface{}) *Node {
	if list.finger == nil {
		if list.head == nil {
			return nil
		}
		return list.traverseFromFinger(target, list.head)
	}
	found := list.traverseFromFinger(target, list.finger)
	if found == nil {
		if list.head == nil {
			return nil
		}
		return list.traverseFromFinger(target, list.head)
	}
	return found
}

// Constant finger model
type ConstList struct {
	head   *Node
	tail   *Node
	finger *Node
}

func CreateList() *ConstList {
	return &ConstList{}
}

func (list *ConstList) InsertFront(data interface{}) {
	newNode := &Node{data: data}
	if list.head == nil {
		list.head = newNode
		list.tail = newNode
		list.finger = newNode
	} else {
		newNode.next = list.head
		list.head.prev = newNode
		list.head = newNode
		list.finger = newNode
	}
}

func (list *ConstList) traverseFromFinger(target interface{}, node *Node) *Node {
	if node.data == target {
		return node
	}
	if node.next == nil {
		list.finger = list.tail
		return nil
	}
	found := list.traverseFromFinger(target, node.next)
	if found != nil {
		return found
	}
	list.finger = node
	return node
}

func (list *ConstList) Find(target interface{}) *Node {
	if list.finger == nil {
		if list.head == nil {
			return nil
		}
		return list.traverseFromFinger(target, list.head)
	}
	found := list.traverseFromFinger(target, list.finger)
	if found == nil {
		if list.head == nil {
			return nil
		}
		return list.traverseFromFinger(target, list.head)
	}
	return found
}