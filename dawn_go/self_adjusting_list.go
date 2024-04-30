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


// Order by Next Request strategy
type ONBRList[T any] struct {
	head *Node[T]
	tail *Node[T]
}

func (l *ONBRList[T]) Insert(value T) {
	newNode := &Node[T]{value: value, next: l.head, prev: nil}
	if l.head != nil {
		l.head.prev = newNode
	} else {
		l.tail = newNode
	}
	l.head = newNode
}

func (l *ONBRList[T]) RemoveHead() (T, bool) {
	if l.head == nil {
		var zero T
		return zero, false
	}
	value := l.head.value
	l.head = l.head.next
	if l.head != nil {
		l.head.prev = nil
	} else {
		l.tail = nil
	}
	return value, true
}

func (l *ONBRList[T]) Access(value T) {
	node := l.find(value)
	if node != nil {
		l.moveToFront(node)
	}
}

func (l *ONBRList[T]) find(value T) *Node[T] {
	curr := l.head
	for curr != nil {
		if curr.value == value {
			return curr
		}
		curr = curr.next
	}
	return nil
}

func (l *ONBRList[T]) moveToFront(node *Node[T]) {
	if node == l.head {
		return
	}
	if node.prev != nil {
		node.prev.next = node.next
	} else {
		l.tail = node.next
	}
	if node.next != nil {
		node.next.prev = node.prev
	}
	node.prev = nil
	node.next = l.head
	l.head.prev = node
	l.head = node
}