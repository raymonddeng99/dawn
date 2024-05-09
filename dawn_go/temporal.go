package deque

import (
	"container/heap"
)

type node struct {
	values [4]interface{}
	length int
}

type Deque struct {
	front, back []node
}

func New() *Deque {
	return &Deque{front: nil, back: nil}
}

func (d *Deque) IsEmpty() bool {
	return len(d.front) == 0 && len(d.back) == 0
}

func (d *Deque) AddFront(x interface{}) {
	d.front = addFront(d.front, x)
}

func (d *Deque) AddBack(x interface{}) {
	d.back = addBack(d.back, x)
}

func (d *Deque) TakeFront() (interface{}, bool) {
	if len(d.front) == 0 {
		return nil, false
	}
	x, newFront := takeFront(d.front)
	d.front = newFront
	return x, true
}

func (d *Deque) TakeBack() (interface{}, bool) {
	if len(d.back) == 0 {
		return nil, false
	}
	x, newBack := takeBack(d.back)
	d.back = newBack
	return x, true
}

func addFront(nodes []node, x interface{}) []node {
	var newNodes []node
	for len(nodes) > 0 {
		n := nodes[0]
		nodes = nodes[1:]
		switch n.length {
		case 4:
			newNodes = append(newNodes, node{[4]interface{}{n.values[1], n.values[2], n.values[3], x}, 4})
			newNodes = append(newNodes, node{[4]interface{}{n.values[0], n.values[0], n.values[0], n.values[0]}, 1})
		case 3:
			newNodes = append(newNodes, node{[4]interface{}{n.values[1], n.values[2], x, x}, 2})
			newNodes = append(newNodes, node{[4]interface{}{n.values[0], n.values[0], n.values[0], n.values[0}, 1})
		case 2:
			newNodes = append(newNodes, node{[4]interface{}{n.values[1], x, x, x}, 3})
			newNodes = append(newNodes, node{[4]interface{}{n.values[0], n.values[0], n.values[0], n.values[0]}, 1})
		case 1:
			newNodes = append(newNodes, node{[4]interface{}{x, x, x, x}, 4})
		}
	}
	newNodes = append(newNodes, node{[4]interface{}{x, x, x, x}, 4})
	return newNodes
}

func addBack(nodes []node, x interface{}) []node {
	var newNodes []node
	for len(nodes) > 0 {
		n := nodes[0]
		nodes = nodes[1:]
		switch n.length {
		case 4:
			newNodes = append(newNodes, node{[4]interface{}{n.values[0], n.values[0], n.values[0], n.values[0]}, 1})
			newNodes = append(newNodes, node{[4]interface{}{x, n.values[1], n.values[2], n.values[3]}, 4})
		case 3:
			newNodes = append(newNodes, node{[4]interface{}{n.values[0], n.values[0], n.values[0], n.values[0]}, 1})
			newNodes = append(newNodes, node{[4]interface{}{x, x, n.values[1], n.values[2]}, 3})
		case 2:
			newNodes = append(newNodes, node{[4]interface{}{n.values[0], n.values[0], n.values[0], x}, 2})
			newNodes = append(newNodes, node{[4]interface{}{x, n.values[1], n.values[1], n.values[1]}, 3})
		case 1:
			newNodes = append(newNodes, node{[4]interface{}{x, x, x, x}, 4})
		}
	}
	newNodes = append(newNodes, node{[4]interface{}{x, x, x, x}, 4})
	return newNodes
}

func takeFront(nodes []node) (interface{}, []node) {
	if len(nodes) == 0 {
		return nil, nil
	}
	x := nodes[0].values[0]
	switch nodes[0].length {
	case 4:
		return x, append(nodes[1:], node{[4]interface{}{nodes[0].values[1], nodes[0].values[2], nodes[0].values[3], nodes[0].values[3]}, 3})
	case 3:
		return x, append(nodes[1:], node{[4]interface{}{nodes[0].values[1], nodes[0].values[2], nodes[0].values[2], nodes[0].values[2]}, 2})
	case 2:
		return x, append(nodes[1:], node{[4]interface{}{nodes[0].values[1], nodes[0].values[1], nodes[0].values[1], nodes[0].values[1]}, 1})
	case 1:
		return x, nodes[1:]
	}
	panic("invalid node length")
}

func takeBack(nodes []node) (interface{}, []node) {
	if len(nodes) == 0 {
		return nil, nil
	}
	n := len(nodes)
	x := nodes[n-1].values[nodes[n-1].length-1]
	var newNodes []node
	if n > 1 {
		newNodes = nodes[:n-1]
	}
	switch nodes[n-1].length {
	case 4:
		newNodes = append(newNodes, node{[4]interface{}{nodes[n-1].values[0], nodes[n-1].values[1], nodes[n-1].values[2], nodes[n-1].values[2]}, 3})
	case 3:
		newNodes = append(newNodes, node{[4]interface{}{nodes[n-1].values[0], nodes[n-1].values[1], nodes[n-1].values[1], nodes[n-1].values[1]}, 2})
	case 2:
		newNodes = append(newNodes, node{[4]interface{}{nodes[n-1].values[0], nodes[n-1].values[0], nodes[n-1].values[0], nodes[n-1].values[0]}, 1})
	}
	return x, newNodes
}


// Partially retroactive priority queue
type Item struct {
	value    interface{}
	priority int
	index    int
}

type PQueue struct {
	data   []Item
	time   int
	retroData []Item
}

func NewPQueue() *PQueue {
	pq := &PQueue{
		data:   make([]Item, 0),
		retroData: make([]Item, 0),
		time:   0,
	}
	heap.Init(&pq.data)
	return pq
}

func (pq *PQueue) Len() int { return len(pq.data) }

func (pq *PQueue) Less(i, j int) bool {
	return pq.data[i].priority < pq.data[j].priority
}

func (pq *PQueue) Swap(i, j int) {
	pq.data[i], pq.data[j] = pq.data[j], pq.data[i]
	pq.data[i].index = i
	pq.data[j].index = j
}

func (pq *PQueue) Push(x interface{}) {
	pq.time++
	n := len(pq.data)
	item := Item{x, pq.time, n}
	pq.data = append(pq.data, item)
}

func (pq *PQueue) Pop() interface{} {
	n := len(pq.data) - 1
	item := pq.data[0]
	pq.data[0] = pq.data[n]
	pq.data[0].index = 0
	pq.data = pq.data[:n]
	heap.Fix(&pq.data, 0)
	return item.value
}

func (pq *PQueue) RetroactiveUpdate(t int, x interface{}) {
	for i := range pq.data {
		if pq.data[i].priority <= t {
			pq.retroData = append(pq.retroData, Item{x, pq.data[i].priority, i})
		}
	}
	for _, item := range pq.retroData {
		pq.data[item.index].value = item.value
	}
	pq.retroData = pq.retroData[:0]
	heap.Init(&pq.data)
}