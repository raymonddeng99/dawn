package priorityqueue

type priorityQueue struct {
	size int
	root *cluster
}

type cluster struct {
	value interface{}
	children []*cluster
}

const clusterSize = 2

func newCluster(value interface{}) *cluster {
	return &cluster{
		value: value,
		children: make([]*cluster, clusterSize),
	}
}

func newPriorityQueue() *priorityQueue {
	return &priorityQueue{
		size: 0,
		root: nil,
	}
}

func (pq *priorityQueue) isEmpty() bool {
	return pq.size == 0
}

func (pq *priorityQueue) insert(value interface{}) {
	if pq.root == nil {
		pq.root = newCluster(value)
	} else {
		pq.root = pq.root.insert(value)
	}
	pq.size++
}

func (c *cluster) insert(value interface{}) *cluster {
	if c.value == nil {
		return newCluster(value)
	}
	for i, child := range c.children {
		if child == nil {
			c.children[i] = newCluster(value)
			return c
		}
		newChild := child.insert(value)
		if newChild != nil {
			c.children[i] = newChild
			return c
		}
	}
	children := make([]*cluster, clusterSize)
	for i := range children {
		if i < len(c.children) {
			children[i] = c.children[i]
		} else {
			children[i] = newCluster(value)
		}
	}
	return &cluster{value: nil, children: children}
}

func (pq *priorityQueue) findMin() interface{} {
	if pq.root == nil {
		return nil
	}
	return pq.root.findMin()
}

func (c *cluster) findMin() interface{} {
	if c.value != nil {
		return c.value
	}
	for _, child := range c.children {
		if child != nil {
			return child.findMin()
		}
	}
	return nil
}

func (pq *priorityQueue) removeMin() interface{} {
	if pq.root == nil {
		return nil
	}
	min, newRoot := pq.root.removeMin()
	pq.root = newRoot
	pq.size--
	return min
}

func (c *cluster) removeMin() (interface{}, *cluster) {
	if c.value != nil {
		return c.value, nil
	}
	for i, child := range c.children {
		if child != nil {
			min, newChild := child.removeMin()
			if newChild == nil {
				c.children[i] = nil
			} else {
				c.children[i] = newChild
			}
			if c.isClusterEmpty() {
				return min, nil
			}
			return min, c
		}
	}
	return nil, nil
}

func (c *cluster) isClusterEmpty() bool {
	if c.value != nil {
		return false
	}
	for _, child := range c.children {
		if child != nil {
			return false
		}
	}
	return true
}