package linkcuttree

type node struct {
	value  int
	parent *node
	left   *node
	right  *node
	rev    bool
	sum    int
}

func makeNode(value int) *node {
	return &node{
		value: value,
		sum:   value,
	}
}

func getSum(n *node) int {
	if n == nil {
		return 0
	}
	return n.sum
}

func updateSum(n *node) {
	if n == nil {
		return
	}
	n.sum = n.value + getSum(n.left) + getSum(n.right)
}

func push(n *node) {
	if n == nil || !n.rev {
		return
	}
	n.rev = false
	n.left, n.right = n.right, n.left
	if n.left != nil {
		n.left.rev = !n.left.rev
	}
	if n.right != nil {
		n.right.rev = !n.right.rev
	}
}

func makeRoot(n *node) {
	if n == nil {
		return
	}
	push(n)
	if n.parent != nil {
		n.parent.left, n.parent.right = nil, nil
		makeRoot(n.parent)
		n.parent = nil
	}
}

func splay(n *node) {
	makeRoot(n)
	for p := n.parent; p != nil; p = n.parent {
		makeRoot(p)
		if p.parent != nil {
			makeRoot(p.parent)
			if (p.parent.left == p) == (p.left == n) {
				rotate(p)
			} else {
				rotate(n)
			}
		}
		rotate(n)
	}
	updateSum(n)
}

func rotate(n *node) {
	p := n.parent
	if p.parent == nil {
		return
	}
	g := p.parent
	if g.left == p {
		g.left = n
	} else {
		g.right = n
	}
	n.parent = g
	if p.left == n {
		p.left = n.right
		n.right = p
	} else {
		p.right = n.left
		n.left = p
	}
	p.parent = n
	updateSum(p)
	updateSum(n)
}

func access(n *node) {
	splay(n)
	n.rev = false
	for c := n.left; c != nil; c = n.left {
		splay(c)
		c.rev = !c.rev
		updateSum(c)
		c.right = n
		n.parent = c
		updateSum(n) {
	c = n.left
	updateSum(n)
}

func link(x, y *node) {
	access(x)
	access(y)
	y.parent = x
	updateSum(y)
}

func cut(x *node) {
	access(x)
	if x.left != nil {
		x.left.parent = nil
		x.left = nil
		updateSum(x)
	}
}

func root(x *node) bool {
	return x.parent == nil
}

func lca(x, y *node) *node {
	access(y)
	ySum := getSum(y)
	access(x)
	z := y
	for getSum(z) < ySum || getSum(z) < getSum(x) {
		access(z.parent)
		z = z.parent
	}
	return z
}

func pathSum(x, y *node) int {
	z := lca(x, y)
	access(x)
	access(y)
	xSum := getSum(x)
	ySum := getSum(y)
	zSum := getSum(z)
	return xSum + ySum - 2*zSum
}