package yfasttrie

type node struct {
  val   interface{}
  left  *node
  right *node
}

type Trie struct {
  root *node
}

func New() *Trie {
  return &Trie{nil}
}

func (t *Trie) Add(x interface{}, xs []interface{}) {
  t.root = add(t.root, x, xs)
}

func add(n *node, x interface{}, xs []interface{}) *node {
  if n == nil {
    if len(xs) == 0 {
      return &node{x, nil, nil}
    }
    y := xs[0]
    n = &node{y, nil, nil}
    n.left = add(n.left, x, xs[1:])
    return n
  }
  if x == n.val {
    return n
  }
  if x < n.val {
    n.left = add(n.left, x, xs)
  } else {
    n.right = add(n.right, x, xs[1:])
  }
  return n
}

func (t *Trie) Lookup(xs []interface{}) (interface{}, bool) {
  return lookup(t.root, xs)
}

func lookup(n *node, xs []interface{}) (interface{}, bool) {
  if n == nil {
    return nil, false
  }
  if len(xs) == 0 {
    return n.val, true
  }
  y := xs[0]
  if y < n.val {
    return lookup(n.left, xs)
  }
  return lookup(n.right, xs[1:])
}