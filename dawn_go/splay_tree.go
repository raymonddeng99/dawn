package splaytree

type SplayTree[T any] struct {
    root *node[T]
}

type node[T any] struct {
    left  *node[T]
    right *node[T]
    val   T
}

func New[T any]() *SplayTree[T] {
    return &SplayTree[T]{nil}
}

func (t *SplayTree[T]) Insert(val T) {
    t.root = t.splay(t.insert(t.root, val))
}

func (t *SplayTree[T]) insert(n *node[T], val T) *node[T] {
    if n == nil {
        return &node[T]{nil, nil, val}
    }
    if val < n.val {
        n.left = t.insert(n.left, val)
    } else if val > n.val {
        n.right = t.insert(n.right, val)
    }
    return n
}

func (t *SplayTree[T]) splay(n *node[T]) *node[T] {
    if n == nil {
        return nil
    }
    n.left = t.splay(n.left)
    n.right = t.splay(n.right)
    if n.left != nil && n.left.val > n.val {
        n = t.rotateRight(n)
    }
    if n.right != nil && n.right.val < n.val {
        n = t.rotateLeft(n)
    }
    return n
}

func (t *SplayTree[T]) rotateRight(n *node[T]) *node[T] {
    x := n.left
    n.left = x.right
    x.right = n
    return x
}

func (t *SplayTree[T]) rotateLeft(n *node[T]) *node[T] {
    x := n.right
    n.right = x.left
    x.left = n
    return x
}

func (t *SplayTree[T]) Search(val T) bool {
    n := t.splay(t.root)
    if n == nil {
        return false
    }
    return n.val == val
}

func (t *SplayTree[T]) Delete(val T) {
    t.root = t.delete(t.root, val)
}

func (t *SplayTree[T]) delete(n *node[T], val T) *node[T] {
    if n == nil {
        return nil
    }
    n = t.splay(n)
    if n.val == val {
        if n.left == nil {
            return n.right
        }
        x := n.left
        for x.right != nil {
            x = x.right
        }
        x.right = n.right
        return x
    } else if val < n.val {
        n.left = t.delete(n.left, val)
    } else {
        n.right = t.delete(n.right, val)
    }
    return n
}