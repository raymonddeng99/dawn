// On RAM priority queues, Thorup '96

type VEBTree struct {
    value  int
    left   *VEBTree
    right  *VEBTree
    isLeaf bool
}

func NewVEBTree(value int) *VEBTree {
    return &VEBTree{value: value, isLeaf: true}
}

func (t *VEBTree) Min() int {
    if t.isLeaf {
        return t.value
    }
    minLeft, minRight := math.MaxInt64, math.MaxInt64
    if t.left != nil {
        minLeft = t.left.Min()
    }
    if t.right != nil {
        minRight = t.right.Min()
    }
    return min(minLeft, minRight)
}

func (t *VEBTree) Insert(value int) *VEBTree {
    if t.isLeaf {
        if value < t.value {
            t.right = NewVEBTree(t.value)
            t.value = value
        } else {
            t.right = NewVEBTree(value)
        }
        t.isLeaf = false
        return t
    }

    if value < t.left.Min() {
        t.left = t.left.Insert(value)
    } else {
        t.right = t.right.Insert(value)
    }
    return t
}

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}