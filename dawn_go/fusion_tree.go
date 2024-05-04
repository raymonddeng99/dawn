type FusionTree struct {
    value int
    left  *FusionTree
    right *FusionTree
}

func NewEmpty() *FusionTree {
    return &FusionTree{value: 0}
}

func (t *FusionTree) IsEmpty() bool {
    return t.value == 0 && t.left == nil && t.right == nil
}

func (t *FusionTree) Insert(x int) *FusionTree {
    if t.IsEmpty() {
        return &FusionTree{value: x}
    }
    left := t.left.Insert(x)
    right := t.right
    return &FusionTree{value: t.value, left: left, right: right}
}

func (t *FusionTree) Find(x int) int {
    if t.IsEmpty() {
        return 0
    }
    if t.value == x {
        return t.value + t.left.Find(x) + t.right.Find(x)
    }
    return t.left.Find(x) + t.right.Find(x)
}

func Union(left, right *FusionTree) *FusionTree {
    if left.IsEmpty() {
        return right
    }
    if right.IsEmpty() {
        return left
    }
    return &FusionTree{
        value: left.value + right.value,
        left:  Union(left.left, right.left),
        right: Union(left.right, right.right),
    }
}