package xfasttrie

type XFastTrie struct {
	bits  []bool
	ones  []int
	size  int
}

func Create(w int) *XFastTrie {
	u := 1 << uint(w)
	bits := make([]bool, 2*u-1)
	ones := make([]int, 0)
	return &XFastTrie{bits, ones, 0}
}

func (trie *XFastTrie) Insert(x int) {
	trie.insert(x, 0)
}

func (trie *XFastTrie) insert(x int, i int) {
	if i >= len(trie.bits) {
		trie.size++
		return
	}
	bit := x&(1<<uint(i)) != 0
	index := 1 << uint(len(trie.bits)-1-i)
	trie.bits[index-1] = trie.bits[index-1] || bit
	if bit {
		trie.ones = append(trie.ones, index)
		trie.insert(x, i+1)
	} else {
		trie.insert(x, i+1)
	}
}

func (trie *XFastTrie) Predecessor(x int) int {
	return trie.predecessor(x, 0)
}

func (trie *XFastTrie) predecessor(x int, i int) int {
	if i >= len(trie.bits) {
		return -1
	}
	index := 1 << uint(len(trie.bits)-1-i)
	if trie.bits[index-1] {
		leftChild := 2 * index
		rightChild := leftChild + 1
		if trie.bits[rightChild-1] {
			return trie.predecessor(x, i+1)
		} else {
			return trie.ones[0]
		}
	} else {
		return trie.predecessor(x, i+1)
	}
}