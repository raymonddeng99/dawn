package rmq

import "math"

type Tree struct {
	values []int
	euler  []int
	first  []int
	rmq    [][]int
}

func log2(n int) int {
	return int(math.Ceil(math.Log2(float64(n))))
}

func precomputeRMQ(tree *Tree) {
	n := len(tree.euler)
	k := log2(n) + 1
	tree.rmq = make([][]int, k)
	for i := range tree.rmq {
		tree.rmq[i] = make([]int, n)
	}

	for i := 0; i < n; i++ {
		tree.rmq[0][i] = i
	}

	for j := 1; j < k; j++ {
		for i := 0; i+int(math.Pow(2, float64(j))) <= n; i++ {
			x := tree.rmq[j-1][i]
			y := tree.rmq[j-1][i+int(math.Pow(2, float64(j-1)))]
			if tree.euler[x] < tree.euler[y] {
				tree.rmq[j][i] = x
			} else {
				tree.rmq[j][i] = y
			}
		}
	}
}

func query(tree *Tree, l, r int) int {
	l, r = min(l, r), max(l, r)
	k := log2(r - l + 1)
	x := tree.rmq[k][tree.first[l]]
	y := tree.rmq[k][tree.first[r]-(1<<uint(k))+1]
	if tree.euler[x] < tree.euler[y] {
		return tree.values[tree.euler[x]]
	}
	return tree.values[tree.euler[y]]
}

func FromTree(values []int) *Tree {
	n := len(values)
	euler := make([]int, 2*n)
	first := make([]int, 2*n)

	var buildEuler func(int, int)
	buildEuler = func(tree, i int) {
		j := i + 1
		euler[i] = tree
		first[i] = j
		if j < 2*n {
			buildEuler(tree+1, j)
		}
	}
	buildEuler(0, 0)

	tree := &Tree{values, euler, first, nil}
	precomputeRMQ(tree)
	return tree
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}