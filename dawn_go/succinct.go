package compactsuffix

import "fmt"

func rankSuffix(text string, rankOfChar func(rune) int, n, i int) int {
	r := 0
	for j := i; j < n; j++ {
		r = r*len(rankOfChar) + rankOfChar(rune(text[j]))
	}
	return r
}

func computeRankOfChar(text string) func(rune) int {
	rankOfChar := make([]int, 256)
	used := make([]bool, 256)
	rank := 0
	for _, c := range text {
		if !used[c] {
			rankOfChar[c] = rank
			rank++
			used[c] = true
		}
	}
	return func(r rune) int { return rankOfChar[r] }
}

func CompactSuffixArray(text string) []int {
	n := len(text)
	rankOfChar := computeRankOfChar(text)
	ranks := make([]int, n)
	for i := 0; i < n; i++ {
		ranks[i] = rankSuffix(text, rankOfChar, n, i)
	}
	indices := make([]int, n)
	for i := range indices {
		indices[i] = i
	}
	sortByRanks(ranks, indices)
	return indices
}

func sortByRanks(ranks, indices []int) {
	sort.Slice(indices, func(i, j int) bool {
		return ranks[indices[i]] < ranks[indices[j]]
	})
}



// Succinct static data structures, Jacobsen '89
func rankNaive(bits []bool, i int) int {
	cnt := 0
	for j := 0; j <= i; j++ {
		if bits[j] {
			cnt++
		}
	}
	return cnt
}

func selectNaive(bits []bool, i int) int {
	cnt, j := 0, 0
	for ; cnt < i; j++ {
		if j >= len(bits) {
			return -1
		}
		if bits[j] {
			cnt++
		}
	}
	if cnt == i {
		return j - 1
	}
	return -1
}

type BitVector struct {
	bits        []bool
	rankTable   []int
	selectTable []int
	k int
}

func createBitVector(bits []bool) *BitVector {
	n := len(bits)
	rankTable := make([]int, n+1)
	selectTable := make([]int, n+1)

	for i := 0; i <= n; i++ {
		if i%(1<<k) == 0 {
			rankTable[i] = 0
			selectTable[i] = -1
		} else {
			rankTable[i] = rankTable[i-(i%(1<<k))] + rankNaive(bits, i) - rankNaive(bits, i-(i%(1<<k)))
			selectTable[i] = selectTable[i-(i%(1<<k))] + selectNaive(bits, i) - selectNaive(bits, i-(i%(1<<k)))
		}
	}

	return &BitVector{bits, rankTable, selectTable}
}

func (bv *BitVector) rank(i int) int {
	return bv.rankTable[i]
}

func (bv *BitVector) select(i int) int {
	l, r := 0, len(bv.bits)
	for l <= r {
		m := l + (r-l)/2
		if bv.rank(m) < i {
			l = m + 1
		} else {
			r = m - 1
		}
	}
	return l
}