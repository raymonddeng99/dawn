package compactsuffix

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