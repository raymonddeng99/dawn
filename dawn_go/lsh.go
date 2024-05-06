package main

import (
	"fmt"
)

func hash(str string) int {
	sum := 0
	for _, c := range str {
		sum += int(c)
	}
	return sum
}

func hammingDistance(str1, str2 string) int {
	distance := 0
	len := min(len(str1), len(str2))
	for i := 0; i < len; i++ {
		if str1[i] != str2[i] {
			distance++
		}
	}
	return distance
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func lshFunc(k, l int, str string) []int {
	hashes := make([]int, k)
	strLen := len(str)
	for i := 0; i < k; i++ {
		start := i * strLen / k
		end := start + l*strLen/k
		hashVal := hash(str[start:end])
		hashes[i] = hashVal
	}
	return hashes
}

func buildLSHTable(k, l int, strings []string) map[int][]string {
	table := make(map[int][]string)
	for _, str := range strings {
		hashes := lshFunc(k, l, str)
		for _, hashVal := range hashes {
			table[hashVal] = append(table[hashVal], str)
		}
	}
	return table
}

func queryLSHTable(k, l int, table map[int][]string, queryStr string) []string {
	hashes := lshFunc(k, l, queryStr)
	candidates := make([]string, 0)
	for _, hashVal := range hashes {
		bucket := table[hashVal]
		for _, str := range bucket {
			if hammingDistance(str, queryStr) <= l {
				candidates = append(candidates, str)
			}
		}
	}
	return candidates
}