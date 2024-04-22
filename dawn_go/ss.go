package main

import (
	"sort"
	"strings"
)

func get_ngrams(n int, s string) []string {
	var ngrams []string
	for i := 0; i <= len(s)-n; i++ {
		ngrams = append(ngrams, s[i:i+n])
	}
	return ngrams
}

func count_3gram_frequencies(sample_keys []string) map[string]int {
	frequencies := make(map[string]int)
	for _, key := range sample_keys {
		ngrams := get_ngrams(3, key)
		for _, ngram := range ngrams {
			frequencies[ngram]++
		}
	}
	return frequencies
}

func select_3gram_intervals(sample_keys []string, dict_size int) []struct {
	start string
	end   *string
} {
	frequencies := count_3gram_frequencies(sample_keys)
	sorted_frequencies := make([]struct {
		ngram string
		freq  int
	}, 0, len(frequencies))
	for ngram, freq := range frequencies {
		sorted_frequencies = append(sorted_frequencies, struct {
			ngram string
			freq  int
		}{ngram, freq})
	}
	sort.Slice(sorted_frequencies, func(i, j int) bool {
		return sorted_frequencies[i].freq > sorted_frequencies[j].freq
	})

	selected_ngrams := make([]string, 0, dict_size/2)
	for _, sf := range sorted_frequencies[:dict_size/2] {
		selected_ngrams = append(selected_ngrams, sf.ngram)
	}
	sort.Strings(selected_ngrams)

	var intervals []struct {
		start string
		end   *string
	}
	var last_ngram string
	for _, ngram := range selected_ngrams {
		start := last_ngram
		if len(start) > 0 {
			start = start[:len(start)-1] + string(rune(start[len(start)-1])+1)
		}
		intervals = append(intervals, struct {
			start string
			end   *string
		}{start, &ngram})
		last_ngram = ngram
	}
	return intervals
}