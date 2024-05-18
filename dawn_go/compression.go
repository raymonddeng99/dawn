package bwt

func encode(s string) (string, int) {
	n := len(s)
	rotations := make([]string, n)
	for i := range rotations {
		rotations[i] = s[i:] + s[:i]
	}
	sortedRotations := sortStringSlice(rotations)
	encoded := make([]byte, n)
	for i, r := range sortedRotations {
		encoded[i] = r[n-1]
	}
	idx := findIndex(s, sortedRotations)
	return string(encoded), idx
}

func decode(encoded string, idx int) string {
	n := len(encoded)
	sortedRotations := make([]string, n)
	for i := range sortedRotations {
		sortedRotations[i] = string(encoded[i]) + string(lastChar(encoded[:i]+string(encoded[i])))
	}
	sortedRotations = sortStringSlice(sortedRotations)
	decoded := make([]byte, n)
	for i, r := range sortedRotations {
		decoded[i] = r[n-1]
	}
	return string(decoded[idx:]) + string(decoded[:idx])
}

func lastChar(s string) byte {
	return sortStringSlice([]string{s})[0][len(s)-1]
}

func findIndex(s string, sortedRotations []string) int {
	for i, r := range sortedRotations {
		if r == s {
			return i
		}
	}
	return -1
}

func sortStringSlice(s []string) []string {
	sortedSlice := make([]string, len(s))
	copy(sortedSlice, s)
	sort.Strings(sortedSlice)
	return sortedSlice
}