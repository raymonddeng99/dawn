package surfbase

type node map[rune]node

func Build(keys []string) node {
    root := make(node)
    for _, key := range keys {
        curr := root
        for _, char := range key {
            next, ok := curr[char]
            if !ok {
                next = make(node)
                curr[char] = next
            }
            curr = next
        }
    }
    return root
}

func Lookup(trie node, key string) bool {
    curr := trie
    for _, char := range key {
        next, ok := curr[char]
        if !ok {
            return false
        }
        curr = next
    }
    return true
}