import (
    "math/rand"
)

func universalHash(a, b, c, d, e, key int) int {
    m := 1 << 32
    hash := a*key + b*key*key + c*key*key*key + d*key*key*key*key + e*key*key*key*key*key
    return hash % m
}

func linearProbe(table []interface{}, key int) int {
    m := len(table)
    a, b, c, d, e := rand.Intn(m), rand.Intn(m), rand.Intn(m), rand.Intn(m), rand.Intn(m)
    hash := universalHash(a, b, c, d, e, key)
    for i := 0; ; i++ {
        index := (hash + i) % m
        if table[index] == nil {
            return index
        }
    }
}