// CRDTs

/*
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)

Op-based require delivery order exists and concurrent updates commute
*/

package main

import "fmt"

type Operation int

const (
    Increment Operation = iota
    Decrement
)

type Counter struct {
    value int
    ops   []Operation
}

func NewCounter() *Counter {
    return &Counter{0, []Operation{}}
}

func (c *Counter) Apply() *Counter {
    value := c.value
    for _, op := range c.ops {
        switch op {
        case Increment:
            value++
        case Decrement:
            value--
        }
    }
    return &Counter{value, []Operation{}}
}

func (c *Counter) Merge(other *Counter) *Counter {
    mergedOps := append(c.ops, other.ops...)
    maxValue := max(c.value, other.value)
    value := maxValue
    for _, op := range mergedOps {
        switch op {
        case Increment:
            value++
        case Decrement:
            value--
        }
    }
    return &Counter{value, []Operation{}}
}

func (c *Counter) Downstream() []Operation {
    return []Operation{}
}

func (c *Counter) Update(op Operation) *Counter {
    return &Counter{c.value, append(c.ops, op)}
}

// State based increment-only counter
type GCounter []int

func NewGCounter(size int) GCounter {
    return make(GCounter, size)
}

func GCounterOperations() (func(GCounter, int) error, func(GCounter, int) (int, error), func(GCounter, GCounter) (int, error), func(GCounter, GCounter) (GCounter, error)) {
    update := func(c GCounter, i int) error {
        if i < 0 || i >= len(c) {
            return errors.New("Index out of bounds")
        }
        c[i]++
        return nil
    }

    query := func(c GCounter, i int) (int, error) {
        if i < 0 || i >= len(c) {
            return 0, errors.New("Index out of bounds")
        }
        return c[i], nil
    }

    compare := func(c1, c2 GCounter) (int, error) {
        if len(c1) != len(c2) {
            return 0, errors.New("Vectors have different lengths")
        }
        for i := range c1 {
            if c1[i] < c2[i] {
                return -1, nil
            } else if c1[i] > c2[i] {
                return 1, nil
            }
        }
        return 0, nil
    }

    merge := func(c1, c2 GCounter) (GCounter, error) {
        if len(c1) != len(c2) {
            return nil, errors.New("Vectors have different lengths")
        }
        c := make(GCounter, len(c1))
        for i := range c1 {
            if c1[i] > c2[i] {
                c[i] = c1[i]
            } else {
                c[i] = c2[i]
            }
        }
        return c, nil
    }

    return update, query, compare, merge
}


// State-based PN Counter
type PNCounter struct {
    p []int
    n []int
}

func NewPNCounter(size int) *PNCounter {
    return &PNCounter{
        p: make([]int, size),
        n: make([]int, size),
    }
}

func PNCounterOperations() (
    func(*PNCounter, int) error,
    func(*PNCounter, int) (int, error),
    func(*PNCounter, *PNCounter) (int, error),
    func(*PNCounter, *PNCounter) (*PNCounter, error),
) {
    increment := func(c *PNCounter, i int) error {
        if i < 0 || i >= len(c.p) {
            return errors.New("Index out of bounds")
        }
        c.p[i]++
        return nil
    }

    decrement := func(c *PNCounter, i int) error {
        if i < 0 || i >= len(c.n) {
            return errors.New("Index out of bounds")
        }
        c.n[i]++
        return nil
    }

    value := func(c *PNCounter, i int) (int, error) {
        if i < 0 || i >= len(c.p) {
            return 0, errors.New("Index out of bounds")
        }
        return c.p[i] - c.n[i], nil
    }

    compare := func(c1, c2 *PNCounter) (int, error) {
        if len(c1.p) != len(c2.p) || len(c1.n) != len(c2.n) {
            return 0, errors.New("Vectors have different lengths")
        }
        for i := range c1.p {
            if c1.p[i] < c2.p[i] || c1.n[i] < c2.n[i] {
                return -1, nil
            } else if c1.p[i] > c2.p[i] || c1.n[i] > c2.n[i] {
                return 1, nil
            }
        }
        return 0, nil
    }

    merge := func(c1, c2 *PNCounter) (*PNCounter, error) {
        if len(c1.p) != len(c2.p) || len(c1.n) != len(c2.n) {
            return nil, errors.New("Vectors have different lengths")
        }
        merged := &PNCounter{
            p: make([]int, len(c1.p)),
            n: make([]int, len(c1.n)),
        }
        for i := range c1.p {
            merged.p[i] = max(c1.p[i], c2.p[i])
            merged.n[i] = max(c1.n[i], c2.n[i])
        }
        return merged, nil
    }

    return increment, decrement, value, compare, merge
}