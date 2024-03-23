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
