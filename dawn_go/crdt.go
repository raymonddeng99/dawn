// CRDTs

/*
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)

Op-based require delivery order exists and concurrent updates commute
*/

package main

import "fmt"
import "sync/atomic"

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


// State-based last-writer-wins register
type LastWriterWinsRegister struct {
    value     int64
    timestamp int64
}

func NewLastWriterWinsRegister(initialValue int64) *LastWriterWinsRegister {
    return &LastWriterWinsRegister{value: initialValue, timestamp: 0}
}

func (r *LastWriterWinsRegister) Read() int64 {
    return atomic.LoadInt64(&r.value)
}

func (r *LastWriterWinsRegister) Write(newValue, newTimestamp int64) {
    for {
        oldValue := r.Read()
        oldTimestamp := atomic.LoadInt64(&r.timestamp)
        if newTimestamp <= oldTimestamp {
            return
        }
        if atomic.CompareAndSwapInt64(&r.value, oldValue, newValue) &&
            atomic.CompareAndSwapInt64(&r.timestamp, oldTimestamp, newTimestamp) {
            return
        }
    }
}

func (r *LastWriterWinsRegister) CompareAndSwap(expectedValue int64, expectedTimestamp int64, newValue int64, newTimestamp int64) bool {
    oldValue := r.Read()
    oldTimestamp := atomic.LoadInt64(&r.timestamp)
    if oldValue == expectedValue && oldTimestamp == expectedTimestamp {
        r.Write(newValue, newTimestamp)
        return true
    }
    return false
}


// Operation-based last-write-wins register
type OpBasedLWWValue struct {
    val int64
    ts  int64
}

type OpBasedLWWOp int

const (
    OpBasedLWWUpdate OpBasedLWWOp = iota
    OpBasedLWWReset
)

type OpBasedLWWRegister struct {
    value   OpBasedLWWValue
    pending []OpBasedLWWOp
}

func NewOpBasedLWWRegister(initialValue int64) *OpBasedLWWRegister {
    return &OpBasedLWWRegister{value: OpBasedLWWValue{val: initialValue, ts: 0}, pending: []OpBasedLWWOp{}}
}

func (r *OpBasedLWWRegister) Read() int64 {
    return r.value.val
}

func (r *OpBasedLWWRegister) Update(newValue, newTimestamp int64) {
    oldValue := r.value
    if newTimestamp > oldValue.ts {
        r.value = OpBasedLWWValue{val: newValue, ts: newTimestamp}
        r.pending = []OpBasedLWWOp{}
    } else {
        r.pending = append(r.pending, OpBasedLWWUpdate)
    }
}

func (r *OpBasedLWWRegister) Reset() {
    r.pending = append(r.pending, OpBasedLWWReset)
}

func (r *OpBasedLWWRegister) ApplyPending() {
    for _, op := range r.pending {
        switch op {
        case OpBasedLWWUpdate:
            if r.pending[0].ts > r.value.ts {
                r.value = OpBasedLWWValue{val: r.pending[0].val, ts: r.pending[0].ts}
                r.pending = r.pending[1:]
            }
        case OpBasedLWWReset:
            r.value = OpBasedLWWValue{val: 0, ts: 0}
            r.pending = r.pending[1:]
        }
    }
}

func (r *OpBasedLWWRegister) Downstream() {
    r.ApplyPending()
}