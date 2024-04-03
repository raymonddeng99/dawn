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


// State-based multi-value register
type MVRegisterValue struct {
    x interface{}
    v []int64
}

type MVRegister struct {
    payload []MVRegisterValue
}

func NewMVRegister() *MVRegister {
    return &MVRegister{
        payload: []MVRegisterValue{
            {x: nil, v: []int64{}},
        },
    }
}

func (r *MVRegister) QueryIncrementVV(processID int) []int64 {
    maxVersion := int64(0)
    for _, entry := range r.payload {
        for _, v := range entry.v {
            if v > maxVersion {
                maxVersion = v
            }
        }
    }
    maxVersion++

    newVersion := make([]int64, len(r.payload))
    for i := range newVersion {
        newVersion[i] = maxVersion
    }
    newVersion[processID]++

    return newVersion
}

func (r *MVRegister) UpdateAssign(set_r []interface{}, processID int) {
    newVersion := r.QueryIncrementVV(processID)
    for _, x := range set_r {
        r.payload = append(r.payload, MVRegisterValue{x: x, v: newVersion})
    }
}

func (r *MVRegister) QueryValue() []MVRegisterValue {
    return r.payload
}

func (r *MVRegister) Compare(other *MVRegister) bool {
    for _, entryA := range r.payload {
        for _, entryB := range other.payload {
            if entryA.x == entryB.x {
                for _, vA := range entryA.v {
                    if allLessThan(vA, entryB.v) {
                        return true
                    }
                }
            }
        }
    }
    return false
}

func allLessThan(val int64, arr []int64) bool {
    for _, v := range arr {
        if val <= v {
            return false
        }
    }
    return true
}

func (r *MVRegister) Merge(other *MVRegister) *MVRegister {
    merged := NewMVRegister()
    for _, entryA := range r.payload {
        include := false
        for _, entryB := range other.payload {
            if entryA.x == entryB.x {
                if !allLessThan(entryB.v[len(entryB.v)-1], entryA.v) || anyLessThan(entryA.v, entryB.v) {
                    include = true
                    break
                }
            }
        }
        if include {
            merged.payload = append(merged.payload, entryA)
        }
    }

    for _, entryB := range other.payload {
        include := false
        for _, entryA := range r.payload {
            if entryB.x == entryA.x {
                if !allLessThan(entryA.v[len(entryA.v)-1], entryB.v) || anyLessThan(entryB.v, entryA.v) {
                    include = true
                    break
                }
            }
        }
        if include {
            merged.payload = append(merged.payload, entryB)
        }
    }

    return merged
}

func anyLessThan(arr []int64, val int64) bool {
    for _, v := range arr {
        if v < val {
            return true
        }
    }
    return false
}


// State-based grow-only set
type GSet[T any] struct {
    data map[T]struct{}
}

func NewGSet[T any]() *GSet[T] {
    return &GSet[T]{data: make(map[T]struct{})}
}

func (s *GSet[T]) Add(e T) {
    s.data[e] = struct{}{}
}

func (s *GSet[T]) Lookup(e T) bool {
    _, ok := s.data[e]
    return ok
}

func (s *GSet[T]) Compare(other *GSet[T]) bool {
    if len(s.data) != len(other.data) {
        return false
    }
    for k := range s.data {
        if _, ok := other.data[k]; !ok {
            return false
        }
    }
    return true
}

func (s *GSet[T]) Merge(other *GSet[T]) *GSet[T] {
    merged := NewGSet[T]()
    for k := range s.data {
        merged.Add(k)
    }
    for k := range other.data {
        merged.Add(k)
    }
    return merged
}

// State-based 2P set
type StateBased2PSet[T comparable] struct {
    added   map[T]bool
    removed map[T]bool
}

func NewStateBased2PSet[T comparable]() *StateBased2PSet[T] {
    return &StateBased2PSet[T]{
        added:   make(map[T]bool),
        removed: make(map[T]bool),
    }
}

func (s *StateBased2PSet[T]) Lookup(e T) bool {
    _, inAdded := s.added[e]
    _, inRemoved := s.removed[e]
    return inAdded && !inRemoved
}

func (s *StateBased2PSet[T]) Add(e T) {
    if !s.Lookup(e) {
        s.added[e] = true
    }
}

func (s *StateBased2PSet[T]) Remove(e T) {
    if s.Lookup(e) {
        s.removed[e] = true
    }
}

func (s *StateBased2PSet[T]) Compare(other *StateBased2PSet[T]) bool {
    for k := range s.added {
        if !other.Lookup(k) {
            return false
        }
    }
    for k := range s.removed {
        if !other.Lookup(k) {
            return false
        }
    }
    return true
}

func (s *StateBased2PSet[T]) Merge(other *StateBased2PSet[T]) *StateBased2PSet[T] {
    merged := NewStateBased2PSet[T]()
    for k := range s.added {
        merged.added[k] = true
    }
    for k := range other.added {
        merged.added[k] = true
    }
    for k := range s.removed {
        merged.removed[k] = true
    }
    for k := range other.removed {
        merged.removed[k] = true
    }
    return merged
}

// Op based 2p set with unique elements
type Element comparable

type USet[T Element] struct {
    added   map[T]bool
    removed map[T]bool
}

func NewUSet[T Element]() *USet[T] {
    return &USet[T]{
        added:   make(map[T]bool),
        removed: make(map[T]bool),
    }
}

func (s *USet[T]) Lookup(e T) bool {
    _, inAdded := s.added[e]
    _, inRemoved := s.removed[e]
    return inAdded && !inRemoved
}

func (s *USet[T]) Add(e T) {
    if !s.Lookup(e) {
        s.added[e] = true
    }
}

func (s *USet[T]) Remove(e T) {
    if s.Lookup(e) {
        s.removed[e] = true
    }
}

func (s *USet[T]) Compare(other *USet[T]) bool {
    for k := range s.added {
        if !other.Lookup(k) {
            return false
        }
    }
    for k := range s.removed {
        if !other.Lookup(k) {
            return false
        }
    }
    return true
}

func (s *USet[T]) Merge(other *USet[T]) *USet[T] {
    merged := NewUSet[T]()
    for k := range s.added {
        merged.added[k] = true
    }
    for k := range other.added {
        merged.added[k] = true
    }
    for k := range s.removed {
        merged.removed[k] = true
    }
    for k := range other.removed {
        merged.removed[k] = true
    }
    return merged
}


// Molli, Weiss, Skaf set
type MWSElement interface{}

type MWSSet map[MWSElement]int

func NewMWSSet() MWSSet {
    return make(MWSSet)
}

func (s MWSSet) Lookup(e MWSElement) bool {
    k, ok := s[e]
    return ok && k > 0
}

func (s MWSSet) Add(e MWSElement) MWSSet {
    k, ok := s[e]
    j := 1
    if ok && k < 0 {
        j = -k + 1
    }
    s[e] = j
    return s
}

func (s MWSSet) Remove(e MWSElement) MWSSet {
    k, ok := s[e]
    if ok && k > 0 {
        s[e] = k - 1
    }
    for k, v := range s {
        if v == 0 {
            delete(s, k)
        }
    }
    return s
}

// Operation based observed-remove set
type UniqueTagORSet struct{}

type ElemORSet struct {
    value int
    tag   UniqueTagORSet
}

type SetORSet []ElemORSet

func uniqueTagORSet() UniqueTagORSet {
    return UniqueTagORSet{}
}

func uniqueElementsORSet(set SetORSet) SetORSet {
    result := make(SetORSet, 0, len(set))
    seen := make(map[ElemORSet]bool)
    for _, elem := range set {
        if !seen[elem] {
            seen[elem] = true
            result = append(result, elem)
        }
    }
    return result
}

func EmptySetORSet() SetORSet {
    return make(SetORSet, 0)
}

func AddORSet(value int, set SetORSet) SetORSet {
    elem := ElemORSet{value, uniqueTagORSet()}
    return uniqueElementsORSet(append(set, elem))
}

func LookupORSet(value int, set SetORSet) bool {
    for _, elem := range set {
        if elem.value == value {
            return true
        }
    }
    return false
}

func RemoveORSet(value int, set SetORSet) SetORSet {
    result := make(SetORSet, 0, len(set))
    for _, elem := range set {
        if elem.value != value {
            result = append(result, elem)
        }
    }
    return result
}

func DownstreamORSet(set SetORSet) SetORSet {
    return set
}

func PreConditionORSet(f func(SetORSet) SetORSet, set SetORSet) SetORSet {
    return f(set)
}

func AtSourceORSet(value int) int {
    return value
}


// Operation based 2P2P graph
type Vertex int
type Edge struct {
    U, V Vertex
}

type Graph struct {
    Va, Vr map[Vertex]struct{}
    Ea, Er map[Edge]struct{}
}

func NewGraph() *Graph {
    return &Graph{
        Va: make(map[Vertex]struct{}),
        Vr: make(map[Vertex]struct{}),
        Ea: make(map[Edge]struct{}),
        Er: make(map[Edge]struct{}),
    }
}