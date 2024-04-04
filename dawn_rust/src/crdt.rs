// CRDTs

/*
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
*/

use std::cmp::Ordering;
use std::sync::atomic::{AtomicI64, Ordering};
use std::collections::HashSet;
use std::hash::Hash;
use std::collections::HashMap;
use std::cmp::Eq;

enum Operation {
    Increment,
    Decrement,
}

struct Counter {
    value: i32,
    ops: Vec<Operation>,
}

impl Counter {
    fn new() -> Self {
        Counter { value: 0, ops: Vec::new() }
    }

    fn apply(mut self) -> Self {
        for op in self.ops.drain(..) {
            match op {
                Operation::Increment => self.value += 1,
                Operation::Decrement => self.value -= 1,
            }
        }
        self.ops = Vec::new();
        self
    }

    fn merge(mut self, other: Self) -> Self {
        let mut merged_ops = self.ops;
        merged_ops.extend(other.ops);
        let max_value = self.value.max(other.value);
        let mut final_value = max_value;
        for op in merged_ops {
            match op {
                Operation::Increment => final_value += 1,
                Operation::Decrement => final_value -= 1,
            }
        }
        self.value = final_value;
        self.ops = Vec::new();
        self
    }

    fn downstream() -> Vec<Operation> {
        Vec::new()
    }

    fn update(mut self, op: Operation) -> Self {
        self.ops.push(op);
        self
    }
}



// State based increment-only counter
pub struct GCounter(Vec<usize>);

impl GCounter {
    pub fn create(size: usize) -> GCounter {
        GCounter(vec![0; size])
    }

    pub fn update(&mut self, i: usize) -> Result<(), &'static str> {
        if i >= self.0.len() {
            return Err("Index out of bounds");
        }
        self.0[i] += 1;
        Ok(())
    }

    pub fn query(&self, i: usize) -> Result<usize, &'static str> {
        if i >= self.0.len() {
            return Err("Index out of bounds");
        }
        Ok(self.0[i])
    }

    pub fn compare(&self, other: &GCounter) -> Result<Ordering, &'static str> {
        if self.0.len() != other.0.len() {
            return Err("Vectors have different lengths");
        }
        for i in 0..self.0.len() {
            if self.0[i] < other.0[i] {
                return Ok(Ordering::Less);
            } else if self.0[i] > other.0[i] {
                return Ok(Ordering::Greater);
            }
        }
        Ok(Ordering::Equal)
    }

    pub fn merge(&self, other: &GCounter) -> Result<GCounter, &'static str> {
        if self.0.len() != other.0.len() {
            return Err("Vectors have different lengths");
        }
        let mut result = Vec::with_capacity(self.0.len());
        for i in 0..self.0.len() {
            result.push(std::cmp::max(self.0[i], other.0[i]));
        }
        Ok(GCounter(result))
    }
}


// State-based PN Counter
pub struct PNCounter {
    p: Vec<i32>,
    n: Vec<i32>,
}

impl PNCounter {
    pub fn initialize(size: usize, p: Vec<i32>, n: Vec<i32>) -> PNCounter {
        PNCounter {
            p: p.clone(),
            n: n.clone(),
        }
    }

    pub fn increment(&mut self) {
        let g = my_id();
        self.p[g] += 1;
    }

    pub fn decrement(&mut self) {
        let g = my_id();
        self.n[g] += 1;
    }

    pub fn value(&self) -> i32 {
        self.p.iter().sum::<i32>() - self.n.iter().sum::<i32>()
    }

    pub fn compare(&self, other: &PNCounter) -> bool {
        self.p.iter().zip(&other.p).all(|(x, y)| x <= y)
            && self.n.iter().zip(&other.n).all(|(x, y)| x <= y)
    }

    pub fn merge(&self, other: &PNCounter) -> PNCounter {
        let mut z = PNCounter {
            p: vec![0; self.p.len()],
            n: vec![0; self.n.len()],
        };

        for i in 0..z.p.len() {
            z.p[i] = self.p[i].max(other.p[i]);
            z.n[i] = self.n[i].max(other.n[i]);
        }

        z
    }
}


// State-based last-writer-wins register
struct LastWriterWinsRegister {
    value: AtomicI64,
    timestamp: AtomicI64,
}

impl LastWriterWinsRegister {
    fn new(initial_value: i64) -> LastWriterWinsRegister {
        LastWriterWinsRegister {
            value: AtomicI64::new(initial_value),
            timestamp: AtomicI64::new(0),
        }
    }

    fn read(&self) -> i64 {
        self.value.load(Ordering::Acquire)
    }

    fn write(&self, new_value: i64, new_timestamp: i64) {
        loop {
            let old_timestamp = self.timestamp.load(Ordering::Acquire);
            if new_timestamp <= old_timestamp {
                return;
            }

            let old_value = self.value.swap(new_value, Ordering::AcqRel);
            if self
                .timestamp
                .compare_exchange(
                    old_timestamp,
                    new_timestamp,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
            {
                return;
            }

            // Retry if the timestamps didn't match
            self.value.swap(old_value, Ordering::AcqRel);
        }
    }

    fn compare_and_swap(
        &self,
        expected_value: i64,
        expected_timestamp: i64,
        new_value: i64,
        new_timestamp: i64,
    ) -> bool {
        let old_value = self.value.load(Ordering::Acquire);
        let old_timestamp = self.timestamp.load(Ordering::Acquire);

        if old_value == expected_value && old_timestamp == expected_timestamp {
            self.write(new_value, new_timestamp);
            true
        } else {
            false
        }
    }
}


// Operation-based last-writer-wins register
#[derive(Copy, Clone, Eq, PartialEq)]
struct OpBasedLWWValue {
    val: i64,
    ts: i64,
}

impl OpBasedLWWValue {
    fn new(val: i64, ts: i64) -> Self {
        OpBasedLWWValue { val, ts }
    }
}

enum OpBasedLWWOp {
    Update(OpBasedLWWValue),
    Reset,
}

struct OpBasedLWWRegister {
    value: OpBasedLWWValue,
    pending: Vec<OpBasedLWWOp>,
}

impl OpBasedLWWRegister {
    fn new(initial_value: i64) -> OpBasedLWWRegister {
        OpBasedLWWRegister {
            value: OpBasedLWWValue::new(initial_value, 0),
            pending: Vec::new(),
        }
    }

    fn read(&self) -> i64 {
        self.value.val
    }

    fn update(&mut self, new_value: i64, new_timestamp: i64) {
        if new_timestamp > self.value.ts {
            self.value = OpBasedLWWValue::new(new_value, new_timestamp);
            self.pending.clear();
        } else {
            self.pending.push(OpBasedLWWOp::Update(OpBasedLWWValue::new(new_value, new_timestamp)));
        }
    }

    fn reset(&mut self) {
        self.pending.push(OpBasedLWWOp::Reset);
    }

    fn apply_pending(&mut self) {
        let mut new_pending = Vec::new();
        for op in self.pending.drain(..) {
            match op {
                OpBasedLWWOp::Update(value) => {
                    if value.ts > self.value.ts {
                        self.value = value;
                    } else {
                        new_pending.push(OpBasedLWWOp::Update(value));
                    }
                }
                OpBasedLWWOp::Reset => {
                    self.value = OpBasedLWWValue::new(0, 0);
                }
            }
        }
        self.pending = new_pending;
    }

    fn downstream(&mut self) {
        self.apply_pending();
    }
}



// State-based multi-value register
pub struct MVRegisterValue<X> {
    x: X,
    v: Vec<i64>,
}

impl<X: Clone> MVRegisterValue<X> {
    fn new(x: X, v: Vec<i64>) -> Self {
        MVRegisterValue { x, v }
    }
}

pub struct MVRegister<X> {
    payload: Vec<MVRegisterValue<X>>,
}

impl<X: Clone + Eq + std::hash::Hash> MVRegister<X> {
    pub fn initial() -> Self {
        MVRegister {
            payload: vec![MVRegisterValue::new(X::default(), Vec::new())],
        }
    }

    pub fn query_increment_vv(&self, process_id: usize) -> Vec<i64> {
        let mut max_version = self
            .payload
            .iter()
            .map(|entry| entry.v.iter().copied().max().unwrap_or(0))
            .max()
            .unwrap_or(0);

        let mut new_version = self
            .payload
            .get(process_id)
            .map(|entry| entry.v.clone())
            .unwrap_or_else(|| vec![0; self.payload.len()]);

        new_version[process_id] += 1;
        max_version = std::cmp::max(max_version, new_version[process_id]);

        for i in 0..new_version.len() {
            new_version[i] = std::cmp::max(new_version[i], max_version);
        }

        new_version
    }

    pub fn update_assign(&mut self, set_r: &[X], process_id: usize) {
        let new_version = self.query_increment_vv(process_id);
        self.payload.extend(set_r.iter().map(|&x| {
            MVRegisterValue::new(x.clone(), new_version.clone())
        }));
    }

    pub fn query_value(&self) -> &Vec<MVRegisterValue<X>> {
        &self.payload
    }

    pub fn compare(&self, other: &Self) -> bool {
        self.payload.iter().any(|entry| {
            other.payload.iter().any(|other_entry| {
                entry.x == other_entry.x
                    && entry
                        .v
                        .iter()
                        .any(|&v_entry| other_entry.v.iter().all(|&v_other| v_entry > v_other))
            })
        })
    }

    pub fn merge(&self, other: &Self) -> Self {
        let mut merged = std::collections::HashSet::new();

        for entry in &self.payload {
            if other.payload.iter().any(|other_entry| {
                entry.x == other_entry.x
                    && (other_entry
                        .v
                        .iter()
                        .any(|&w_entry| w_entry >= *entry.v.last().unwrap_or(&0))
                        || entry
                            .v
                            .iter()
                            .any(|&v_entry| other_entry.v.iter().all(|&w_entry| v_entry > w_entry)))
            }) {
                merged.insert(MVRegisterValue::new(entry.x.clone(), entry.v.clone()));
            }
        }

        for other_entry in &other.payload {
            if self.payload.iter().any(|entry| {
                other_entry.x == entry.x
                    && (entry
                        .v
                        .iter()
                        .any(|&v_entry| v_entry >= *other_entry.v.last().unwrap_or(&0))
                        || other_entry
                            .v
                            .iter()
                            .any(|&w_entry| entry.v.iter().any(|&v_entry| w_entry < v_entry)))
            }) {
                merged.insert(MVRegisterValue::new(
                    other_entry.x.clone(),
                    other_entry.v.clone(),
                ));
            }
        }

        MVRegister {
            payload: merged.into_iter().collect(),
        }
    }
}


// State-based grow-only set
pub struct GSet<T> {
    data: HashSet<T>,
}

impl<T> GSet<T> {
    pub fn new() -> Self {
        GSet { data: HashSet::new() }
    }

    pub fn add(&mut self, element: T) {
        self.data.insert(element);
    }

    pub fn lookup(&self, element: &T) -> bool {
        self.data.contains(element)
    }

    pub fn compare(&self, other: &GSet<T>) -> bool {
        self.data == other.data
    }

    pub fn merge(&self, other: &GSet<T>) -> GSet<T> {
        let mut merged = GSet::new();
        merged.data.extend(self.data.iter().cloned());
        merged.data.extend(other.data.iter().cloned());
        merged
    }
}

// State-based 2P set
pub struct StateBased2PSet<T>
where
    T: Eq + std::hash::Hash,
{
    added: HashSet<T>,
    removed: HashSet<T>,
}

impl<T> StateBased2PSet<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    pub fn new() -> Self {
        Self {
            added: HashSet::new(),
            removed: HashSet::new(),
        }
    }

    pub fn lookup(&self, e: &T) -> bool {
        self.added.contains(e) && !self.removed.contains(e)
    }

    pub fn add(&mut self, e: T) {
        if !self.lookup(&e) {
            self.added.insert(e);
        }
    }

    pub fn remove(&mut self, e: &T) {
        if self.lookup(e) {
            self.removed.insert(e.clone());
        }
    }

    pub fn compare(&self, other: &Self) -> bool {
        self.added.is_subset(&other.added) && self.removed.is_subset(&other.removed)
    }

    pub fn merge(&self, other: &Self) -> Self {
        let mut merged = Self::new();
        merged.added = self.added.union(&other.added).cloned().collect();
        merged.removed = self.removed.union(&other.removed).cloned().collect();
        merged
    }
}


// Op based 2p set with unique elements
pub struct USet<T>
where
    T: Eq + Hash,
{
    data: HashSet<T>,
}

impl<T> USet<T>
where
    T: Eq + Hash,
{
    pub fn new() -> Self {
        USet {
            data: HashSet::new(),
        }
    }

    pub fn add(&mut self, element: T) {
        self.data.insert(element);
    }

    pub fn lookup(&self, element: &T) -> bool {
        self.data.contains(element)
    }

    pub fn compare(&self, other: &USet<T>) -> bool {
        self.data == other.data
    }

    pub fn merge(&self, other: &USet<T>) -> USet<T> {
        let mut merged = USet::new();
        merged.data.extend(self.data.iter().cloned());
        merged.data.extend(other.data.iter().cloned());
        merged
    }
}


// Molli, Weiss, Skaf set
pub struct MWSSet<T>
where
    T: Eq + Hash + Copy,
{
    data: HashMap<T, i32>,
}

impl<T> MWSSet<T>
where
    T: Eq + Hash + Copy,
{
    pub fn new() -> Self {
        MWSSet {
            data: HashMap::new(),
        }
    }

    pub fn lookup(&self, e: &T) -> bool {
        match self.data.get(e) {
            Some(k) => *k > 0,
            None => false,
        }
    }

    pub fn add(&mut self, e: T) {
        let j = match self.data.get(&e) {
            Some(k) if *k < 0 => (-*k) as i32 + 1,
            _ => 1,
        };
        self.data.insert(e, j);
    }

    pub fn remove(&mut self, e: &T) {
        if let Some(k) = self.data.get_mut(e) {
            if *k > 0 {
                *k -= 1;
            }
        }
        self.data.retain(|_, k| *k != 0);
    }
}


// Operation based observed-remove set
pub struct ORSet<T>
where
    T: Eq + Hash + Copy,
{
    data: HashMap<T, bool>,
}

impl<T> ORSet<T>
where
    T: Eq + Hash + Copy,
{
    pub fn new() -> Self {
        ORSet {
            data: HashMap::new(),
        }
    }

    pub fn lookup(&self, e: &T) -> bool {
        self.data.contains_key(e)
    }

    pub fn add(&mut self, e: T) {
        self.data.insert(e, true);
    }

    pub fn remove(&mut self, e: &T) {
        self.data.remove(e);
    }
}


// Operation based 2P2P graph
type Vertex = u32;
type Edge = (Vertex, Vertex);

struct Graph {
    va: HashSet<Vertex>,
    vr: HashSet<Vertex>,
    ea: HashSet<Edge>,
    er: HashSet<Edge>,
}

impl Graph {
    fn new() -> Self {
        Graph {
            va: HashSet::new(),
            vr: HashSet::new(),
            ea: HashSet::new(),
            er: HashSet::new(),
        }
    }

    fn lookup_vertex(&self, v: Vertex) -> bool {
        self.va.contains(&v) && !self.vr.contains(&v)
    }

    fn lookup_edge(&self, e: &Edge) -> bool {
        let (u, v) = *e;
        self.va.contains(&u)
            && self.va.contains(&v)
            && (self.ea.contains(e) || self.er.contains(e))
    }

    fn add_vertex(&mut self, v: Vertex) {
        self.va.insert(v);
    }

    fn add_edge(&mut self, u: Vertex, v: Vertex) {
        if self.va.contains(&u) && self.va.contains(&v) {
            self.ea.insert((u, v));
        }
    }

    fn remove_vertex(&mut self, v: Vertex) {
        if self.va.contains(&v) {
            let mut can_remove = true;
            for e in self.ea.iter().chain(self.er.iter()) {
                if e.0 == v || e.1 == v {
                    can_remove = false;
                    break;
                }
            }
            if can_remove {
                self.va.remove(&v);
                self.vr.insert(v);
            }
        }
    }

    fn remove_edge(&mut self, u: Vertex, v: Vertex) {
        if self.va.contains(&u) && self.va.contains(&v) {
            self.er.insert((u, v));
        }
    }
}