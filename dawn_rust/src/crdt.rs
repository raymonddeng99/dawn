// CRDTs

/*
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
*/

use std::cmp::Ordering;

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