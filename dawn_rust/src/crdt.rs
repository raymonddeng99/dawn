// CRDTs

/*
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
*/

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
