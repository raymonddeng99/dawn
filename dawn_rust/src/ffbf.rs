use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

struct BloomFilter {
    size: usize,
    vector: Vec<u8>,
    hash_funcs: Vec<u64>,
}

impl BloomFilter {
    fn new(size: usize, hash_funcs: Vec<u64>) -> Self {
        let vector = vec![0; (size + 7) / 8];
        BloomFilter {
            size,
            vector,
            hash_funcs,
        }
    }

    fn add(&mut self, element: &str) {
        let mut hasher = DefaultHasher::new();
        element.hash(&mut hasher);
        let hash = hasher.finish();

        for seed in &self.hash_funcs {
            let mut hasher = DefaultHasher::new();
            hasher.write_u64(seed);
            hasher.write(&element.as_bytes());
            let bit_index = (hasher.finish() % self.size as u64) as usize;
            self.vector[bit_index / 8] |= 1 << (bit_index % 8);
        }
    }

    fn contains(&self, element: &str) -> bool {
        let mut hasher = DefaultHasher::new();
        element.hash(&mut hasher);
        let hash = hasher.finish();

        for seed in &self.hash_funcs {
            let mut hasher = DefaultHasher::new();
            hasher.write_u64(seed);
            hasher.write(&element.as_bytes());
            let bit_index = (hasher.finish() % self.size as u64) as usize;
            if (self.vector[bit_index / 8] & (1 << (bit_index % 8))) == 0 {
                return false;
            }
        }
        true
    }
}