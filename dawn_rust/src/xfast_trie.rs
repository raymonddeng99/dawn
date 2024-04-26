use std::collections::VecDeque;

pub struct XFastTrie {
    bits: Vec<bool>,
    ones: VecDeque<usize>,
    size: usize,
}

impl XFastTrie {
    pub fn create(w: u32) -> Self {
        let u = 1 << w;
        let bits = vec![false; 2 * u as usize - 1];
        XFastTrie {
            bits,
            ones: VecDeque::new(),
            size: 0,
        }
    }

    pub fn insert(&mut self, x: usize) {
        self.insert_aux(x, 0);
    }

    fn insert_aux(&mut self, x: usize, i: usize) {
        if i >= self.bits.len() {
            self.size += 1;
            return;
        }
        let bit = (x & (1 << i)) != 0;
        let index = 1 << (self.bits.len() - 1 - i);
        self.bits[index - 1] = self.bits[index - 1] || bit;
        if bit {
            self.ones.push_front(index);
            self.insert_aux(x, i + 1);
        } else {
            self.insert_aux(x, i + 1);
        }
    }

    pub fn predecessor(&self, x: usize) -> Option<usize> {
        self.predecessor_aux(x, 0)
    }

    fn predecessor_aux(&self, x: usize, i: usize) -> Option<usize> {
        if i >= self.bits.len() {
            return None;
        }
        let index = 1 << (self.bits.len() - 1 - i);
        if self.bits[index - 1] {
            let left_child = 2 * index;
            let right_child = left_child + 1;
            if self.bits[right_child - 1] {
                self.predecessor_aux(x, i + 1)
            } else {
                self.ones.front().copied()
            }
        } else {
            self.predecessor_aux(x, i + 1)
        }
    }
}