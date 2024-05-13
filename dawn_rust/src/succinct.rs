pub fn rank_suffix(text: &str, rank_of_char: &[u32], n: usize, i: usize) -> u32 {
    text.chars()
        .skip(i)
        .fold(0, |r, c| r * (rank_of_char.len() as u32) + rank_of_char[c as usize])
}

pub fn compute_rank_of_char(text: &str) -> Vec<u32> {
    let mut rank_of_char = vec![0u32; 256];
    let mut used = vec![false; 256];
    let mut rank = 0u32;
    for c in text.chars() {
        if !used[c as usize] {
            rank_of_char[c as usize] = rank;
            rank += 1;
            used[c as usize] = true;
        }
    }
    rank_of_char
}

pub fn suffix_array(text: &str) -> Vec<usize> {
    let n = text.len();
    let rank_of_char = compute_rank_of_char(text);
    let mut ranks: Vec<u32> = (0..n).map(|i| rank_suffix(text, &rank_of_char, n, i)).collect();
    let mut indices: Vec<usize> = (0..n).collect();
    indices.sort_by(|&i, &j| ranks[i].cmp(&ranks[j]));
    indices
}


// Succinct static data structures, Jacobsen '89
pub struct BitVector {
    bits: Vec<bool>,
    rank_table: Vec<usize>,
    select_table: Vec<isize>,
}

impl BitVector {
    const K: usize = 6;

    fn rank_naive(&self, i: usize) -> usize {
        self.bits.iter().take(i + 1).filter(|&&b| b).count()
    }

    fn select_naive(&self, i: usize) -> isize {
        let mut cnt = 0;
        let mut j = 0;
        for j in 0..self.bits.len() {
            if self.bits[j] {
                cnt += 1;
                if cnt == i + 1 {
                    return j as isize;
                }
            }
        }
        -1
    }

    pub fn create(bits: Vec<bool>) -> BitVector {
        let n = bits.len();
        let mut rank_table = vec![0; n + 1];
        let mut select_table = vec![-1; n + 1];

        for i in 0..=n {
            if i % (1 << Self::K) == 0 {
                rank_table[i] = 0;
                select_table[i] = -1;
            } else {
                let prev = i - (i % (1 << Self::K));
                rank_table[i] = rank_table[prev] + BitVector::rank_naive(&bits, i) - BitVector::rank_naive(&bits, prev);
                select_table[i] = select_table[prev] as isize + BitVector::select_naive(&bits, i) - BitVector::select_naive(&bits, prev);
            }
        }

        BitVector { bits, rank_table, select_table }
    }

    pub fn rank(&self, i: usize) -> usize {
        self.rank_table[i]
    }

    pub fn select(&self, i: usize) -> isize {
        let mut l = 0;
        let mut r = self.bits.len();
        while l <= r {
            let m = l + (r - l) / 2;
            if self.rank(m) < i {
                l = m + 1;
            } else {
                r = m - 1;
            }
        }
        if l < self.bits.len() && self.rank(l) == i {
            l as isize
        } else {
            -1
        }
    }
}