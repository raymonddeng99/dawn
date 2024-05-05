pub mod rmq {
    use std::cmp::{max, min};

    pub struct Tree<T> {
        values: Vec<T>,
        euler: Vec<usize>,
        first: Vec<usize>,
        rmq: Vec<Vec<usize>>,
    }

    impl<T: Ord + Copy> Tree<T> {
        fn log2(n: usize) -> usize {
            (n as f64).log2() as usize + 1
        }

        fn precompute_rmq(&mut self) {
            let n = self.euler.len();
            let k = Self::log2(n);
            self.rmq = vec![vec![0; n]; k];

            for i in 0..n {
                self.rmq[0][i] = i;
            }

            for j in 1..k {
                for i in 0..n - (1 << (j - 1)) {
                    let x = self.rmq[j - 1][i];
                    let y = self.rmq[j - 1][i + (1 << (j - 1))];
                    self.rmq[j][i] = if self.euler[x] < self.euler[y] {
                        x
                    } else {
                        y
                    };
                }
            }
        }

        pub fn query(&self, l: usize, r: usize) -> T {
            let l = min(l, r);
            let r = max(l, r);
            let k = Self::log2(r - l + 1);
            let x = self.rmq[k][self.first[l]];
            let y = self.rmq[k][self.first[r] - (1 << k) + 1];
            min(self.values[self.euler[x]], self.values[self.euler[y]])
        }

        pub fn from_tree(values: &[T]) -> Self
        where
            T: Ord + Copy,
        {
            let n = values.len();
            let mut euler = vec![0; 2 * n];
            let mut first = vec![0; 2 * n];

            let mut build_euler = |tree: usize, i: usize| {
                let j = i + 1;
                euler[i] = tree;
                first[i] = j;
                if j < 2 * n {
                    build_euler(tree + 1, j);
                }
            };
            build_euler(0, 0);

            let mut tree = Tree {
                values: values.to_vec(),
                euler,
                first,
                rmq: vec![],
            };
            tree.precompute_rmq();
            tree
        }
    }
}