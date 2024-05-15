use std::cmp::Ordering;

pub struct PriorityQueue<T: Ord> {
    size: usize,
    root: Option<Cluster<T>>,
}

struct Cluster<T: Ord> {
    values: Vec<T>,
    children: Vec<Option<Box<Cluster<T>>>>,
}

const CLUSTER_SIZE: usize = 2;

impl<T: Ord> PriorityQueue<T> {
    pub fn new() -> Self {
        PriorityQueue {
            size: 0,
            root: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    pub fn insert(&mut self, value: T) {
        let new_root = match self.root.take() {
            None => Cluster::new_leaf(value),
            Some(mut root) => {
                root.insert(value);
                root
            }
        };
        self.root = Some(new_root);
        self.size += 1;
    }

    pub fn find_min(&self) -> Option<&T> {
        self.root.as_ref().and_then(|root| root.find_min())
    }

    pub fn remove_min(&mut self) -> Option<T> {
        let (min, new_root) = match self.root.take() {
            None => return None,
            Some(mut root) => {
                let min = root.remove_min();
                (min, root)
            }
        };
        self.root = new_root;
        self.size -= 1;
        min
    }
}

impl<T: Ord> Cluster<T> {
    fn new_leaf(value: T) -> Box<Self> {
        Box::new(Cluster {
            values: vec![value],
            children: vec![],
        })
    }

    fn insert(&mut self, value: T) {
        let idx = match self.values.binary_search(&value) {
            Ok(idx) => idx,
            Err(idx) => idx,
        };
        self.values.insert(idx, value);

        if self.values.len() > CLUSTER_SIZE {
            self.split();
        }
    }

    fn split(&mut self) {
        let median_idx = self.values.len() / 2;
        let median = self.values[median_idx].clone();
        let mut left = Cluster {
            values: self.values.drain(..median_idx).collect(),
            children: vec![],
        };
        let mut right = Cluster {
            values: self.values.drain(1..).collect(),
            children: vec![],
        };
        self.values.push(median);
        self.children.push(Some(Box::new(left)));
        self.children.push(Some(Box::new(right)));
    }

    fn find_min(&self) -> Option<&T> {
        self.values.first()
    }

    fn remove_min(&mut self) -> Option<T> {
        self.values.pop()
    }
}