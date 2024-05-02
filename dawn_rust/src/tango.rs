use std::cmp::Ordering;

struct AuxTree<T: Ord> {
    left: Option<Box<AuxTree<T>>>,
    key: T,
    right: Option<Box<AuxTree<T>>>,
}

impl<T: Ord> AuxTree<T> {
    fn new(key: T) -> Self {
        AuxTree {
            left: None,
            key,
            right: None,
        }
    }

    fn insert(&mut self, key: T) {
        match key.cmp(&self.key) {
            Ordering::Less => {
                if let Some(ref mut left) = self.left {
                    left.insert(key);
                } else {
                    self.left = Some(Box::new(AuxTree::new(key)));
                }
            }
            Ordering::Greater => {
                if let Some(ref mut right) = self.right {
                    right.insert(key);
                } else {
                    self.right = Some(Box::new(AuxTree::new(key)));
                }
            }
            Ordering::Equal => {}
        }
    }

    fn find(&self, key: &T) -> bool {
        match key.cmp(&self.key) {
            Ordering::Less => self.left.as_ref().map_or(false, |left| left.find(key)),
            Ordering::Greater => self.right.as_ref().map_or(false, |right| right.find(key)),
            Ordering::Equal => true,
        }
    }
}

struct TangoTree<T: Ord> {
    aux_trees: Vec<AuxTree<T>>,
}

impl<T: Ord> TangoTree<T> {
    fn new() -> Self {
        TangoTree {
            aux_trees: vec![AuxTree::new(T::min_value())],
        }
    }

    fn find(&self, key: &T) -> bool {
        self.aux_trees.iter().any(|aux| aux.find(key))
    }

    fn update(&mut self, key: T) {
        let mut left = AuxTree::new(T::min_value());
        let mut right = AuxTree::new(T::max_value());

        for aux in self.aux_trees.drain(..) {
            if aux.key < key {
                left.insert(aux.key);
            } else {
                right.insert(aux.key);
            }
        }

        self.aux_trees = vec![left, AuxTree::new(key), right];
    }
}