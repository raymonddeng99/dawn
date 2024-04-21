// On RAM priority queues, Thorup '96 

use std::cmp::Ordering;

enum VEBTree<T>
where
    T: Ord,
{
    Leaf(T),
    Node(Box<VEBTree<T>>, Box<VEBTree<T>>),
}

impl<T> VEBTree<T>
where
    T: Ord,
{
    fn min(&self) -> Option<&T> {
        match self {
            VEBTree::Leaf(x) => Some(x),
            VEBTree::Node(l, r) => match (l.min(), r.min()) {
                (Some(x), Some(y)) => Some(x.min(y)),
                (Some(x), None) => Some(x),
                (None, Some(y)) => Some(y),
                (None, None) => None,
            },
        }
    }

    fn insert(&mut self, x: T) {
        match self {
            VEBTree::Leaf(y) => {
                if x < *y {
                    *self = VEBTree::Node(Box::new(VEBTree::Leaf(x)), Box::new(VEBTree::Leaf(*y)));
                } else {
                    *self = VEBTree::Node(Box::new(VEBTree::Leaf(*y)), Box::new(VEBTree::Leaf(x)));
                }
            }
            VEBTree::Node(l, r) => {
                if let Some(min_l) = l.min() {
                    if x < *min_l {
                        l.insert(x);
                    } else {
                        r.insert(x);
                    }
                } else {
                    l.insert(x);
                }
            }
        }
    }
}
