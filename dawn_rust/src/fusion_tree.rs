use std::rc::Rc;

enum FusionTree<T> {
    Leaf(T),
    Node(Rc<FusionTree<T>>, Rc<FusionTree<T>>),
}

impl<T: Copy + std::ops::Add<Output = T> + Default> FusionTree<T> {
    fn empty() -> FusionTree<T> {
        FusionTree::Leaf(T::default())
    }

    fn is_empty(&self) -> bool {
        match self {
            FusionTree::Leaf(x) => *x == T::default(),
            _ => false,
        }
    }

    fn insert(&self, x: T) -> FusionTree<T> {
        match self {
            FusionTree::Leaf(y) if *y == T::default() => FusionTree::Leaf(x),
            FusionTree::Leaf(y) => {
                let left = Rc::new(FusionTree::Leaf(x));
                let right = Rc::new(FusionTree::Leaf(*y));
                FusionTree::Node(left, right)
            }
            FusionTree::Node(left, right) => {
                let left = Rc::new(left.insert(x));
                FusionTree::Node(left, right.clone())
            }
        }
    }

    fn find(&self, x: T) -> T {
        match self {
            FusionTree::Leaf(y) => *y,
            FusionTree::Node(left, right) => left.find(x) + right.find(x),
        }
    }
}

fn union<T: Copy + std::ops::Add<Output = T> + Default>(
    left: &FusionTree<T>,
    right: &FusionTree<T>,
) -> FusionTree<T> {
    match (left, right) {
        (FusionTree::Leaf(0), t) | (t, FusionTree::Leaf(0)) => t.clone(),
        (FusionTree::Leaf(x), FusionTree::Leaf(y)) => {
            let left = Rc::new(FusionTree::Leaf(*x));
            let right = Rc::new(FusionTree::Leaf(*y));
            FusionTree::Node(left, right)
        }
        (FusionTree::Node(ll, lr), FusionTree::Node(rl, rr)) => {
            let new_left = union(&*ll, &*rl);
            let new_right = union(&*lr, &*rr);
            FusionTree::Node(Rc::new(new_left), Rc::new(new_right))
        }
    }
}