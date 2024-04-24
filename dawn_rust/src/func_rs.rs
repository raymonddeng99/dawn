use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Color {
    Red,
    Black,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum RBTree<T: Ord> {
    Empty,
    Node {
        color: Color,
        left: Box<RBTree<T>>,
        data: T,
        right: Box<RBTree<T>>,
    },
}

impl<T: Ord> RBTree<T> {
    fn balance(color: Color, a: RBTree<T>, x: T, b: RBTree<T>) -> RBTree<T> {
        match (color, a, b) {
            (
                Color::Black,
                Node {
                    color: Color::Red,
                    left: a,
                    data: y,
                    right: b,
                },
                z,
                d,
            ) if matches!(b, Node { color: Color::Red, .. }) => RBTree::Node {
                color: Color::Red,
                left: Box::new(RBTree::Node {
                    color: Color::Black,
                    left: a,
                    data: y,
                    right: b.left.unwrap_or_else(RBTree::Empty),
                }),
                data: b.data.unwrap(),
                right: Box::new(RBTree::Node {
                    color: Color::Black,
                    left: b.right.unwrap_or_else(RBTree::Empty),
                    data: z,
                    right: d,
                }),
            },
            (
                Color::Black,
                a,
                z,
                Node {
                    color: Color::Red,
                    left: b,
                    data: y,
                    right: c,
                },
            ) => RBTree::Node {
                color: Color::Red,
                left: Box::new(RBTree::Node {
                    color: Color::Black,
                    left: a,
                    data: x,
                    right: b,
                }),
                data: y,
                right: Box::new(RBTree::Node {
                    color: Color::Black,
                    left: c,
                    data: z,
                    right: RBTree::Empty,
                }),
            },
            (color, a, x, b) => RBTree::Node {
                color,
                left: Box::new(a),
                data: x,
                right: Box::new(b),
            },
        }
    }

    fn insert(&self, x: T) -> RBTree<T> {
        match self {
            RBTree::Empty => RBTree::Node {
                color: Color::Red,
                left: Box::new(RBTree::Empty),
                data: x,
                right: Box::new(RBTree::Empty),
            },
            RBTree::Node {
                color,
                left,
                data: y,
                right,
            } => match x.cmp(y) {
                Ordering::Less => RBTree::balance(
                    color.clone(),
                    left.insert(x),
                    y.clone(),
                    right.clone(),
                ),
                Ordering::Greater => RBTree::balance(
                    color.clone(),
                    left.clone(),
                    y.clone(),
                    right.insert(x),
                ),
                Ordering::Equal => RBTree::Node {
                    color: color.clone(),
                    left: left.clone(),
                    data: y.clone(),
                    right: right.clone(),
                },
            },
        }
    }
}