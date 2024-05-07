use std::cmp::Ordering;

enum Node<T: Ord> {
    Leaf,
    Node(T, Box<Node<T>>, Box<Node<T>>),
}

pub struct Trie<T: Ord> {
    root: Node<T>,
}

impl<T: Ord> Trie<T> {
    pub fn new() -> Self {
        Trie { root: Node::Leaf }
    }

    pub fn add(&mut self, x: T, xs: &[T]) {
        self.root = add(self.root, x, xs);
    }

    pub fn lookup(&self, xs: &[T]) -> Option<&T> {
        lookup(&self.root, xs)
    }
}

fn add<T: Ord>(node: Node<T>, x: T, xs: &[T]) -> Node<T> {
    match node {
        Node::Leaf => {
            if xs.is_empty() {
                Node::Node(x, Box::new(Node::Leaf), Box::new(Node::Leaf))
            } else {
                let y = xs[0];
                let mut new_node = Node::Node(y, Box::new(Node::Leaf), Box::new(Node::Leaf));
                *new_node.0_mut().left = add(new_node.0, x, &xs[1..]);
                new_node
            }
        }
        Node::Node(y, left, right) => {
            match x.cmp(&y) {
                Ordering::Less => Node::Node(y, Box::new(add(*left, x, xs)), right),
                Ordering::Greater => Node::Node(y, left, Box::new(add(*right, x, &xs[1..]))),
                Ordering::Equal => Node::Node(y, left, right),
            }
        }
    }
}

fn lookup<'a, T: Ord>(node: &'a Node<T>, xs: &[T]) -> Option<&'a T> {
    match node {
        Node::Leaf => None,
        Node::Node(y, left, right) => {
            if xs.is_empty() {
                Some(y)
            } else {
                let x = xs[0];
                match x.cmp(y) {
                    Ordering::Less => lookup(&*left, xs),
                    Ordering::Greater => lookup(&*right, &xs[1..]),
                    Ordering::Equal => lookup(&*right, &xs[1..]),
                }
            }
        }
    }
}