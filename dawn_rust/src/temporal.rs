use std::mem;

enum Node<T> {
    One(T),
    Two(T, T),
    Three(T, T, T),
    Four(T, T, T, T),
}

struct Deque<T> {
    front: Vec<Node<T>>,
    back: Vec<Node<T>>,
}

impl<T: Clone> Deque<T> {
    fn new() -> Self {
        Deque {
            front: Vec::new(),
            back: Vec::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.front.is_empty() && self.back.is_empty()
    }

    fn add_front(&mut self, x: T) {
        let mut new_front = Vec::new();
        for node in self.front.drain(..) {
            match node {
                Node::One(y) => {
                    new_front.push(Node::Four(y, y, y, y));
                    new_front.push(Node::One(x.clone()));
                }
                Node::Two(y, z) => {
                    new_front.push(Node::Three(y, y, z));
                    new_front.push(Node::Four(x.clone(), x.clone(), x.clone(), x.clone()));
                }
                Node::Three(y, z, w) => {
                    new_front.push(Node::Two(y, z));
                    new_front.push(Node::Two(z, w));
                    new_front.push(Node::Four(w, w, w, w));
                    new_front.push(Node::One(x.clone()));
                }
                Node::Four(y, z, w, v) => {
                    new_front.push(Node::One(y));
                    new_front.push(Node::One(z));
                    new_front.push(Node::One(w));
                    new_front.push(Node::One(v));
                    new_front.push(Node::One(x.clone()));
                }
            }
        }
        new_front.push(Node::One(x));
        mem::swap(&mut self.front, &mut new_front);
    }

    fn add_back(&mut self, x: T) {
        let mut new_back = Vec::new();
        for node in self.back.drain(..) {
            match node {
                Node::One(y) => {
                    new_back.push(Node::One(y));
                    new_back.push(Node::Four(x.clone(), x.clone(), x.clone(), x.clone()));
                }
                Node::Two(y, z) => {
                    new_back.push(Node::Three(y, z, z));
                    new_back.push(Node::Four(y, y, y, x.clone()));
                }
                Node::Three(y, z, w) => {
                    new_back.push(Node::Two(y, z));
                    new_back.push(Node::Two(w, w));
                    new_back.push(Node::Four(z, z, z, z));
                    new_back.push(Node::One(x.clone()));
                }
                Node::Four(y, z, w, v) => {
                    new_back.push(Node::One(y));
                    new_back.push(Node::One(z));
                    new_back.push(Node::One(w));
                    new_back.push(Node::One(v));
                    new_back.push(Node::One(x.clone()));
                }
            }
        }
        new_back.push(Node::One(x));
        mem::swap(&mut self.back, &mut new_back);
    }

    fn take_front(&mut self) -> Option<T> {
        if self.front.is_empty() {
            return None;
        }
        let node = self.front.remove(0);
        let (x, new_front) = match node {
            Node::One(x) => (x, Vec::new()),
            Node::Two(x, y) => (x, vec![Node::One(y)]),
            Node::Three(x, y, z) => (x, vec![Node::Two(y, z)]),
            Node::Four(w, x, y, z) => (w, vec![Node::Three(x, y, z)]),
        };
        self.front = self.back.drain(..).rev().collect();
        mem::swap(&mut self.front, &mut new_front);
        Some(x)
    }

    fn take_back(&mut self) -> Option<T> {
        if self.back.is_empty() {
            return None;
        }
        let node = self.back.pop().unwrap();
        let (x, new_back) = match node {
            Node::One(x) => (x, Vec::new()),
            Node::Two(x, y) => (y, vec![Node::One(x)]),
            Node::Three(x, y, z) => (z, vec![Node::Two(x, y)]),
            Node::Four(w, x, y, z) => (z, vec![Node::Three(w, x, y)]),
        };
        self.back = new_back;
        Some(x)
    }
}