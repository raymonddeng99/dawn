use std::mem;
use std::collections::BinaryHeap;
use std::rc::Rc;

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


// Partially retroactive priority queue
#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Item<T> {
    value: T,
    priority: usize,
}

pub struct PriorityQueue<T> {
    data: BinaryHeap<Item<T>>,
    time: usize,
}

impl<T: Ord> PriorityQueue<T> {
    pub fn new() -> Self {
        PriorityQueue {
            data: BinaryHeap::new(),
            time: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn insert(&mut self, x: T) {
        self.time += 1;
        self.data.push(Item {
            value: x,
            priority: self.time,
        });
    }

    pub fn find_min(&self) -> Option<&T> {
        self.data.peek().map(|item| &item.value)
    }

    pub fn delete_min(&mut self) -> Option<T> {
        self.data.pop().map(|item| item.value)
    }

    pub fn retroactive_update(&mut self, t: usize, x: T) {
        let mut new_data = BinaryHeap::new();
        for item in self.data.iter() {
            if item.priority <= t {
                new_data.push(Item {
                    value: x.clone(),
                    priority: item.priority,
                });
            } else {
                new_data.push(item.clone());
            }
        }
        self.data = new_data;
    }
}


// Simple Confluently Persistent Catenable Lists, Tarjan et al
type Link<T> = Option<Rc<Node<T>>>;

struct Node<T> {
    value: T,
    left: Link<T>,
    right: Link<T>,
}

pub struct PersistentList<T> {
    left: Link<T>,
    right: Link<T>,
}

impl<T> PersistentList<T> {
    pub fn new() -> Self {
        PersistentList { left: None, right: None }
    }

    pub fn is_empty(&self) -> bool {
        self.left.is_none() && self.right.is_none()
    }

    pub fn singleton(value: T) -> Self {
        let node = Rc::new(Node {
            value,
            left: None,
            right: None,
        });
        PersistentList {
            left: Some(node.clone()),
            right: Some(node),
        }
    }

    pub fn cons(&self, value: T) -> Self {
        let new_node = Rc::new(Node {
            value,
            left: None,
            right: self.left.clone(),
        });
        PersistentList {
            left: Some(new_node),
            right: self.right.clone(),
        }
    }

    pub fn head(&self) -> Option<&T> {
        self.left.as_ref().map(|node| &node.value)
    }

    pub fn tail(&self) -> Self {
        if self.left.as_ref() == self.right.as_ref() {
            PersistentList::new()
        } else {
            let right_node = self.right.as_ref().unwrap();
            PersistentList {
                left: right_node.left.clone(),
                right: right_node.right.clone(),
            }
        }
    }

    pub fn catenate(&self, other: &Self) -> Self {
        if let Some(left_node) = &self.right {
            if let Some(right_node) = &other.left {
                if Rc::ptr_eq(left_node, right_node) {
                    return PersistentList {
                        left: self.left.clone(),
                        right: other.right.clone(),
                    };
                }
            }
        }
        let new_node = Rc::new(Node {
            value: Default::default(),
            left: self.right.clone(),
            right: other.left.clone(),
        });
        PersistentList {
            left: self.left.clone(),
            right: other.right.clone(),
        }
    }
}