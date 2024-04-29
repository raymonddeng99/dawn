use std::rc::Rc;
use std::cell::RefCell;

type Link<T> = Option<Rc<RefCell<Node<T>>>>;

struct Node<T> {
    prev: Link<T>,
    next: Link<T>,
    data: T,
}

// Sleator-Tarjan one-finger model
struct SelfAdjustingList<T> {
    head: Link<T>,
    tail: Link<T>,
    finger: Link<T>,
}

impl<T: PartialEq + Clone> SelfAdjustingList<T> {
    fn new() -> Self {
        SelfAdjustingList {
            head: None,
            tail: None,
            finger: None,
        }
    }

    fn insert_front(&mut self, data: T) {
        let new_node = Rc::new(RefCell::new(Node {
            prev: None,
            next: self.head.clone(),
            data,
        }));

        match self.head.take() {
            Some(old_head) => {
                old_head.borrow_mut().prev = Some(new_node.clone());
            }
            None => {
                self.tail = Some(new_node.clone());
            }
        }

        self.head = Some(new_node.clone());
        self.finger = Some(new_node);
    }

    fn traverse_from_finger<F>(&mut self, mut target_fn: F) -> Link<T>
    where
        F: FnMut(&T) -> bool,
    {
        let mut current = self.finger.clone();

        while let Some(node) = current {
            let node_ref = node.borrow();
            if target_fn(&node_ref.data) {
                return Some(node);
            }

            current = node_ref.next.clone();
        }

        self.finger = self.tail.clone();
        None
    }

    fn find(&mut self, target: T) -> Link<T> {
        if let Some(finger) = self.finger.clone() {
            if let Some(found) = self.traverse_from_finger(|data| data == &target) {
                return Some(found);
            }
        }

        self.traverse_from_finger(|data| data == &target)
    }
}

// Constant finger model
struct ConstList<T> {
    head: Link<T>,
    tail: Link<T>,
    finger: Link<T>,
}

impl<T: PartialEq + Clone> ConstList<T> {
    fn new() -> Self {
        ConstList {
            head: None,
            tail: None,
            finger: None,
        }
    }

    fn insert_front(&mut self, data: T) {
        let new_node = Rc::new(RefCell::new(Node {
            prev: None,
            next: self.head.clone(),
            data,
        }));

        match self.head.take() {
            Some(old_head) => {
                old_head.borrow_mut().prev = Some(new_node.clone());
            }
            None => {
                self.tail = Some(new_node.clone());
            }
        }

        self.head = Some(new_node.clone());
        self.finger = Some(new_node);
    }

    fn traverse_from_finger<F>(&mut self, mut target_fn: F) -> Link<T>
    where
        F: FnMut(&T) -> bool,
    {
        let mut current = self.finger.clone();

        while let Some(node) = current {
            let node_ref = node.borrow();
            if target_fn(&node_ref.data) {
                return Some(node);
            }

            current = node_ref.next.clone();
        }

        self.finger = self.tail.clone();
        None
    }

    fn find(&mut self, target: T) -> Link<T> {
        if let Some(finger) = self.finger.clone() {
            if let Some(found) = self.traverse_from_finger(|data| data == &target) {
                return Some(found);
            }
        }

        self.traverse_from_finger(|data| data == &target)
    }
}