use std::cmp::Ordering;
use std::mem;

struct Node<T: Ord> {
    val: T,
    left: Option<Box<Node<T>>>,
    right: Option<Box<Node<T>>>,
}

impl<T: Ord> Node<T> {
    fn new(val: T) -> Self {
        Node {
            val,
            left: None,
            right: None,
        }
    }

    fn splay(&mut self) {
        let mut current = self;
        loop {
            if let Some(parent) = Self::get_parent(current) {
                if let Some(grandparent) = Self::get_parent(parent) {
                    let (new_gp, new_p, new_c) = Self::rotate_around_grandparent(grandparent, parent, current);
                    *grandparent = new_gp;
                    *parent = new_p;
                    current = new_c;
                } else {
                    let (_, new_c) = Self::rotate_around_parent(parent, current);
                    current = new_c;
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn get_parent<'a>(node: &'a mut Node<T>) -> Option<&'a mut Node<T>> {
        let mut parent = None;
        let mut current = node;
        while let Some(ref mut p) = parent {
            if &mut **current == p.left.as_mut().unwrap() {
                parent = Some(p);
                current = p;
            } else if &mut **current == p.right.as_mut().unwrap() {
                parent = Some(p);
                current = p;
            } else {
                break;
            }
        }
        parent
    }

    fn rotate_around_parent<'a>(
        parent: &'a mut Node<T>,
        current: &'a mut Node<T>,
    ) -> (&'a mut Node<T>, &'a mut Node<T>) {
        if parent.left.as_mut().unwrap() as *mut Node<T> == current as *mut Node<T> {
            let new_parent = mem::replace(&mut parent.left, None).unwrap();
            parent.left = new_parent.right;
            new_parent.right = Some(parent.clone());
            (parent, &mut *new_parent)
        } else {
            let new_parent = mem::replace(&mut parent.right, None).unwrap();
            parent.right = new_parent.left;
            new_parent.left = Some(parent.clone());
            (parent, &mut *new_parent)
        }
    }

    fn rotate_around_grandparent<'a>(
        grandparent: &'a mut Node<T>,
        parent: &'a mut Node<T>,
        current: &'a mut Node<T>,
    ) -> (&'a mut Node<T>, &'a mut Node<T>, &'a mut Node<T>) {
        if grandparent.left.as_mut().unwrap() as *mut Node<T> == parent as *mut Node<T> {
            if parent.left.as_mut().unwrap() as *mut Node<T> == current as *mut Node<T> {
                let (new_gp, new_p) = Self::rotate_around_parent(grandparent, parent);
                let (new_p, new_c) = Self::rotate_around_parent(new_p, current);
                (new_gp, new_p, new_c)
            } else {
                let (new_p, new_c) = Self::rotate_around_parent(parent, current);
                let (new_gp, new_p) = Self::rotate_around_parent(grandparent, new_p);
                (new_gp, new_p, new_c)
            }
        } else {
            if parent.right.as_mut().unwrap() as *mut Node<T> == current as *mut Node<T> {
                let (new_p, new_c) = Self::rotate_around_parent(parent, current);
                let (new_gp, new_p) = Self::rotate_around_parent(grandparent, new_p);
                (new_gp, new_p, new_c)
            } else {
                let (new_gp, new_p) = Self::rotate_around_parent(grandparent, parent);
                let (new_p, new_c) = Self::rotate_around_parent(new_p, current);
                (new_gp, new_p, new_c)
            }
        }
    }
}

pub struct SplayTree<T: Ord> {
    root: Option<Box<Node<T>>>,
}

impl<T: Ord> SplayTree<T> {
    pub fn new() -> Self {
        SplayTree { root: None }
    }

    pub fn insert(&mut self, val: T) {
        let mut new_root = Some(Box::new(Node::new(val)));
        if let Some(root) = self.root.take() {
            new_root.as_mut().unwrap().left = Some(root);
            new_root.as_mut().unwrap().splay();
        }
        self.root = new_root;
    }

    pub fn remove(&mut self, val: &T) {
        if let Some(root) = self.root.as_mut() {
            root.splay();
            if root.val == *val {
                if root.left.is_none() {
                    self.root = root.right.take();
                } else {
                    let mut new_root = root.left.take().unwrap();
                    new_root.splay();
                    new_root.right = root.right.take();
                    self.root = Some(new_root);
                }
            } else if val < &root.val {
                root.left.as_mut().unwrap().splay();
                root.left.as_mut().unwrap().remove(val);
            } else {
                root.right.as_mut().unwrap().splay();
                root.right.as_mut().unwrap().remove(val);
            }
        }
    }

    pub fn contains(&mut self, val: &T) -> bool {
        if let Some(root) = self.root.as_mut() {
            root.splay();
            root.val == *val
        } else {
            false
        }
    }
}