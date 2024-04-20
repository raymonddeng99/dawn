use std::collections::HashMap;

type Node = HashMap<char, Node>;

fn build(keys: &[&str]) -> Node {
    let mut root = Node::new();
    for key in keys {
        let mut curr = &mut root;
        for c in key.chars() {
            curr = curr.entry(c).or_insert(Node::new());
        }
    }
    root
}

fn lookup(trie: &Node, key: &str) -> bool {
    let mut curr = trie;
    for c in key.chars() {
        match curr.get(&c) {
            Some(next) => curr = next,
            None => return false,
        }
    }
    true
}