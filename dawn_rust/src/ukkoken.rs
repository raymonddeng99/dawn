use std::collections::HashMap;

// On-line construction of suffix trees, Ukkoken '95

struct Node {
    start: usize,
    len: usize,
    children: HashMap<char, Node>,
    suffix_link: Option<Box<Node>>,
    parent: Option<Box<Node>>,
}

fn create_node(start: usize, len: usize, parent: Option<Box<Node>>) -> Box<Node> {
    Box::new(Node {
        start,
        len,
        children: HashMap::new(),
        suffix_link: None,
        parent,
    })
}

fn split_node(node: &mut Node, start: usize, len: usize, node_label: char) -> Box<Node> {
    let new_node = create_node(start, len, Some(Box::new((*node).clone())));
    let remaining_len = node.len - len - 1;
    node.len = len;
    let child = create_node(node.start + len + 1, remaining_len, Some(new_node.clone()));
    node.children.insert(node_label, child);
    new_node.suffix_link = node.suffix_link.clone();
    node.suffix_link = Some(new_node.clone());
    new_node
}

fn find_node(root: &Node, start: usize, len: usize) -> Option<&Node> {
    if len == 0 {
        Some(root)
    } else {
        match root.children.get(&root.start[start]) {
            Some(child) => {
                let child_len = child.len;
                if child_len >= len {
                    Some(child)
                } else {
                    find_node(child, child.start + child_len + 1, len - child_len - 1)
                }
            }
            None => None,
        }
    }
}

fn follow_links(node: &Node) -> &Node {
    match &node.suffix_link {
        Some(link) => follow_links(link),
        None => node,
    }
}

fn update_tree(s: &str, i: usize, root: &mut Box<Node>) {
    let len = s.len();
    let mut node = follow_links(root);
    let mut remaining = len - i;
    if remaining > 0 {
        let leaf = create_node(i, remaining, Some((*node).clone()));
        let node_label = s.as_bytes()[i] as char;
        if let Some(child) = node.children.get(&node_label) {
            let start = child.start;
            let len = child.len;
            if let Some(found_node) = find_node(root, start, len) {
                if &s[start..start + len] == &s[i..i + len] {
                    update_tree(s, i + len + 1, &mut Box::new((*child).clone()));
                } else {
                    let new_node = split_node(&mut **found_node, start, len, node_label);
                    let leaf = create_node(i, remaining, Some(new_node.clone()));
                    new_node.children.insert(node_label, leaf);
                }
            } else {
                node.children.insert(node_label, leaf);
            }
        } else {
            node.children.insert(node_label, leaf);
        }
    }
}

fn build_suffix_tree(s: &str) -> Box<Node> {
    let mut root = create_node(0, 0, None);
    for i in 0..s.len() {
        update_tree(s, i, &mut root);
    }
    root
}