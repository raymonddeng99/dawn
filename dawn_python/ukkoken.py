
# On-line construction of suffix trees, Ukkoken '95

class Node:
    def __init__(self, start, length, parent=None):
        self.start = start
        self.length = length
        self.children = {}
        self.suffix_link = None
        self.parent = parent

def create_node(start, length, parent):
    return Node(start, length, parent)

def split_node(node, start, length, node_label):
    new_node = create_node(start, length, node.parent)
    remaining_length = node.length - length - 1
    node.length = length
    child = create_node(node.start + length + 1, remaining_length, new_node)
    node.children[node_label] = child
    new_node.suffix_link = node.suffix_link
    node.suffix_link = new_node
    return new_node

def find_node(root, start, length, s):
    if length == 0:
        return root
    child = root.children.get(s[start])
    if child:
        child_length = child.length
        if child_length >= length:
            return child
        return find_node(child, child.start + child_length + 1, length - child_length - 1, s)
    return None

def follow_links(node):
    if node.suffix_link:
        return follow_links(node.suffix_link)
    return node

def update_tree(s, i, root):
    length = len(s)
    node = follow_links(root)
    remaining = length - i
    if remaining > 0:
        leaf = create_node(i, remaining, node)
        node_label = s[i]
        child = node.children.get(node_label)
        if child:
            start = child.start
            length = child.length
            found_node = find_node(root, start, length, s)
            if found_node and s[start:start + length] == s[i:i + length]:
                update_tree(s, i + length + 1, child)
            elif found_node:
                new_node = split_node(found_node, start, length, node_label)
                leaf = create_node(i, remaining, new_node)
                new_node.children[node_label] = leaf
        else:
            node.children[node_label] = leaf

def build_suffix_tree(s):
    root = create_node(0, 0, None)
    for i in range(len(s)):
        update_tree(s, i, root)
    return root