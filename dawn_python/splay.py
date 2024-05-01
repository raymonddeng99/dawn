class Node:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

class SplayTree:
    def __init__(self):
        self.root = None

    def splay(self, node):
        if not node:
            return None

        while True:
            parent = self.get_parent(self.root, node)
            if not parent:
                break

            grandparent = self.get_parent(self.root, parent)
            if not grandparent:
                self.rotate(self.root, parent, node)
            elif self.is_left_child(parent, node):
                self.rotate(grandparent, parent, node)
            else:
                self.rotate(grandparent, node, parent)

        return node

    def get_parent(self, root, node):
        if not root or root == node:
            return None

        if root.left == node:
            return root
        if root.right == node:
            return root

        parent = self.get_parent(root.left, node)
        if not parent:
            parent = self.get_parent(root.right, node)

        return parent

    def is_left_child(self, parent, node):
        return parent.left == node

    def rotate(self, root, parent, node):
        if self.is_left_child(parent, node):
            parent.left = node.right
            node.right = parent
        else:
            parent.right = node.left
            node.left = parent

        if root == parent:
            self.root = node
        elif self.is_left_child(root, parent):
            root.left = node
        else:
            root.right = node

    def insert(self, value):
        node = Node(value)
        if not self.root:
            self.root = node
            return

        self.splay(self.root)
        if value < self.root.value:
            node.left = self.root.left
            node.right = self.root
            self.root.left = None
        else:
            node.right = self.root.right
            node.left = self.root
            self.root.right = None
        self.root = node

    def contains(self, value):
        if not self.root:
            return False

        self.splay(self.root)
        if self.root.value == value:
            return True
        return False

    def remove(self, value):
        if not self.contains(value):
            return

        self.splay(self.root)
        if not self.root.left:
            self.root = self.root.right
        else:
            max_node = self.root.left
            while max_node.right:
                max_node = max_node.right
            self.splay(max_node)
            max_node.right = self.root.right
            self.root = max_node