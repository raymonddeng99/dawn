class YFastTrie:
    class Node:
        def __init__(self, val, left=None, right=None):
            self.val = val
            self.left = left
            self.right = right

    def __init__(self):
        self.root = None

    def add(self, x, xs):
        self.root = self._add(self.root, x, xs)

    def _add(self, node, x, xs):
        if node is None:
            if not xs:
                return YFastTrie.Node(x)
            y = xs[0]
            new_node = YFastTrie.Node(y)
            new_node.left = self._add(None, x, xs[1:])
            return new_node
        if x == node.val:
            return node
        if x < node.val:
            node.left = self._add(node.left, x, xs)
        else:
            node.right = self._add(node.right, x, xs[1:])
        return node

    def lookup(self, xs):
        return self._lookup(self.root, xs)

    def _lookup(self, node, xs):
        if node is None:
            return None
        if not xs:
            return node.val
        y = xs[0]
        if y < node.val:
            return self._lookup(node.left, xs)
        else:
            return self._lookup(node.right, xs[1:])