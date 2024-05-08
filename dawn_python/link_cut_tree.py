class Node:
    def __init__(self, value):
        self.value = value
        self.parent = None
        self.left = None
        self.right = None
        self.rev = False
        self.sum = value

    def get_sum(self):
        return self.sum

    def update_sum(self):
        self.sum = self.value + (self.left.get_sum() if self.left else 0) + (self.right.get_sum() if self.right else 0)

    def push(self):
        if self.rev:
            self.rev = False
            self.left, self.right = self.right, self.left
            if self.left:
                self.left.rev = not self.left.rev
                self.left.push()
            if self.right:
                self.right.rev = not self.right.rev
                self.right.push()

    def make_root(self):
        self.push()
        if self.parent:
            if self.parent.left is self:
                self.parent.left = None
            else:
                self.parent.right = None
            self.parent.make_root()
            self.parent = None

    def splay(self):
        self.make_root()
        p = self.parent
        while p:
            p.make_root()
            if p.parent:
                p.parent.make_root()
                if (p.parent.left is p) == (p.left is self):
                    p.rotate()
                else:
                    self.rotate()
            self.rotate()
            p = self.parent
        self.update_sum()

    def rotate(self):
        p = self.parent
        pp = p.parent
        if pp:
            if pp.left is p:
                pp.left = self
            else:
                pp.right = self
        self.parent = pp
        if p.left is self:
            p.left = self.right
            self.right = p
        else:
            p.right = self.left
            self.left = p
        p.parent = self
        p.update_sum()
        self.update_sum()

    def access(self):
        self.splay()
        self.rev = False
        c = self.left
        while c:
            c.splay()
            c.rev = not c.rev
            c.update_sum()
            c.right = self
            self.parent = c
            c = c.left

    def link(self, other):
        self.access()
        other.access()
        other.parent = self
        other.update_sum()

    def cut(self):
        self.access()
        if self.left:
            self.left.parent = None
            self.left = None
            self.update_sum()

    def is_root(self):
        return self.parent is None

    def lca(self, other):
        other.access()
        other_sum = other.get_sum()
        self.access()
        z = other
        while z.get_sum() < other_sum or z.get_sum() < self.get_sum():
            z.access()
            z = z.parent
        return z

    def path_sum(self, other):
        z = self.lca(other)
        self.access()
        other.access()
        x_sum = self.get_sum()
        y_sum = other.get_sum()
        z_sum = z.get_sum()
        return x_sum + y_sum - 2 * z_sum