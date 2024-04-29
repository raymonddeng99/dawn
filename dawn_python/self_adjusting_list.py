class Node:
    def __init__(self, data):
        self.prev = None
        self.next = None
        self.data = data

# Sleator-Tarjan one-finger model
class SelfAdjustingList:
    def __init__(self):
        self.head = None
        self.tail = None
        self.finger = None

    def insert_front(self, data):
        new_node = Node(data)
        if self.head is None:
            self.head = new_node
            self.tail = new_node
            self.finger = new_node
        else:
            new_node.next = self.head
            self.head.prev = new_node
            self.head = new_node
            self.finger = new_node

    def traverse_from_finger(self, target):
        node = self.finger
        while node:
            if node.data == target:
                return node
            node = node.next
        self.finger = self.tail
        return None

    def find(self, target):
        if self.finger:
            found = self.traverse_from_finger(target)
            if found:
                return found
        if self.head:
            return self.traverse_from_finger(target)
        return None


# Constant finger model
class ConstList:
    def __init__(self):
        self.head = None
        self.tail = None
        self.finger = None

    def insert_front(self, data):
        new_node = Node(data)
        if self.head is None:
            self.head = new_node
            self.tail = new_node
            self.finger = new_node
        else:
            new_node.next = self.head
            self.head.prev = new_node
            self.head = new_node
            self.finger = new_node

    def traverse_from_finger(self, target):
        node = self.finger
        while node:
            if node.data == target:
                return node
            node = node.next
        self.finger = self.tail
        return None

    def find(self, target):
        if self.finger:
            found = self.traverse_from_finger(target)
            if found:
                return found
        if self.head:
            return self.traverse_from_finger(target)
        return None