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


# Order by Next Request strategy
class ONBRList:
    def __init__(self):
        self.head = None
        self.tail = None

    def insert(self, value):
        new_node = Node(value)
        if self.head is None:
            self.head = new_node
            self.tail = new_node
        else:
            new_node.next = self.head
            self.head.prev = new_node
            self.head = new_node

    def remove_head(self):
        if self.head is None:
            return None
        value = self.head.value
        if self.head == self.tail:
            self.head = None
            self.tail = None
        else:
            self.head = self.head.next
            self.head.prev = None
        return value

    def access(self, value):
        current = self.head
        while current is not None:
            if current.value == value:
                if current != self.head:
                    self._remove(current)
                    self._move_to_front(current)
                return
            current = current.next

    def _remove(self, node):
        if node.prev is not None:
            node.prev.next = node.next
        else:
            self.head = node.next
        if node.next is not None:
            node.next.prev = node.prev
        else:
            self.tail = node.prev

    def _move_to_front(self, node):
        node.next = self.head
        self.head.prev = node
        self.head = node
        node.prev = None