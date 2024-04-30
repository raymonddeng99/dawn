#include <iostream>
#include <memory>
#include <optional>

template <typename T>
struct Node {
    std::shared_ptr<Node<T>> prev;
    std::shared_ptr<Node<T>> next;
    T data;
};

// Sleator-Tarjan one-finger model
template <typename T>
class SelfAdjustingList {
private:
    std::shared_ptr<Node<T>> head;
    std::shared_ptr<Node<T>> tail;
    std::shared_ptr<Node<T>> finger;

    std::shared_ptr<Node<T>> traverseFromFinger(const T& target, const std::shared_ptr<Node<T>>& node) {
        if (node->data == target) {
            return node;
        }
        if (node->next == nullptr) {
            finger = tail;
            return nullptr;
        }
        auto found = traverseFromFinger(target, node->next);
        if (found != nullptr) {
            return found;
        }
        finger = node;
        return node;
    }

public:
    SelfAdjustingList() : head(nullptr), tail(nullptr), finger(nullptr) {}

    void insertFront(const T& data) {
        auto newNode = std::make_shared<Node<T>>();
        newNode->data = data;
        if (head == nullptr) {
            head = newNode;
            tail = newNode;
            finger = newNode;
        } else {
            newNode->next = head;
            head->prev = newNode;
            head = newNode;
            finger = newNode;
        }
    }

    std::shared_ptr<Node<T>> find(const T& target) {
        if (finger == nullptr) {
            if (head == nullptr) {
                return nullptr;
            }
            return traverseFromFinger(target, head);
        }
        auto found = traverseFromFinger(target, finger);
        if (found == nullptr) {
            if (head == nullptr) {
                return nullptr;
            }
            return traverseFromFinger(target, head);
        }
        return found;
    }
};



// Constant finger model
template <typename T>
class ConstList {
private:
    std::shared_ptr<Node<T>> head;
    std::shared_ptr<Node<T>> tail;
    std::shared_ptr<Node<T>> finger;

    std::shared_ptr<Node<T>> traverseFromFinger(const T& target, const std::shared_ptr<Node<T>>& node) {
        if (node->data == target) {
            return node;
        }
        if (node->next == nullptr) {
            finger = tail;
            return nullptr;
        }
        auto found = traverseFromFinger(target, node->next);
        if (found != nullptr) {
            return found;
        }
        finger = node;
        return node;
    }

public:
    ConstList() : head(nullptr), tail(nullptr), finger(nullptr) {}

    void insertFront(const T& data) {
        auto newNode = std::make_shared<Node<T>>();
        newNode->data = data;
        if (head == nullptr) {
            head = newNode;
            tail = newNode;
            finger = newNode;
        } else {
            newNode->next = head;
            head->prev = newNode;
            head = newNode;
            finger = newNode;
        }
    }

    std::shared_ptr<Node<T>> find(const T& target) {
        if (finger == nullptr) {
            if (head == nullptr) {
                return nullptr;
            }
            return traverseFromFinger(target, head);
        }
        auto found = traverseFromFinger(target, finger);
        if (found == nullptr) {
            if (head == nullptr) {
                return nullptr;
            }
            return traverseFromFinger(target, head);
        }
        return found;
    }
};


// Order by Next Request strategy
template <typename T>
class ONBRList {
private:
    Node<T>* head;
    Node<T>* tail;

public:
    ONBRList() : head(nullptr), tail(nullptr) {}

    ~ONBRList() {
        Node<T>* curr = head;
        while (curr != nullptr) {
            Node<T>* next = curr->next;
            delete curr;
            curr = next;
        }
    }

    void insert(const T& value) {
        Node<T>* newNode = new Node<T>(value);
        if (head == nullptr) {
            head = newNode;
            tail = newNode;
        } else {
            newNode->next = head;
            head->prev = newNode;
            head = newNode;
        }
    }

    std::optional<T> remove_head() {
        if (head == nullptr) {
            return std::nullopt;
        }
        T value = head->value;
        Node<T>* oldHead = head;
        head = head->next;
        if (head != nullptr) {
            head->prev = nullptr;
        } else {
            tail = nullptr;
        }
        delete oldHead;
        return value;
    }

    void access(const T& value) {
        Node<T>* curr = head;
        Node<T>* prev = nullptr;
        while (curr != nullptr) {
            if (curr->value == value) {
                if (prev == nullptr) {
                    // Value is already at the head
                    return;
                }
                prev->next = curr->next;
                if (curr->next != nullptr) {
                    curr->next->prev = prev;
                } else {
                    tail = prev;
                }
                curr->next = head;
                head->prev = curr;
                head = curr;
                curr->prev = nullptr;
                return;
            }
            prev = curr;
            curr = curr->next;
        }
    }
};