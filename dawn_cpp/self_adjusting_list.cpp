#include <iostream>
#include <memory>

template <typename T>
struct Node {
    std::shared_ptr<Node<T>> prev;
    std::shared_ptr<Node<T>> next;
    T data;
};

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