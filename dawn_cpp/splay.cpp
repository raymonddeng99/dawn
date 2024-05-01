#include <iostream>
#include <memory>

template <typename T>
struct Node {
    T value;
    std::unique_ptr<Node<T>> left, right;

    Node(T value) : value(value), left(nullptr), right(nullptr) {}
};

template <typename T>
class SplayTree {
private:
    std::unique_ptr<Node<T>> root;

    std::unique_ptr<Node<T>>& leftChild(std::unique_ptr<Node<T>>& node) {
        return node->left;
    }

    std::unique_ptr<Node<T>>& rightChild(std::unique_ptr<Node<T>>& node) {
        return node->right;
    }

    void splay(std::unique_ptr<Node<T>>& node) {
        if (!node)
            return;

        std::unique_ptr<Node<T>> parent, grandparent;
        while ((parent = getParent(root, node))) {
            grandparent = getParent(root, parent);
            if (!grandparent)
                rotate(root, parent, node);
            else if (isLeftChild(parent, node))
                rotate(grandparent, parent, node);
            else
                rotate(grandparent, node, parent);
        }
    }

    std::unique_ptr<Node<T>>* getParent(std::unique_ptr<Node<T>>& root, std::unique_ptr<Node<T>>& node) {
        if (!root || root.get() == node.get())
            return nullptr;
        if (leftChild(root) && leftChild(root).get() == node.get())
            return &root;
        if (rightChild(root) && rightChild(root).get() == node.get())
            return &root;
        std::unique_ptr<Node<T>>* parent = getParent(leftChild(root), node);
        if (!parent)
            parent = getParent(rightChild(root), node);
        return parent;
    }

    bool isLeftChild(std::unique_ptr<Node<T>>& parent, std::unique_ptr<Node<T>>& node) {
        return parent->left.get() == node.get();
    }

    void rotate(std::unique_ptr<Node<T>>& root, std::unique_ptr<Node<T>>& parent, std::unique_ptr<Node<T>>& node) {
        if (isLeftChild(parent, node)) {
            leftChild(root) = std::move(rightChild(*node));
            rightChild(*node) = std::move(parent);
        } else {
            rightChild(root) = std::move(leftChild(*node));
            leftChild(*node) = std::move(parent);
        }
        std::swap(parent, node);
    }

public:
    SplayTree() : root(nullptr) {}

    void insert(T value) {
        std::unique_ptr<Node<T>> newNode = std::make_unique<Node<T>>(value);
        if (!root) {
            root = std::move(newNode);
            return;
        }
        std::unique_ptr<Node<T>>* insertPos = &root;
        while (*insertPos) {
            if ((*insertPos)->value > value)
                insertPos = &leftChild(**insertPos);
            else
                insertPos = &rightChild(**insertPos);
        }
        *insertPos = std::move(newNode);
        splay(*insertPos);
    }

    bool contains(T value) {
        std::unique_ptr<Node<T>>* node = &root;
        while (*node) {
            if ((*node)->value == value) {
                splay(*node);
                return true;
            } else if ((*node)->value > value)
                node = &leftChild(**node);
            else
                node = &rightChild(**node);
        }
        return false;
    }

    void remove(T value) {
        if (!contains(value))
            return;
        if (!root->left) {
            root = std::move(root->right);
            return;
        }
        std::unique_ptr<Node<T>>* maxNode = &leftChild(*root);
        while (rightChild(**maxNode))
            maxNode = &rightChild(**maxNode);
        splay(*maxNode);
        rightChild(**maxNode) = std::move(root->right);
        root = std::move(*maxNode);
    }
};