#include <iostream>
#include <vector>
#include <memory>

template <typename T>
class YFastTrie {
private:
    struct Node {
        T val;
        std::unique_ptr<Node> left, right;
        Node(T v) : val(v), left(nullptr), right(nullptr) {}
    };

    std::unique_ptr<Node> root;

    std::unique_ptr<Node> add(std::unique_ptr<Node> node, T x, const std::vector<T>& xs) {
        if (!node) {
            if (xs.empty()) {
                return std::make_unique<Node>(x);
            }
            T y = xs[0];
            node = std::make_unique<Node>(y);
            node->left = add(nullptr, x, std::vector<T>(xs.begin() + 1, xs.end()));
            return node;
        }
        if (x == node->val) {
            return node;
        }
        if (x < node->val) {
            node->left = add(std::move(node->left), x, xs);
        } else {
            node->right = add(std::move(node->right), x, std::vector<T>(xs.begin() + 1, xs.end()));
        }
        return node;
    }

    T* lookup(const std::unique_ptr<Node>& node, const std::vector<T>& xs) const {
        if (!node) {
            return nullptr;
        }
        if (xs.empty()) {
            return &node->val;
        }
        T y = xs[0];
        if (y < node->val) {
            return lookup(node->left, xs);
        }
        return lookup(node->right, std::vector<T>(xs.begin() + 1, xs.end()));
    }

public:
    YFastTrie() : root(nullptr) {}

    void add(T x, const std::vector<T>& xs) {
        root = add(std::move(root), x, xs);
    }

    T* lookup(const std::vector<T>& xs) const {
        return lookup(root, xs);
    }
};