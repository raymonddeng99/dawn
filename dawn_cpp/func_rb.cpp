#include <iostream>
#include <memory>

enum class Color { Red, Black };

template <typename T>
struct RBTree {
    Color color;
    std::unique_ptr<RBTree<T>> left, right;
    T data;

    RBTree(Color c, std::unique_ptr<RBTree<T>> l, T d, std::unique_ptr<RBTree<T>> r)
        : color(c), left(std::move(l)), data(std::move(d)), right(std::move(r)) {}
};

template <typename T>
std::unique_ptr<RBTree<T>> balance(Color color, std::unique_ptr<RBTree<T>> a, T x, std::unique_ptr<RBTree<T>> b) {
    if (color == Color::Black) {
        if (a && a->color == Color::Red && a->left && a->left->color == Color::Red) {
            return std::make_unique<RBTree<T>>(
                Color::Red,
                std::move(a->left->left),
                std::move(x),
                std::make_unique<RBTree<T>>(
                    Color::Black, std::move(a->right), std::move(a->data), std::move(b)));
        }
        if (a && a->color == Color::Red && a->right && a->right->color == Color::Red) {
            return std::make_unique<RBTree<T>>(
                Color::Red,
                std::make_unique<RBTree<T>>(
                    Color::Black, std::move(a->left), std::move(a->data), std::move(a->right->left)),
                std::move(x),
                std::make_unique<RBTree<T>>(
                    Color::Black, std::move(a->right->right), std::move(b->data), std::move(b->right)));
        }
        if (b && b->color == Color::Red && b->right && b->right->color == Color::Red) {
            return std::make_unique<RBTree<T>>(
                Color::Red,
                std::make_unique<RBTree<T>>(
                    Color::Black, std::move(a), std::move(x), std::move(b->left)),
                std::move(b->data),
                std::move(b->right));
        }
        if (b && b->color == Color::Red && b->left && b->left->color == Color::Red) {
            return std::make_unique<RBTree<T>>(
                Color::Red,
                std::make_unique<RBTree<T>>(
                    Color::Black, std::move(a), std::move(x), std::move(b->left->left)),
                std::move(b->left->data),
                std::make_unique<RBTree<T>>(
                    Color::Black, std::move(b->left->right), std::move(b->data), std::move(b->right)));
        }
    }
    return std::make_unique<RBTree<T>>(color, std::move(a), std::move(x), std::move(b));
}

template <typename T>
std::unique_ptr<RBTree<T>> insert(std::unique_ptr<RBTree<T>> t, T x) {
    if (!t) {
        return std::make_unique<RBTree<T>>(Color::Red, nullptr, std::move(x), nullptr);
    }
    if (x < t->data) {
        t->left = insert(std::move(t->left), std::move(x));
    } else if (x > t->data) {
        t->right = insert(std::move(t->right), std::move(x));
    } else {
        return t;
    }
    return balance(t->color, std::move(t->left), std::move(t->data), std::move(t->right));
}