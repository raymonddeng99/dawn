#include <memory>

template <typename T>
struct FusionTree {
    T value;
    std::unique_ptr<FusionTree<T>> left, right;

    FusionTree(T val) : value(val), left(nullptr), right(nullptr) {}

    static FusionTree<T> empty() {
        return FusionTree<T>(T());
    }

    bool is_empty() const {
        return value == T() && !left && !right;
    }

    FusionTree<T> insert(T x) {
        if (is_empty()) {
            return FusionTree<T>(x);
        }
        auto new_left = left->insert(x);
        return FusionTree<T>(value, std::make_unique<FusionTree<T>>(std::move(new_left)), std::move(right));
    }

    T find(T x) const {
        if (is_empty()) {
            return T();
        }
        if (value == x) {
            T left_sum = left ? left->find(x) : T();
            T right_sum = right ? right->find(x) : T();
            return value + left_sum + right_sum;
        }
        T left_sum = left ? left->find(x) : T();
        T right_sum = right ? right->find(x) : T();
        return left_sum + right_sum;
    }

    static FusionTree<T> union_(const FusionTree<T>& left, const FusionTree<T>& right) {
        if (left.is_empty()) {
            return right;
        }
        if (right.is_empty()) {
            return left;
        }
        auto new_left = union_(*left.left, *right.left);
        auto new_right = union_(*left.right, *right.right);
        return FusionTree<T>(left.value + right.value, std::make_unique<FusionTree<T>>(std::move(new_left)), std::make_unique<FusionTree<T>>(std::move(new_right)));
    }
};