#include <algorithm>
#include <utility>

// On RAM priority queues, Thorup '96 
template <typename T>
struct VEBTree {
    T value;
    VEBTree* left = nullptr;
    VEBTree* right = nullptr;
    bool isLeaf;

    VEBTree(T val) : value(val), isLeaf(true) {}

    ~VEBTree() {
        delete left;
        delete right;
    }

    T min() const {
        if (isLeaf) return value;
        T minLeft = (left != nullptr) ? left->min() : std::numeric_limits<T>::max();
        T minRight = (right != nullptr) ? right->min() : std::numeric_limits<T>::max();
        return std::min(minLeft, minRight);
    }

    void insert(T val) {
        if (isLeaf) {
            if (val < value) {
                right = new VEBTree(value);
                value = val;
            } else {
                right = new VEBTree(val);
            }
            isLeaf = false;
            return;
        }

        if (val < left->min()) {
            if (left == nullptr) left = new VEBTree(val);
            else left->insert(val);
        } else {
            if (right == nullptr) right = new VEBTree(val);
            else right->insert(val);
        }
    }
};