#include <array>
#include <vector>
#include <memory>

template <typename T>
class PriorityQueue {
private:
    static const int CLUSTER_SIZE = 2;

    struct Cluster {
        std::vector<std::unique_ptr<Cluster>> children;
        T value;

        Cluster(T val) : value(val) {}
        Cluster() : value(T()) {}
    };

    int size;
    std::unique_ptr<Cluster> root;

    std::unique_ptr<Cluster> insertHelper(std::unique_ptr<Cluster> c, T val) {
        if (!c) {
            return std::make_unique<Cluster>(val);
        }

        if (c->children.empty()) {
            c->children.reserve(CLUSTER_SIZE);
            for (int i = 0; i < CLUSTER_SIZE; ++i) {
                c->children.emplace_back(nullptr);
            }
        }

        bool inserted = false;
        for (int i = 0; i < CLUSTER_SIZE; ++i) {
            if (!c->children[i]) {
                c->children[i] = std::make_unique<Cluster>(val);
                inserted = true;
                break;
            } else {
                c->children[i] = insertHelper(std::move(c->children[i]), val);
            }
        }

        if (!inserted) {
            std::vector<std::unique_ptr<Cluster>> newChildren;
            newChildren.reserve(CLUSTER_SIZE);
            for (int i = 0; i < CLUSTER_SIZE; ++i) {
                if (i < (int)c->children.size()) {
                    newChildren.emplace_back(std::move(c->children[i]));
                } else {
                    newChildren.emplace_back(std::make_unique<Cluster>(val));
                }
            }
            return std::make_unique<Cluster>(Cluster{std::move(newChildren), T()});
        }

        return c;
    }

public:
    PriorityQueue() : size(0), root(nullptr) {}

    bool empty() const { return size == 0; }

    void insert(T val) {
        auto newRoot = insertHelper(std::move(root), val);
        root = std::move(newRoot);
        ++size;
    }

    T findMin() const {
        if (!root) {
            throw std::out_of_range("Priority queue is empty");
        }
        return findMinHelper(root.get());
    }

    T removeMin() {
        if (!root) {
            throw std::out_of_range("Priority queue is empty");
        }
        T minVal = findMinHelper(root.get());
        root = removeMinHelper(std::move(root));
        --size;
        return minVal;
    }

private:
    T findMinHelper(const Cluster* c) const {
        if (!c->children.empty()) {
            T minChild = findMinHelper(c->children[0].get());
            for (int i = 1; i < CLUSTER_SIZE; ++i) {
                if (c->children[i]) {
                    minChild = std::min(minChild, findMinHelper(c->children[i].get()));
                }
            }
            return minChild;
        }
        return c->value;
    }

    std::unique_ptr<Cluster> removeMinHelper(std::unique_ptr<Cluster> c) {
        if (!c->children.empty()) {
            std::unique_ptr<Cluster> minChild = removeMinHelper(std::move(c->children[0]));
            bool isFirst = true;
            for (int i = 1; i < CLUSTER_SIZE; ++i) {
                if (c->children[i]) {
                    if (isFirst || (minChild && findMinHelper(c->children[i].get()) < findMinHelper(minChild.get()))) {
                        std::swap(minChild, c->children[i]);
                        isFirst = false;
                    }
                }
            }
            return c;
        }
        return nullptr;
    }
};