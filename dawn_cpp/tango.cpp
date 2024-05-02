#include <iostream>
#include <vector>

template <typename T>
struct AuxTree {
    T key;
    AuxTree<T>* left = nullptr;
    AuxTree<T>* right = nullptr;

    AuxTree(T key) : key(key) {}

    ~AuxTree() {
        delete left;
        delete right;
    }

    void insert(T key) {
        if (key < this->key) {
            if (left == nullptr) {
                left = new AuxTree(key);
            } else {
                left->insert(key);
            }
        } else {
            if (right == nullptr) {
                right = new AuxTree(key);
            } else {
                right->insert(key);
            }
        }
    }

    bool find(T key) {
        if (key == this->key) {
            return true;
        } else if (key < this->key) {
            return (left != nullptr) && left->find(key);
        } else {
            return (right != nullptr) && right->find(key);
        }
    }
};

template <typename T>
struct TangoTree {
    std::vector<AuxTree<T>*> auxTrees;

    TangoTree() {
        auxTrees.push_back(new AuxTree<T>(0));
    }

    ~TangoTree() {
        for (auto aux : auxTrees) {
            delete aux;
        }
    }

    bool find(T key) {
        for (auto aux : auxTrees) {
            if (aux->find(key)) {
                return true;
            }
        }
        return false;
    }

    void update(T key) {
        std::vector<AuxTree<T>*> left, right;
        for (auto aux : auxTrees) {
            if (aux->key < key) {
                left.push_back(aux);
            } else {
                right.push_back(aux);
            }
        }
        auxTrees.clear();
        auxTrees.insert(auxTrees.end(), left.begin(), left.end());
        auxTrees.push_back(new AuxTree<T>(key));
        auxTrees.insert(auxTrees.end(), right.begin(), right.end());
    }
};