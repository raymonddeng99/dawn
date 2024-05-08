#include <iostream>
#include <cstdlib>

class Node {
public:
    int value;
    Node* parent;
    Node* left;
    Node* right;
    bool rev;
    int sum;

    Node(int value) : value(value), parent(nullptr), left(nullptr), right(nullptr), rev(false), sum(value) {}

    int getSum() {
        return sum;
    }

    void updateSum() {
        sum = value + (left ? left->getSum() : 0) + (right ? right->getSum() : 0);
    }

    void push() {
        if (rev) {
            rev = false;
            std::swap(left, right);
            if (left) left->rev = !left->rev, left->push();
            if (right) right->rev = !right->rev, right->push();
        }
    }

    void makeRoot() {
        push();
        if (parent) {
            if (parent->left == this) parent->left = nullptr;
            else parent->right = nullptr;
            parent->makeRoot();
            parent = nullptr;
        }
    }

    void splay() {
        makeRoot();
        Node* p = parent;
        while (p) {
            Node* pp = p->parent;
            p->makeRoot();
            if (pp) {
                pp->makeRoot();
                if ((pp->left == p) == (p->left == this)) p->rotate();
                else rotate();
            }
            rotate();
            p = parent;
        }
        updateSum();
    }

    void rotate() {
        Node* p = parent;
        Node* pp = p->parent;
        if (pp) {
            if (pp->left == p) pp->left = this;
            else pp->right = this;
        }
        parent = pp;
        if (p->left == this) {
            p->left = right;
            right = p;
        } else {
            p->right = left;
            left = p;
        }
        p->parent = this;
        p->updateSum();
        updateSum();
    }

    void access() {
        splay();
        rev = false;
        Node* c = left;
        while (c) {
            c->splay();
            c->rev = !c->rev;
            c->updateSum();
            c->right = this;
            c->parent = this;
            c = c->left;
        }
    }

    void link(Node* other) {
        access();
        other->access();
        other->parent = this;
        other->updateSum();
    }

    void cut() {
        access();
        if (left) {
            left->parent = nullptr;
            left = nullptr;
            updateSum();
        }
    }

    bool isRoot() {
        return parent == nullptr;
    }

    Node* lca(Node* other) {
        other->access();
        int otherSum = other->getSum();
        access();
        Node* z = other;
        while (z->getSum() < otherSum || z->getSum() < getSum()) {
            z->access();
            z = z->parent;
        }
        return z;
    }

    int pathSum(Node* other) {
        Node* z = lca(other);
        access();
        other->access();
        int xSum = getSum();
        int ySum = other->getSum();
        int zSum = z->getSum();
        return xSum + ySum - 2 * zSum;
    }
};