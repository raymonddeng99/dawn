#include <vector>

class XFastTrie {
private:
    std::vector<bool> bits;
    std::vector<int> ones;
    int size;

public:
    XFastTrie(int w) {
        int u = 1 << w;
        bits = std::vector<bool>(2 * u - 1, false);
        ones = std::vector<int>();
        size = 0;
    }

    void insert(int x) {
        insertAux(x, 0);
    }

    int predecessor(int x) {
        int pred = predecessorAux(x, 0);
        if (pred == -1) {
            return -1;
        }
        while (pred > 0 && !std::binary_search(ones.begin(), ones.end(), pred)) {
            pred /= 2;
        }
        return pred - 1;
    }

private:
    void insertAux(int x, int i) {
        if (i >= bits.size()) {
            size++;
            return;
        }
        bool bit = (x & (1 << i)) != 0;
        int index = 1 << (bits.size() - 1 - i);
        bits[index - 1] = bits[index - 1] || bit;
        if (bit) {
            ones.push_back(index);
            insertAux(x, i + 1);
        } else {
            insertAux(x, i + 1);
        }
    }

    int predecessorAux(int x, int i) {
        if (i >= bits.size()) {
            return -1;
        }
        int index = 1 << (bits.size() - 1 - i);
        if (bits[index - 1]) {
            int leftChild = 2 * index;
            int rightChild = leftChild + 1;
            if (bits[rightChild - 1]) {
                return predecessorAux(x, i + 1);
            } else {
                return index;
            }
        } else {
            return predecessorAux(x, i + 1);
        }
    }
};