#include <vector>
#include <stack>

template <typename T>
class EulerTourTree {
public:
    std::vector<T> tour;
    std::vector<int> first;
    std::vector<int> last;
    std::vector<int> level;

    EulerTourTree(const std::vector<T>& tour) {
        int n = tour.size();
        this->tour = tour;
        first.resize(n);
        last.resize(n);
        level.resize(n);
        std::stack<int> stack;
        for (int i = 0; i < n; i++) {
            if (stack.empty()) {
                stack.push(i);
                level[i] = 0;
            } else {
                int j = stack.top();
                if (tour[i] == tour[j]) {
                    stack.pop();
                    last[j] = i;
                } else {
                    stack.push(i);
                    level[i] = level[j] + 1;
                    first[i] = j;
                }
            }
        }
    }
};