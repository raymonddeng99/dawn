#include <algorithm>
#include <vector>

class SignatureSort {
public:
    template <typename T>
    static std::vector<T> sort(const std::vector<T>& arr) {
        if (arr.size() < 2) {
            return arr;
        }

        std::vector<T> left(arr.begin(), arr.begin() + arr.size() / 2);
        std::vector<T> right(arr.begin() + arr.size() / 2, arr.end());

        left = sort(left);
        right = sort(right);

        return merge(left, right);
    }

private:
    template <typename T>
    static std::vector<T> merge(const std::vector<T>& left, const std::vector<T>& right) {
        std::vector<T> result;
        size_t i = 0, j = 0;

        while (i < left.size() && j < right.size()) {
            if (left[i] <= right[j]) {
                result.push_back(left[i++]);
            } else {
                result.push_back(right[j++]);
            }
        }

        result.insert(result.end(), left.begin() + i, left.end());
        result.insert(result.end(), right.begin() + j, right.end());

        return result;
    }
};