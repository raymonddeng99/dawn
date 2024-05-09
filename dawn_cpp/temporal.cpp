#include <vector>
#include <variant>
#include <optional>
#include <queue>
#include <utility>

template <typename T>
class Deque {
private:
    std::variant<std::monostate, T, std::pair<T, T>, std::tuple<T, T, T>, std::tuple<T, T, T, T>> node;

    std::vector<std::variant<std::monostate, T, std::pair<T, T>, std::tuple<T, T, T>, std::tuple<T, T, T, T>>> front;
    std::vector<std::variant<std::monostate, T, std::pair<T, T>, std::tuple<T, T, T>, std::tuple<T, T, T, T>>> back;

    void addFrontHelper(const T& x) {
        std::vector<std::variant<std::monostate, T, std::pair<T, T>, std::tuple<T, T, T>, std::tuple<T, T, T, T>>> newFront;
        for (const auto& n : front) {
            if (std::holds_alternative<std::monostate>(n)) {
                newFront.emplace_back(x);
            } else if (auto* p = std::get_if<T>(&n)) {
                newFront.emplace_back(std::make_tuple(*p, *p, *p, x));
                newFront.emplace_back(*p);
            } else if (auto* p = std::get_if<std::pair<T, T>>(&n)) {
                newFront.emplace_back(std::make_tuple(p->first, p->first, p->second));
                newFront.emplace_back(std::make_tuple(p->second, x, x, x));
            } else if (auto* p = std::get_if<std::tuple<T, T, T>>(&n)) {
                newFront.emplace_back(std::make_pair(std::get<0>(*p), std::get<1>(*p)));
                newFront.emplace_back(std::make_pair(std::get<1>(*p), std::get<2>(*p)));
                newFront.emplace_back(std::make_tuple(std::get<2>(*p), std::get<2>(*p), std::get<2>(*p), std::get<2>(*p)));
                newFront.emplace_back(x);
            } else if (auto* p = std::get_if<std::tuple<T, T, T, T>>(&n)) {
                newFront.emplace_back(std::get<0>(*p));
                newFront.emplace_back(std::get<1>(*p));
                newFront.emplace_back(std::get<2>(*p));
                newFront.emplace_back(std::get<3>(*p));
                newFront.emplace_back(x);
            }
        }
        newFront.emplace_back(x);
        front = std::move(newFront);
    }

    void addBackHelper(const T& x) {
        std::vector<std::variant<std::monostate, T, std::pair<T, T>, std::tuple<T, T, T>, std::tuple<T, T, T, T>>> newBack;
        for (const auto& n : back) {
            if (std::holds_alternative<std::monostate>(n)) {
                newBack.emplace_back(x);
            } else if (auto* p = std::get_if<T>(&n)) {
                newBack.emplace_back(*p);
                newBack.emplace_back(std::make_tuple(*p, *p, *p, x));
            } else if (auto* p = std::get_if<std::pair<T, T>>(&n)) {
                newBack.emplace_back(std::make_tuple(p->first, p->second, p->second));
                newBack.emplace_back(std::make_tuple(p->first, p->first, p->first, x));
            } else if (auto* p = std::get_if<std::tuple<T, T, T>>(&n)) {
                newBack.emplace_back(std::make_pair(std::get<0>(*p), std::get<1>(*p)));
                newBack.emplace_back(std::make_pair(std::get<2>(*p), std::get<2>(*p)));
                newBack.emplace_back(std::make_tuple(std::get<1>(*p), std::get<1>(*p), std::get<1>(*p), std::get<1>(*p)));
                newBack.emplace_back(x);
            } else if (auto* p = std::get_if<std::tuple<T, T, T, T>>(&n)) {
                newBack.emplace_back(std::get<0>(*p));
                newBack.emplace_back(std::get<1>(*p));
                newBack.emplace_back(std::get<2>(*p));
                newBack.emplace_back(std::get<3>(*p));
                newBack.emplace_back(x);
            }
        }
        newBack.emplace_back(x);
        back = std::move(newBack);
    }

public:
    Deque() : front(), back() {}

    bool isEmpty() const {
        return front.empty() && back.empty();
    }

    void addFront(const T& x) {
        addFrontHelper(x);
    }

    void addBack(const T& x) {
        addBackHelper(x);
    }

    std::optional<T> takeFront() {
        if (front.empty()) {
            return std::nullopt;
        }
        T x;
        if (std::holds_alternative<T>(front.front())) {
            x = std::get<T>(front.front());
            front.erase(front.begin());
        } else if (auto* p = std::get_if<std::pair<T, T>>(&front.front())) {
            x = p->first;
            front.front() = p->second;
        } else if (auto* p = std::get_if<std::tuple<T, T, T>>(&front.front())) {
            x = std::get<0>(*p);
            front.front() = std::make_pair(std::get<1>(*p), std::get<2>(*p));
        } else if (auto* p = std::get_if<std::tuple<T, T, T, T>>(&front.front())) {
            x = std::get<0>(*p);
            front.front() = std::make_tuple(std::get<1>(*p), std::get<2>(*p), std::get<3>(*p));
        }
        if (back.empty()) {
            back = std::move(front);
            front.clear();
        } else {
            std::vector<std::variant<std::monostate, T, std::pair<T, T>, std::tuple<T, T, T>, std::tuple<T, T, T, T>>> temp = std::move(back);
            back = std::move(front);
            front = std::move(temp);
        }
        return x;
    }

    std::optional<T> takeBack() {
        if (back.empty()) {
            return std::nullopt;
        }
        T x;
        if (std::holds_alternative<T>(back.back())) {
            x = std::get<T>(back.back());
            back.pop_back();
        } else if (auto* p = std::get_if<std::pair<T, T>>(&back.back())) {
            x = p->second;
            back.back() = p->first;
        } else if (auto* p = std::get_if<std::tuple<T, T, T>>(&back.back())) {
            x = std::get<2>(*p);
            back.back() = std::make_pair(std::get<0>(*p), std::get<1>(*p));
        } else if (auto* p = std::get_if<std::tuple<T, T, T, T>>(&back.back())) {
            x = std::get<3>(*p);
            back.back() = std::make_tuple(std::get<0>(*p), std::get<1>(*p), std::get<2>(*p));
        }
        return x;
    }
};


// Partially retroactive priority queue
template <typename T>
class PriorityQueue {
private:
    std::priority_queue<std::pair<int, T>, std::vector<std::pair<int, T>>, std::greater<std::pair<int, T>>> pq;
    int time;

public:
    PriorityQueue() : time(0) {}

    bool isEmpty() const {
        return pq.empty();
    }

    void insert(const T& x) {
        time++;
        pq.emplace(time, x);
    }

    T findMin() const {
        return pq.top().second;
    }

    T deleteMin() {
        T result = pq.top().second;
        pq.pop();
        return result;
    }

    void retroactiveUpdate(int t, const T& x) {
        std::priority_queue<std::pair<int, T>, std::vector<std::pair<int, T>>, std::greater<std::pair<int, T>>> newPq;
        while (!pq.empty()) {
            int priority = pq.top().first;
            T value = pq.top().second;
            pq.pop();
            if (priority <= t) {
                newPq.emplace(priority, x);
            } else {
                newPq.emplace(priority, value);
            }
        }
        std::swap(pq, newPq);
    }
};