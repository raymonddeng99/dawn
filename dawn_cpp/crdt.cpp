// CRDTs

/*
Specification: CRDTs = state-based or op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
*/

#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <stdexcept>
#include <atomic>
#include <utility>
#include <mutex>
#include <unordered_set>
#include <unordered_map>

enum class Operation { Increment, Decrement };

struct Counter {
    int value;
    std::vector<Operation> ops;

    Counter() : value(0), ops() {}

    Counter apply() const {
        Counter result = *this;
        for (const auto& op : ops) {
            switch (op) {
                case Operation::Increment:
                    result.value++;
                    break;
                case Operation::Decrement:
                    result.value--;
                    break;
            }
        }
        result.ops.clear();
        return result;
    }

    Counter merge(const Counter& other) const {
        Counter result;
        result.value = std::max(value, other.value);
        result.ops = ops;
        result.ops.insert(result.ops.end(), other.ops.begin(), other.ops.end());
        for (const auto& op : result.ops) {
            switch (op) {
                case Operation::Increment:
                    result.value++;
                    break;
                case Operation::Decrement:
                    result.value--;
                    break;
            }
        }
        result.ops.clear();
        return result;
    }

    static std::vector<Operation> downstream() {
        return {};
    }

    Counter update(Operation op) const {
        Counter result = *this;
        result.ops.push_back(op);
        return result;
    }
};


// State based increment-only counter
class GCounter {
public:
    GCounter(int size) : data(size, 0) {}

    void update(int i) {
        if (i < 0 || i >= static_cast<int>(data.size())) {
            throw std::out_of_range("Index out of bounds");
        }
        ++data[i];
    }

    int query(int i) const {
        if (i < 0 || i >= static_cast<int>(data.size())) {
            throw std::out_of_range("Index out of bounds");
        }
        return data[i];
    }

    int compare(const GCounter& other) const {
        if (data.size() != other.data.size()) {
            throw std::invalid_argument("Vectors have different lengths");
        }
        return std::lexicographical_compare(data.begin(), data.end(), other.data.begin(), other.data.end());
    }

    GCounter merge(const GCounter& other) const {
        if (data.size() != other.data.size()) {
            throw std::invalid_argument("Vectors have different lengths");
        }
        std::vector<int> result(data.size());
        std::transform(data.begin(), data.end(), other.data.begin(), result.begin(),
                       [](int a, int b) { return std::max(a, b); });
        return Counter(result);
    }

private:
    std::vector<int> data;
};


// State-based PN Counter
class PNCounter {
public:
    PNCounter(int size) : p(size, 0), n(size, 0) {}

    static PNCounter initialize(int size, const std::vector<int>& p, const std::vector<int>& q) {
        return {p, q};
    }

    void increment() {
        int g = myID();
        p[g]++;
    }

    void decrement() {
        int g = myID();
        n[g]++;
    }

    int value() const {
        int sum_p = std::accumulate(p.begin(), p.end(), 0);
        int sum_n = std::accumulate(n.begin(), n.end(), 0);
        return sum_p - sum_n;
    }

    bool compare(const PNCounter& other) const {
        return std::equal(p.begin(), p.end(), other.p.begin(), std::less_equal<int>()) &&
               std::equal(n.begin(), n.end(), other.n.begin(), std::less_equal<int>());
    }

    PNCounter merge(const PNCounter& other) const {
        if (p.size() != other.p.size() || n.size() != other.n.size()) {
            throw std::invalid_argument("Vectors have different lengths");
        }
        PNCounter z(p.size());
        std::transform(p.begin(), p.end(), other.p.begin(), z.p.begin(), std::max<int>);
        std::transform(n.begin(), n.end(), other.n.begin(), z.n.begin(), std::max<int>);
        return z;
    }

private:
    std::vector<int> p;
    std::vector<int> n;
};


// State-based last-writer-wins register
template <typename T>
class LastWriterWinsRegister {
private:
    std::atomic<T> value;
    std::atomic<double> timestamp;

public:
    LastWriterWinsRegister(T initialValue) : value(initialValue), timestamp(0.0) {}

    T read() const {
        return value.load(std::memory_order_acquire);
    }

    void write(T newValue, double newTimestamp) {
        T oldValue = value.load(std::memory_order_relaxed);
        double oldTimestamp = timestamp.load(std::memory_order_relaxed);

        while (newTimestamp > oldTimestamp &&
               !std::atomic_compare_exchange_strong(
                   &value, &oldValue, newValue,
                   std::memory_order_acq_rel,
                   std::memory_order_relaxed)) {
            oldTimestamp = timestamp.load(std::memory_order_relaxed);
        }

        if (newTimestamp > oldTimestamp) {
            timestamp.store(newTimestamp, std::memory_order_release);
        }
    }

    bool compareAndSwap(T expectedValue, double expectedTimestamp, T newValue, double newTimestamp) {
        T oldValue = value.load(std::memory_order_acquire);
        double oldTimestamp = timestamp.load(std::memory_order_acquire);

        if (oldValue == expectedValue && oldTimestamp == expectedTimestamp) {
            write(newValue, newTimestamp);
            return true;
        }

        return false;
    }
};


// Operation-based last-writer-wins register
struct OpBasedLWWValue {
    int val;
    double ts;

    OpBasedLWWValue(int val, double ts) : val(val), ts(ts) {}
};

enum class OpType { Update, Reset };

class OpBasedLWWRegister {
private:
    std::atomic<OpBasedLWWValue> value;
    std::vector<std::pair<OpType, OpBasedLWWValue>> pending;
    std::mutex mtx;

public:
    OpBasedLWWRegister(int initialValue)
        : value(OpBasedLWWValue(initialValue, 0.0)), pending() {}

    int read() const {
        return value.load(std::memory_order_acquire).val;
    }

    void update(int newValue, double newTimestamp) {
        std::lock_guard<std::mutex> lock(mtx);
        OpBasedLWWValue oldValue = value.load(std::memory_order_relaxed);
        if (newTimestamp > oldValue.ts) {
            value.store(OpBasedLWWValue(newValue, newTimestamp), std::memory_order_release);
            pending.clear();
        } else {
            pending.emplace_back(OpType::Update, OpBasedLWWValue(newValue, newTimestamp));
        }
    }

    void reset() {
        std::lock_guard<std::mutex> lock(mtx);
        pending.emplace_back(OpType::Reset, OpBasedLWWValue(0, 0.0));
    }

    void applyPending() {
        std::vector<std::pair<OpType, OpBasedLWWValue>> newPending;
        for (const auto& [op, arg] : pending) {
            switch (op) {
            case OpType::Update:
                if (arg.ts > value.load(std::memory_order_relaxed).ts) {
                    value.store(arg, std::memory_order_release);
                } else {
                    newPending.emplace_back(OpType::Update, arg);
                }
                break;
            case OpType::Reset:
                value.store(OpBasedLWWValue(0, 0.0), std::memory_order_release);
                break;
            }
        }
        pending = std::move(newPending);
    }

    void downstream() {
        std::lock_guard<std::mutex> lock(mtx);
        applyPending();
    }
};

// State-based multi-value register
struct MVRegisterValue {
    int x;
    std::vector<int64_t> v;

    MVRegisterValue(int x, const std::vector<int64_t>& v) : x(x), v(v) {}
};

class MVRegister {
private:
    std::vector<MVRegisterValue> payload;

public:
    MVRegister() {
        payload.emplace_back(MVRegisterValue(-1, std::vector<int64_t>{}));
    }

    std::vector<int64_t> queryIncrementVV(int processID) {
        int64_t maxVersion = 0;
        for (const auto& entry : payload) {
            for (const auto& v : entry.v) {
                maxVersion = std::max(maxVersion, v);
            }
        }
        maxVersion++;

        std::vector<int64_t> newVersion(payload.size(), maxVersion);
        newVersion[processID]++;
        return newVersion;
    }

    void updateAssign(const std::vector<int>& set_r, int processID) {
        std::vector<int64_t> newVersion = queryIncrementVV(processID);
        for (int x : set_r) {
            payload.emplace_back(MVRegisterValue(x, newVersion));
        }
    }

    const std::vector<MVRegisterValue>& queryValue() const {
        return payload;
    }

    bool compare(const MVRegister& other) const {
        for (const auto& entryA : payload) {
            for (const auto& entryB : other.payload) {
                if (entryA.x == entryB.x) {
                    for (const auto& vA : entryA.v) {
                        if (std::all_of(entryB.v.begin(), entryB.v.end(), [&vA](int64_t vB) { return vA > vB; })) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    MVRegister merge(const MVRegister& other) const {
        MVRegister merged;
        auto includeEntry = [&](const MVRegisterValue& entry, const std::vector<MVRegisterValue>& otherPayload) {
            return std::any_of(otherPayload.begin(), otherPayload.end(), [&entry](const MVRegisterValue& otherEntry) {
                const auto& [x, v] = entry;
                const auto& [x_, w] = otherEntry;
                if (x == x_) {
                    return *std::max_element(w.begin(), w.end()) >= v.back() ||
                           std::any_of(v.begin(), v.end(), [&w](int64_t vEntry) {
                               return std::all_of(w.begin(), w.end(), [vEntry](int64_t wEntry) {
                                   return vEntry > wEntry;
                               });
                           });
                }
                return false;
            });
        };

        for (const auto& entry : payload) {
            if (includeEntry(entry, other.payload)) {
                merged.payload.push_back(entry);
            }
        }

        for (const auto& otherEntry : other.payload) {
            if (includeEntry(otherEntry, payload)) {
                merged.payload.push_back(otherEntry);
            }
        }

        return merged;
    }
};


// State-based grow-only set
template <typename T>
class GSet {
public:
  GSet() : data(std::unordered_set<T>()) {}

  void add(const T& element) {
    data.insert(element);
  }

  bool lookup(const T& element) const {
    return data.count(element) > 0;
  }

  bool compare(const GSet<T>& other) const {
    return data == other.data;
  }

  GSet<T> merge(const GSet<T>& other) const {
    GSet<T> merged;
    merged.data.insert(data.begin(), data.end());
    merged.data.insert(other.data.begin(), other.data.end());
    return merged;
  }

private:
  std::unordered_set<T> data;
};


// State-based 2P set
template <typename T>
class StateBased2PSet {
private:
    std::set<T> added;
    std::set<T> removed;

public:
    StateBased2PSet() = default;

    bool lookup(const T& e) const {
        return (added.count(e) > 0) && (removed.count(e) == 0);
    }

    void add(const T& e) {
        if (!lookup(e)) {
            added.insert(e);
        }
    }

    void remove(const T& e) {
        if (lookup(e)) {
            removed.insert(e);
        }
    }

    bool compare(const StateBased2PSet& other) const {
        std::set<T> temp_added, temp_removed;
        std::set_intersection(added.begin(), added.end(), other.added.begin(), other.added.end(),
                              std::inserter(temp_added, temp_added.begin()));
        std::set_intersection(removed.begin(), removed.end(), other.removed.begin(), other.removed.end(),
                              std::inserter(temp_removed, temp_removed.begin()));
        return (temp_added == added) && (temp_removed == removed);
    }

    StateBased2PSet merge(const StateBased2PSet& other) const {
        StateBased2PSet<T> merged;
        std::set_union(added.begin(), added.end(), other.added.begin(), other.added.end(),
                       std::inserter(merged.added, merged.added.begin()));
        std::set_union(removed.begin(), removed.end(), other.removed.begin(), other.removed.end(),
                       std::inserter(merged.removed, merged.removed.begin()));
        return merged;
    }
};


// Op based 2p set with unique elements
template <typename T>
class USet {
public:
    USet(const std::vector<T>& p, const std::vector<T>& q) : p(p), n(q) {}

    static USet initialize(int size, const std::vector<T>& p, const std::vector<T>& q) {
        return USet(p, q);
    }

    void add(const T& element) {
        p.push_back(element);
    }

    bool lookup(const T& element) const {
        return std::find(p.begin(), p.end(), element) != p.end() &&
               std::find(n.begin(), n.end(), element) == n.end();
    }

    void remove(const T& element) {
        if (lookup(element)) {
            n.push_back(element);
        }
    }

    bool compare(const USet& other) const {
        return std::is_permutation(p.begin(), p.end(), other.p.begin(), other.p.end()) &&
               std::is_permutation(n.begin(), n.end(), other.n.begin(), other.n.end());
    }

    USet merge(const USet& other) const {
        std::vector<T> merged_p(p), merged_n(n);
        merged_p.insert(merged_p.end(), other.p.begin(), other.p.end());
        merged_n.insert(merged_n.end(), other.n.begin(), other.n.end());
        return USet(merged_p, merged_n);
    }

private:
    std::vector<T> p;
    std::vector<T> n;
};


// Molli, Weiss, Skaf set
template <typename T>
class MWSSet {
private:
    std::unordered_map<T, int> data;

public:
    MWSSet() {}

    bool lookup(const T& e) const {
        auto it = data.find(e);
        return it != data.end() && it->second > 0;
    }

    void add(const T& e) {
        auto it = data.find(e);
        int j;
        if (it != data.end() && it->second < 0) {
            j = -it->second + 1;
        } else {
            j = 1;
        }
        data[e] = j;
    }

    void remove(const T& e) {
        auto it = data.find(e);
        if (it != data.end() && it->second > 0) {
            data[e] = it->second - 1;
            if (data[e] == 0) {
                data.erase(it);
            }
        }
    }
};


// Operation based observed-remove set
template <typename T>
class ORSet {
private:
    std::unordered_set<T> added;
    std::unordered_set<T> removed;

public:
    ORSet() {}

    bool lookup(const T& e) const {
        return added.count(e) && !removed.count(e);
    }

    void add(const T& e) {
        if (!lookup(e)) {
            added.insert(e);
        }
    }

    void remove(const T& e) {
        if (lookup(e)) {
            removed.insert(e);
        }
    }
};

// Operation based 2P2P graph
using Vertex = int;
using Edge = std::pair<Vertex, Vertex>;

class TwoPGraph {
private:
    std::unordered_set<Vertex> va, vr;
    std::unordered_set<Edge> ea, er;

public:
    TwoPGraph() = default;

    bool lookup_vertex(Vertex v) const {
        return va.count(v) && !vr.count(v);
    }

    bool lookup_edge(const Edge& e) const {
        Vertex u = e.first, v = e.second;
        return va.count(u) && va.count(v) && (ea.count(e) || er.count(e));
    }

    void add_vertex(Vertex v) {
        va.insert(v);
    }

    void add_edge(Vertex u, Vertex v) {
        if (va.count(u) && va.count(v)) {
            ea.insert(std::make_pair(u, v));
        }
    }

    void remove_vertex(Vertex v) {
        if (va.count(v)) {
            bool can_remove = true;
            for (const auto& e : ea) {
                if (e.first == v || e.second == v) {
                    can_remove = false;
                    break;
                }
            }
            for (const auto& e : er) {
                if (e.first == v || e.second == v) {
                    can_remove = false;
                    break;
                }
            }
            if (can_remove) {
                va.erase(v);
                vr.insert(v);
            }
        }
    }

    void remove_edge(Vertex u, Vertex v) {
        if (va.count(u) && va.count(v)) {
            er.insert(std::make_pair(u, v));
        }
    }
};


// Op-based add only monotonic DAG
class MonotonicDAG {
public:
    MonotonicDAG() {
        vertices.insert(-1);
        vertices.insert(1);
        edges[{-1, 1}] = true;
    }

    bool lookupVertex(Vertex v) const {
        return vertices.count(v);
    }

    bool lookupEdge(const Edge& e) const {
        return edges.count(e);
    }

    void addVertex(Vertex v) {
        vertices.insert(v);
    }

    void addEdge(Vertex u, Vertex v) {
        if (lookupVertex(u) && lookupVertex(v)) {
            std::unordered_set<Vertex> visited;
            if (dfs(u, v, visited)) {
                edges[{u, v}] = true;
            }
        }
    }

    void removeVertex(Vertex v) {
        if (lookupVertex(v)) {
            bool canRemove = true;
            for (const auto& [u, w] : edges) {
                if (u.first == v || u.second == v) {
                    canRemove = false;
                    break;
                }
            }
            if (canRemove) {
                vertices.erase(v);
            }
        }
    }

    void removeEdge(Vertex u, Vertex v) {
        edges.erase({u, v});
    }
private:
    std::unordered_set<Vertex> vertices;
    std::unordered_map<Edge, bool> edges;

    bool dfs(Vertex u, Vertex v, std::unordered_set<Vertex>& visited) const {
        if (u == v) return true;
        if (visited.count(u)) return false;
        visited.insert(u);
        for (const auto& [x, y] : edges) {
            if (x.first == u && dfs(x.second, v, visited))
                return true;
        }
        return false;
    }
};


// Add remove partial order
class AddRemovePartialOrder {
public:
    using Vertex = int;
    using Edge = std::pair<Vertex, Vertex>;

    AddRemovePartialOrder() {
        vertices = {-1, 1};
        edges = {{-1, 1}};
    }

    bool lookup(Vertex v) const {
        return std::find(vertices.begin(), vertices.end(), v) != vertices.end();
    }

    bool before(Vertex u, Vertex v) const {
        for (Vertex w : vertices) {
            if ((w == u && lookup(v)) || (w == v && lookup(u))) {
                return true;
            }
            if (lookup(w)) {
                auto it1 = std::find(edges.begin(), edges.end(), std::make_pair(w, u));
                auto it2 = std::find(edges.begin(), edges.end(), std::make_pair(w, v));
                if (it1 != edges.end() && it2 != edges.end()) {
                    return true;
                }
            }
        }
        return false;
    }

    void addBetween(Vertex u, Vertex v, Vertex w) {
        if (!lookup(w) || !before(u, w) || !before(w, v)) {
            throw std::invalid_argument("addBetween precondition violated");
        }
        vertices.push_back(w);
        edges.push_back(std::make_pair(u, w));
        edges.push_back(std::make_pair(w, v));
    }

    void remove(Vertex v) {
        if (!lookup(v) || v == -1 || v == 1) {
            throw std::invalid_argument("remove precondition violated");
        }
        removed.push_back(v);
        vertices.erase(std::remove(vertices.begin(), vertices.end(), v), vertices.end());
        edges.erase(std::remove_if(edges.begin(), edges.end(), [v](const Edge& e) { return e.first == v || e.second == v; }), edges.end());
    }

private:
    std::vector<Vertex> vertices;
    std::vector<Vertex> removed;
    std::vector<Edge> edges;
};


// Replicable growth array
class RGA {
public:
    RGA(std::function<int()> now) {
        va.insert(Vertex(-1, -1));
        vr.insert(Vertex(-1, 0));
        edges[Vertex(-1, -1)].push_back(Vertex(-1, 0));
        this->now = now;
    }

    bool lookup(const Vertex& v) const {
        return va.count(v) && !vr.count(v);
    }

    bool before(const Vertex& u, const Vertex& v) const {
        if (!lookup(u) || !lookup(v)) {
            return false;
        }

        for (const auto& w : va) {
            if ((w == u && lookup(v)) ||
                (w == v && lookup(u)) ||
                (lookup(w) && edges.at(w).count(u) && edges.at(w).count(v))) {
                return true;
            }
        }

        return false;
    }

    std::optional<Vertex> successor(const Vertex& u) const {
        if (!lookup(u)) {
            return std::nullopt;
        }

        for (const auto& v : va) {
            if (before(u, v) && !before(v, u)) {
                return v;
            }
        }

        return std::nullopt;
    }

    Vertex decompose(const Vertex& u) const {
        return u;
    }

    bool addRight(const Vertex& u, int a) {
        int t = now();
        Vertex w = std::make_pair(a, t);

        if (lookup(w)) {
            return false;
        }

        va.insert(w);
        edges[u].insert(w);

        return true;
    }

    bool remove(const Vertex& w) {
        if (!lookup(w)) {
            return false;
        }

        vr.insert(w);
        va.erase(w);

        for (auto& [u, neighbors] : edges) {
            neighbors.erase(w);
        }

        return true;
    }

private:
    using Vertex = std::pair<int, int>;
    using Edge = std::pair<Vertex, Vertex>;
    std::unordered_set<Vertex> va;
    std::unordered_set<Vertex> vr;
    std::unordered_map<Vertex, std::unordered_set<Vertex>> edges;
    std::function<int()> now;
};



using Identifier = std::string;

// Continuous sequence
class ContSeq {
public:
    class Node {
    public:
        Node* left = nullptr;
        Node* right = nullptr;
        std::string data;
        Identifier id;

        Node(const std::string& value, const Identifier& id) : data(value), id(id) {}

        void insert(const std::string& value, const Identifier& id) {
            if (id < this->id) {
                if (left == nullptr) {
                    left = new Node(value, id);
                } else {
                    left->insert(value, id);
                }
            } else {
                if (right == nullptr) {
                    right = new Node(value, id);
                } else {
                    right->insert(value, id);
                }
            }
        }

        Node* lookup(const Identifier& id) {
            if (id == this->id) {
                return this;
            } else if (id < this->id) {
                if (left == nullptr) {
                    return nullptr;
                }
                return left->lookup(id);
            } else {
                if (right == nullptr) {
                    return nullptr;
                }
                return right->lookup(id);
            }
        }
    };

    Node* root = nullptr;

    void insert(const std::string& value, const Identifier& id) {
        if (root == nullptr) {
            root = new Node(value, id);
        } else {
            root->insert(value, id);
        }
    }

    Node* lookup(const Identifier& id) {
        if (root == nullptr) {
            return nullptr;
        }
        return root->lookup(id);
    }

    Identifier allocateIdentifierBetween(const Identifier& id1, const Identifier& id2) {
        if (id1.length() != 1 || id2.length() != 1) {
            return "";
        }

        char min_char = std::min(id1[0], id2[0]);
        char max_char = std::max(id1[0], id2[0]);

        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(min_char + 1, max_char - 1);

        char random_char = static_cast<char>(dis(gen));
        while (random_char == min_char || random_char == max_char) {
            random_char = static_cast<char>(dis(gen));
        }

        return std::string(1, random_char);
    }

    void addBetween(const std::string& value, const Identifier& id1, const Identifier& id2) {
        if (root == nullptr) {
            root = new Node(value, allocateIdentifierBetween(id1, id2));
        } else {
            root = addBetweenRecursive(root, value, id1, id2);
        }
    }

    void remove(const Identifier& id) {
        Node* node = lookup(id);
        if (node == nullptr) {
            throw std::runtime_error("Element not found in set");
        }

        removeNode(node);

        if (lookup(id) != nullptr) {
            throw std::runtime_error("Removal failed");
        }
    }

    void removeNode(Node* node) {
        if (node->left == nullptr) {
            Node* rightChild = node->right;
            *node = *node->right;
            delete rightChild;
        } else if (node->right == nullptr) {
            Node* leftChild = node->left;
            *node = *node->left;
            delete leftChild;
        } else {
            Node* successor = findSuccessor(node->right);
            std::swap(node->data, successor->data);
            std::swap(node->id, successor->id);
            removeNode(successor);
        }
    }

    Node* ContSeq::findSuccessor(Node* node) {
        if (node->left == nullptr) {
            return node;
        }
        return findSuccessor(node->left);
    }

private:
    Node* addBetweenRecursive(Node* current, const std::string& value, const Identifier& id1, const Identifier& id2) {
        if (current == nullptr) {
            return new Node(value, allocateIdentifierBetween(id1, id2));
        }

        Identifier newId = allocateIdentifierBetween(id1, current->id);
        if (!newId.empty()) {
            current->left = addBetweenRecursive(current->left, value, id1, newId);
        }

        newId = allocateIdentifierBetween(current->id, id2);
        if (!newId.empty()) {
            current->right = addBetweenRecursive(current->right, value, newId, id2);
        }

        return current;
    }
};


// Op-based observed-remove shopping cart
class ORCart {
private:
    using ISBN = std::string;
    using UniqueTag = std::string;
    using Payload = std::vector<std::tuple<ISBN, int, UniqueTag>>;

    static UniqueTag generateUniqueTag() {
        static std::random_device rd;
        static std::mt19937 gen(rd());
        static std::uniform_int_distribution<> dis(0, 255);

        std::string tag(8, ' ');
        std::generate(tag.begin(), tag.end(), std::bind(dis, std::ref(gen)));
        return tag;
    }

public:
    static Payload emptyPayload() {
        return Payload();
    }

    static int getQuantity(const Payload& payload, const ISBN& k) {
        int total = 0;
        for (const auto& [isbn, quantity, _] : payload) {
            if (isbn == k) {
                total += quantity;
            }
        }
        return total;
    }

    static Payload add(const Payload& payload, const ISBN& k, int n) {
        Payload newPayload;
        int existingQuantity = getQuantity(payload, k);
        UniqueTag newTag = generateUniqueTag();
        for (const auto& item : payload) {
            const auto& [isbn, quantity, tag] = item;
            if (isbn != k) {
                newPayload.emplace_back(isbn, quantity, tag);
            }
        }
        newPayload.emplace_back(k, n + existingQuantity, newTag);
        return newPayload;
    }

    static Payload remove(const Payload& payload, const ISBN& k) {
        Payload newPayload;
        for (const auto& item : payload) {
            const auto& [isbn, quantity, tag] = item;
            if (isbn != k) {
                newPayload.emplace_back(isbn, quantity, tag);
            }
        }
        return newPayload;
    }

    static void downstream(const Payload& payload) {
        for (const auto& [isbn, quantity, tag] : payload) {
            std::cout << "add(" << isbn << ", " << quantity << ", " << tag << ") has been delivered" << std::endl;
        }
    }

    static Payload upsertPrecondition(const Payload& payload, const ISBN& k, int n, const UniqueTag& alpha, const Payload& r) {
        Payload rSet;
        for (const auto& item : r) {
            const auto& [isbn, _, _] = item;
            if (isbn == k) {
                rSet.push_back(item);
            }
        }

        Payload union_ = payload;
        union_.insert(union_.end(), rSet.begin(), rSet.end());
        union_.emplace_back(k, n, alpha);
        std::sort(union_.begin(), union_.end());
        union_.erase(std::unique(union_.begin(), union_.end()), union_.end());
        return union_;
    }

    static Payload removeElementsObservedAtSource(const Payload& payload, const Payload& r) {
        std::unordered_set<std::tuple<ISBN, int, UniqueTag>> rSet(r.begin(), r.end());
        Payload newPayload;
        for (const auto& item : payload) {
            if (rSet.find(item) == rSet.end()) {
                newPayload.push_back(item);
            }
        }
        return newPayload;
    }
};