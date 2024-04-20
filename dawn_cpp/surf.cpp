#include <unordered_map>
#include <string>
#include <vector>

using Node = std::unordered_map<char, Node>;

Node build(const std::vector<std::string>& keys) {
    Node root;
    for (const auto& key : keys) {
        auto curr = &root;
        for (char c : key) {
            auto it = curr->find(c);
            if (it == curr->end()) {
                curr->emplace(c, Node());
                curr = &curr->at(c);
            } else {
                curr = &it->second;
            }
        }
    }
    return root;
}

bool lookup(const Node& trie, const std::string& key) {
    const Node* curr = &trie;
    for (char c : key) {
        auto it = curr->find(c);
        if (it == curr->end()) {
            return false;
        }
        curr = &it->second;
    }
    return true;
}