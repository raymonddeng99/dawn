#include <unordered_map>
#include <string>
#include <vector>

// On-line construction of suffix trees, Ukkoken '95

struct Node {
    int start, len;
    std::unordered_map<char, Node*> children;
    Node* suffixLink;
    Node* parent;

    Node(int start, int len, Node* parent) : start(start), len(len), suffixLink(nullptr), parent(parent) {}
};

Node* createNode(int start, int len, Node* parent) {
    return new Node(start, len, parent);
}

Node* splitNode(Node* node, int start, int len, char nodeLabel) {
    Node* newNode = createNode(start, len, node->parent);
    int remainingLen = node->len - len - 1;
    node->len = len;
    Node* child = createNode(node->start + len + 1, remainingLen, newNode);
    node->children[nodeLabel] = child;
    newNode->suffixLink = node->suffixLink;
    node->suffixLink = newNode;
    return newNode;
}

Node* findNode(Node* root, int start, int len, const std::string& s) {
    if (len == 0) return root;
    auto it = root->children.find(s[start]);
    if (it == root->children.end()) return nullptr;
    Node* child = it->second;
    int childLen = child->len;
    if (childLen >= len) return child;
    return findNode(child, child->start + childLen + 1, len - childLen - 1, s);
}

Node* followLinks(Node* node) {
    return node->suffixLink ? followLinks(node->suffixLink) : node;
}

void updateTree(const std::string& s, int i, Node* root) {
    int len = s.length();
    Node* node = followLinks(root);
    int remaining = len - i;
    if (remaining > 0) {
        Node* leaf = createNode(i, remaining, node);
        char nodeLabel = s[i];
        auto it = node->children.find(nodeLabel);
        if (it != node->children.end()) {
            Node* child = it->second;
            int start = child->start;
            int len = child->len;
            Node* foundNode = findNode(root, start, len, s);
            if (foundNode && s.substr(start, len) == s.substr(i, len)) {
                updateTree(s, i + len + 1, child);
            } else if (foundNode) {
                Node* newNode = splitNode(foundNode, start, len, nodeLabel);
                Node* leaf = createNode(i, remaining, newNode);
                newNode->children[nodeLabel] = leaf;
            } else {
                node->children[nodeLabel] = leaf;
            }
        } else {
            node->children[nodeLabel] = leaf;
        }
    }
}

Node* buildSuffixTree(const std::string& s) {
    Node* root = createNode(0, 0, nullptr);
    for (int i = 0; i < s.length(); i++) {
        updateTree(s, i, root);
    }
    return root;
}