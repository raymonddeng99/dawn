#include <iostream>
#include <unordered_map>
#include <vector>
#include <string>
#include <algorithm>

class LSHHash {
public:
    static int hash(const std::string& str) {
        int sum = 0;
        for (char c : str) {
            sum += static_cast<int>(c);
        }
        return sum;
    }

    static int hammingDistance(const std::string& str1, const std::string& str2) {
        int distance = 0;
        int len = std::min(str1.length(), str2.length());
        for (int i = 0; i < len; i++) {
            if (str1[i] != str2[i]) {
                distance++;
            }
        }
        return distance;
    }

    static std::vector<int> lshFunc(int k, int l, const std::string& str) {
        std::vector<int> hashes(k);
        int len = str.length();
        for (int i = 0; i < k; i++) {
            int start = i * len / k;
            int end = start + l * len / k;
            hashes[i] = hash(str.substr(start, end - start));
        }
        return hashes;
    }

    static std::unordered_map<int, std::vector<std::string>> buildLSHTable(int k, int l, const std::vector<std::string>& strings) {
        std::unordered_map<int, std::vector<std::string>> table;
        for (const auto& str : strings) {
            std::vector<int> hashes = lshFunc(k, l, str);
            for (int hashVal : hashes) {
                table[hashVal].push_back(str);
            }
        }
        return table;
    }

    static std::vector<std::string> queryLSHTable(int k, int l, const std::unordered_map<int, std::vector<std::string>>& table, const std::string& queryStr) {
        std::vector<std::string> candidates;
        std::vector<int> hashes = lshFunc(k, l, queryStr);
        for (int hashVal : hashes) {
            auto it = table.find(hashVal);
            if (it != table.end()) {
                for (const auto& str : it->second) {
                    if (hammingDistance(str, queryStr) <= l) {
                        candidates.push_back(str);
                    }
                }
            }
        }
        return candidates;
    }
};