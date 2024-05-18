#include <algorithm>
#include <string>
#include <vector>

class BWT {
public:
    static std::pair<std::string, int> encode(const std::string& s) {
        int n = s.length();
        std::vector<std::string> rotations(n);
        for (int i = 0; i < n; i++) {
            rotations[i] = s.substr(i) + s.substr(0, i);
        }
        std::sort(rotations.begin(), rotations.end());
        std::string encoded(n, '\0');
        for (int i = 0; i < n; i++) {
            encoded[i] = rotations[i][n - 1];
        }
        int idx = std::find(rotations.begin(), rotations.end(), s) - rotations.begin();
        return {encoded, idx};
    }

    static std::string decode(const std::string& encoded, int idx) {
        int n = encoded.length();
        std::vector<std::string> sortedRotations(n);
        for (int i = 0; i < n; i++) {
            sortedRotations[i] = encoded.substr(i, 1) + lastChar(encoded, i);
        }
        std::sort(sortedRotations.begin(), sortedRotations.end());
        std::string decoded(n, '\0');
        for (int i = 0; i < n; i++) {
            decoded[i] = sortedRotations[i][n - 1];
        }
        return decoded.substr(idx) + decoded.substr(0, idx);
    }

private:
    static std::string lastChar(const std::string& s, int i) {
        std::string temp = s.substr(0, i) + s.substr(i + 1);
        std::sort(temp.begin(), temp.end());
        return std::string(1, temp[temp.length() - 1]);
    }
};