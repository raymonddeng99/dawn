#include <algorithm>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

std::vector<std::string> get_ngrams(int n, const std::string& s) {
    std::vector<std::string> ngrams;
    for (int i = 0; i <= (int)s.length() - n; ++i) {
        ngrams.push_back(s.substr(i, n));
    }
    return ngrams;
}

std::unordered_map<std::string, int> count_3gram_frequencies(const std::vector<std::string>& sample_keys) {
    std::unordered_map<std::string, int> frequencies;
    for (const auto& key : sample_keys) {
        for (const auto& ngram : get_ngrams(3, key)) {
            ++frequencies[ngram];
        }
    }
    return frequencies;
}

std::vector<std::pair<std::string, std::optional<std::string>>> select_3gram_intervals(const std::vector<std::string>& sample_keys, int dict_size) {
    auto frequencies = count_3gram_frequencies(sample_keys);
    std::vector<std::pair<std::string, int>> sorted_frequencies(frequencies.begin(), frequencies.end());
    std::sort(sorted_frequencies.begin(), sorted_frequencies.end(), [](const auto& a, const auto& b) { return a.second > b.second; });

    std::vector<std::string> selected_ngrams;
    selected_ngrams.reserve(dict_size / 2);
    for (int i = 0; i < dict_size / 2; ++i) {
        selected_ngrams.push_back(sorted_frequencies[i].first);
    }
    std::sort(selected_ngrams.begin(), selected_ngrams.end());

    std::vector<std::pair<std::string, std::optional<std::string>>> intervals;
    std::string last_ngram;
    for (const auto& ngram : selected_ngrams) {
        std::string start = last_ngram;
        if (!start.empty()) {
            start.back() = start.back() + 1;
        }
        intervals.emplace_back(start, ngram);
        last_ngram = ngram;
    }
    return intervals;
}