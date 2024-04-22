from collections import Counter

def get_ngrams(n, s):
    return [s[i:i+n] for i in range(len(s) - n + 1)]

def count_3gram_frequencies(sample_keys):
    frequencies = Counter()
    for key in sample_keys:
        frequencies.update(get_ngrams(3, key))
    return frequencies

def select_3gram_intervals(sample_keys, dict_size):
    frequencies = count_3gram_frequencies(sample_keys)
    sorted_frequencies = sorted(frequencies.items(), key=lambda x: x[1], reverse=True)
    selected_ngrams = [ngram for ngram, _ in sorted_frequencies[:dict_size // 2]]
    selected_ngrams.sort()

    intervals = []
    last_ngram = ""
    for ngram in selected_ngrams:
        start = last_ngram
        if start:
            start = start[:-1] + chr(ord(start[-1]) + 1)
        intervals.append((start, ngram))
        last_ngram = ngram

    return intervals