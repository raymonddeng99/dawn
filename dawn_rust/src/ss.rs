use std::collections::HashMap;

fn get_ngrams(n: usize, s: &str) -> Vec<&str> {
    let mut ngrams = Vec::new();
    for i in 0..s.len() - n + 1 {
        ngrams.push(&s[i..i + n]);
    }
    ngrams
}

fn count_3gram_frequencies(sample_keys: &[&str]) -> HashMap<&str, usize> {
    let mut frequencies = HashMap::new();
    for key in sample_keys {
        for ngram in get_ngrams(3, key) {
            *frequencies.entry(ngram).or_insert(0) += 1;
        }
    }
    frequencies
}

fn select_3gram_intervals(sample_keys: &[&str], dict_size: usize) -> Vec<(&str, Option<&str>)> {
    let mut frequencies = count_3gram_frequencies(sample_keys);
    let mut sorted_frequencies: Vec<_> = frequencies.iter().collect();
    sorted_frequencies.sort_by(|a, b| b.1.cmp(a.1));

    let selected_ngrams: Vec<_> = sorted_frequencies
        .iter()
        .take(dict_size / 2)
        .map(|(&ngram, _)| ngram)
        .collect();

    let mut intervals = Vec::new();
    let mut last_ngram = "";
    for ngram in selected_ngrams.iter().sorted() {
        let start = {
            let mut chars: Vec<_> = last_ngram.chars().collect();
            if !chars.is_empty() {
                chars.last_mut().unwrap().next_code();
            }
            chars.iter().collect()
        };
        intervals.push((start, Some(ngram)));
        last_ngram = ngram;
    }
    intervals
}