use std::collections::HashMap;

pub fn hash(str: &str) -> u32 {
    str.chars().map(|c| c as u32).sum()
}

pub fn hamming_distance(str1: &str, str2: &str) -> u32 {
    str1.chars()
        .zip(str2.chars())
        .filter(|&(c1, c2)| c1 != c2)
        .count() as u32
}

pub fn lsh_func(k: usize, l: usize, str: &str) -> Vec<u32> {
    let len = str.len();
    let mut hashes = Vec::with_capacity(k);
    for i in 0..k {
        let start = i * len / k;
        let end = start + l * len / k;
        let hash_val = hash(&str[start..end]);
        hashes.push(hash_val);
    }
    hashes
}

pub fn build_lsh_table(k: usize, l: usize, strings: &[&str]) -> HashMap<u32, Vec<&str>> {
    let mut table = HashMap::new();
    for &str in strings {
        let hashes = lsh_func(k, l, str);
        for hash_val in hashes {
            table.entry(hash_val).or_insert_with(Vec::new).push(str);
        }
    }
    table
}

pub fn query_lsh_table(k: usize, l: usize, table: &HashMap<u32, Vec<&str>>, query_str: &str) -> Vec<&str> {
    let hashes = lsh_func(k, l, query_str);
    let mut candidates = Vec::new();
    for hash_val in hashes {
        if let Some(bucket) = table.get(&hash_val) {
            for &str in bucket {
                if hamming_distance(str, query_str) <= l as u32 {
                    candidates.push(str);
                }
            }
        }
    }
    candidates
}