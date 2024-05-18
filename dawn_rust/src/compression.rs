use std::cmp::Ordering;

fn last_char(s: &str) -> char {
    let mut rotations: Vec<_> = s.chars().map(|c| format!("{}{}", &s[s.char_indices().nth(1).map(|i| i.0).unwrap_or(s.len())..], &s[..s.char_indices().nth(1).map(|i| i.0).unwrap_or(s.len())])).collect();
    rotations.sort();
    rotations.last().unwrap().chars().next().unwrap()
}

fn find_index(s: &str, sorted_rotations: &[String]) -> usize {
    sorted_rotations.iter().position(|r| r == s).unwrap()
}

pub fn encode(s: &str) -> (String, usize) {
    let mut rotations: Vec<_> = s.chars().enumerate().map(|(i, c)| format!("{}{}", &s[s.char_indices().nth(i).map(|i| i.0).unwrap_or(s.len())..], &s[..s.char_indices().nth(i).map(|i| i.0).unwrap_or(s.len())])).collect();
    rotations.sort();
    let encoded: String = rotations.iter().map(|r| r.chars().last().unwrap()).collect();
    let idx = find_index(s, &rotations);
    (encoded, idx)
}

pub fn decode(encoded: &str, idx: usize) -> String {
    let mut sorted_rotations: Vec<_> = encoded.chars().enumerate().map(|(i, c)| format!("{}{}", c, last_char(&encoded[..i] + &encoded[i+1..]))).collect();
    sorted_rotations.sort();
    let decoded: String = sorted_rotations.iter().map(|r| r.chars().last().unwrap()).collect();
    decoded.chars().cycle().skip(idx).take(encoded.len()).collect()
}