pub fn rank_suffix(text: &str, rank_of_char: &[u32], n: usize, i: usize) -> u32 {
    text.chars()
        .skip(i)
        .fold(0, |r, c| r * (rank_of_char.len() as u32) + rank_of_char[c as usize])
}

pub fn compute_rank_of_char(text: &str) -> Vec<u32> {
    let mut rank_of_char = vec![0u32; 256];
    let mut used = vec![false; 256];
    let mut rank = 0u32;
    for c in text.chars() {
        if !used[c as usize] {
            rank_of_char[c as usize] = rank;
            rank += 1;
            used[c as usize] = true;
        }
    }
    rank_of_char
}

pub fn suffix_array(text: &str) -> Vec<usize> {
    let n = text.len();
    let rank_of_char = compute_rank_of_char(text);
    let mut ranks: Vec<u32> = (0..n).map(|i| rank_suffix(text, &rank_of_char, n, i)).collect();
    let mut indices: Vec<usize> = (0..n).collect();
    indices.sort_by(|&i, &j| ranks[i].cmp(&ranks[j]));
    indices
}