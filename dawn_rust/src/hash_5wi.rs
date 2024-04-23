use rand::Rng;

fn universal_hash(a: u32, b: u32, c: u32, d: u32, e: u32, key: u32) -> u32 {
    let m = 1 << 32;
    let hash = (a * key
        + b * key.overflowing_mul(key).0
        + c * key.overflowing_mul(key.overflowing_mul(key).0).0
        + d * key.overflowing_mul(key.overflowing_mul(key.overflowing_mul(key).0).0).0
        + e * key.overflowing_mul(key.overflowing_mul(key.overflowing_mul(key.overflowing_mul(key).0).0).0).0)
        % m;
    hash
}

fn linear_probe<T>(table: &mut [Option<T>], key: u32) -> usize {
    let m = table.len();
    let mut rng = rand::thread_rng();
    let a = rng.gen_range(0..m as u32);
    let b = rng.gen_range(0..m as u32);
    let c = rng.gen_range(0..m as u32);
    let d = rng.gen_range(0..m as u32);
    let e = rng.gen_range(0..m as u32);
    let hash = universal_hash(a, b, c, d, e, key) as usize;
    for i in 0.. {
        let index = (hash + i) % m;
        if table[index].is_none() {
            return index;
        }
    }
    unreachable!()
}