open Base

module CompactSuffixArray = struct
  let rank_suffix text rank_of_char rank i =
    let n = String.length text in
    let rec loop j r =
      if j >= n then r
      else loop (j + 1) (r * rank_of_char + rank text.[j])
    in
    loop i 0

  let compute_rank_of_char text =
    let n = String.length text in
    let rank_of_char = Array.make 256 0 in
    let used = Array.make 256 false in
    for i = 0 to n - 1 do
      let c = Char.code text.[i] in
      if not used.(c) then (
        let rank = Array.fold_left (fun r v -> if v then r + 1 else r) 0 used in
        rank_of_char.(c) <- rank;
        used.(c) <- true)
    done;
    rank_of_char

  let compact_suffix_array text =
    let n = String.length text in
    let rank_of_char = compute_rank_of_char text in
    let rank = Array.make n 0 in
    for i = 0 to n - 1 do
      rank.(i) <- rank_suffix text rank_of_char n i
    done;
    Array.sort (fun i j -> compare rank.(i) rank.(j)) (Array.init n (fun i -> i))
end