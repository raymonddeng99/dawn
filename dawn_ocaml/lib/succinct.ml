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

(* Succinct static data structures, Jacobsen '89 *)
module BitVector = struct
  let k = 6

  let rank_naive bits i =
    let rec aux j cnt =
      if j > i then cnt
      else aux (j + 1) (cnt + if List.nth bits j then 1 else 0)
    in
    aux 0 0

  let select_naive bits i =
    let rec aux j cnt =
      if cnt = i then j
      else if j >= List.length bits then -1
      else aux (j + 1) (cnt + if List.nth bits j then 1 else 0)
    in
    aux 0 0

  let create bits =
    let n = List.length bits in
    let rank_table = Array.make (n + 1) 0 in
    let select_table = Array.make (n + 1) (-1) in
    for i = 0 to n do
      rank_table.(i) <-
        if i mod (1 lsl k) = 0 then 0
        else rank_table.(i - (i mod (1 lsl k))) + rank_naive bits i - rank_naive bits (i - (i mod (1 lsl k)));
      select_table.(i) <-
        if i mod (1 lsl k) = 0 then -1
        else select_table.(i - (i mod (1 lsl k))) + select_naive bits i - select_naive bits (i - (i mod (1 lsl k)))
    done;
    (bits, rank_table, select_table)

  let rank (bits, rank_table, _) i = rank_table.(i)

  let select (bits, _, select_table) i =
    let rec aux l r =
      if l > r then -1
      else
        let m = l + (r - l) / 2 in
        if rank (bits, rank_table, select_table) m < i then aux (m + 1) r
        else aux l (m - 1)
    in
    aux 0 (List.length bits)
end