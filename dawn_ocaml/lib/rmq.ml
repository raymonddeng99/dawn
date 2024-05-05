open Base

module RMQ = struct
  type tree = {
    n : int;
    values : int array;
    euler : int array;
    first : int array;
    rmq : int array array;
  }

  let log2 n =
    let rec aux n acc =
      if n <= 1 then acc else aux (n lsr 1) (acc + 1)
    in
    aux n 0

  let precompute_rmq tree =
    let n = Array.length tree.euler in
    let k = log2 n + 1 in
    let rmq = Array.make_matrix k n max_int in
    for i = 0 to n - 1 do
      rmq.(0).(i) <- i
    done;
    for j = 1 to k - 1 do
      for i = 0 to n - (1 lsr j) do
        let x = rmq.(j-1).(i) in
        let y = rmq.(j-1).(i + (1 lsr (j-1))) in
        rmq.(j).(i) <- if tree.euler.(x) < tree.euler.(y) then x else y
      done
    done;
    { tree with rmq }

  let query tree l r =
    let l, r = min l r, max l r in
    let k = log2 (r - l + 1) in
    let x = tree.rmq.(k).(tree.first.(l)) in
    let y = tree.rmq.(k).(tree.first.(r) - (1 lsr k) + 1) in
    tree.values.(min tree.euler.(x) tree.euler.(y))

  let from_tree values =
    let n = Array.length values in
    let euler = Array.make (2 * n) 0 in
    let first = Array.make (2 * n) 0 in
    let rec build_euler tree i =
      let j = i + 1 in
      euler.(i) <- tree;
      first.(i) <- j;
      if j < 2 * n then build_euler (tree + 1) j
    in
    build_euler 0 0;
    let tree = { n; values; euler; first; rmq = [||] } in
    precompute_rmq tree
end