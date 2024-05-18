open Base

module BWT = struct
  let last_char s =
    let n = String.length s in
    let sorted_rotations = Array.init n (fun i -> String.sub s i (n - i) ^ String.sub s 0 i) in
    Array.sort compare sorted_rotations;
    sorted_rotations.(n - 1).[0]

  let find_index s =
    let n = String.length s in
    let sorted_rotations = Array.init n (fun i -> String.sub s i (n - i) ^ String.sub s 0 i) in
    Array.sort compare sorted_rotations;
    Array.to_list sorted_rotations |> List.assoc s

  let encode s =
    let n = String.length s in
    let sorted_rotations = Array.init n (fun i -> String.sub s i (n - i) ^ String.sub s 0 i) in
    Array.sort compare sorted_rotations;
    let encoded = String.concat "" (Array.to_list (Array.map (fun x -> String.make 1 x.[n - 1]) sorted_rotations)) in
    let idx = find_index s in
    (encoded, idx)

  let decode (encoded, idx) =
    let n = String.length encoded in
    let sorted_rotations = Array.init n (fun i -> String.sub encoded i 1 ^ String.make 1 (last_char (String.sub encoded 0 i ^ String.make 1 encoded.[i]))) in
    Array.sort compare sorted_rotations;
    let decoded = Array.fold_left (fun acc x -> acc ^ String.make 1 x.[n - 1]) "" sorted_rotations in
    String.sub decoded idx n
end