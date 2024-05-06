open Base

module LSHHash = struct
  let hash str =
    let sum = ref 0 in
    for i = 0 to String.length str - 1 do
      sum := !sum + int_of_char str.[i]
    done;
    !sum

  let hamming_distance str1 str2 =
    let distance = ref 0 in
    let len1 = String.length str1 in
    let len2 = String.length str2 in
    let len = min len1 len2 in
    for i = 0 to len - 1 do
      if str1.[i] <> str2.[i] then incr distance
    done;
    !distance

  let lsh_func k l str =
    let hashes = Array.make k 0 in
    let len = String.length str in
    for i = 0 to k - 1 do
      let hash_val = hash (String.sub str (i * len / k) (l * len / k)) in
      hashes.(i) <- hash_val
    done;
    hashes

  let build_lsh_table k l strings =
    let lsh_table = Hashtbl.create 1000 in
    List.iter (fun str ->
      let hashes = lsh_func k l str in
      Array.iter (fun hash_val ->
        let bucket = try Hashtbl.find lsh_table hash_val with Not_found -> [] in
        Hashtbl.replace lsh_table hash_val (str :: bucket)
      ) hashes
    ) strings;
    lsh_table

  let query_lsh_table k l lsh_table query_str =
    let hashes = lsh_func k l query_str in
    let candidates = ref [] in
    Array.iter (fun hash_val ->
      let bucket = try Hashtbl.find lsh_table hash_val with Not_found -> [] in
      candidates := List.rev_append bucket !candidates
    ) hashes;
    List.filter (fun str -> hamming_distance str query_str <= l) !candidates

end