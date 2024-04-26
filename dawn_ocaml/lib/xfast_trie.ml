open Base

module XFastTrie = struct
  type t = {
    bits : bool array;
    ones : int list;
    size : int;
  }

  let create w =
    let u = 1 lsl w in
    let bits = Array.make (2 * u - 1) false in
    let ones = [] in
    { bits; ones; size = 0 }

  let insert trie x =
    let rec insert_aux trie x i =
      if i >= Array.length trie.bits then
        { trie with size = trie.size + 1 }
      else
        let bit = x land (1 lsl i) <> 0 in
        let index = (1 lsl (Array.length trie.bits - 1 - i)) in
        trie.bits.(index - 1) <- trie.bits.(index - 1) || bit;
        if bit then
          insert_aux { trie with ones = index :: trie.ones } x (i + 1)
        else
          insert_aux trie x (i + 1)
    in
    insert_aux trie x 0

  let predecessor trie x =
    let rec pred_aux trie x i =
      if i >= Array.length trie.bits then None
      else
        let index = (1 lsl (Array.length trie.bits - 1 - i)) in
        if trie.bits.(index - 1) then
          let left_child = 2 * index in
          let right_child = left_child + 1 in
          if trie.bits.(right_child - 1) then
            pred_aux trie x (i + 1)
          else
            Some (List.hd trie.ones)
        else
          pred_aux trie x (i + 1)
    in
    match pred_aux trie x 0 with
    | None -> None
    | Some index ->
       let rec find_predecessor index =
         if index = 0 then None
         else if List.mem index trie.ones then Some (index - 1)
         else find_predecessor (index / 2)
       in
       find_predecessor (index - 1)
end