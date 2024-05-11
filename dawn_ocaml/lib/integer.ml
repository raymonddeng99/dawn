open Base

module SignatureSort = struct
  let rec merge_sort cmp arr =
    let aux = Array.copy arr in
    let swap a b =
      let temp = aux.(a) in
      aux.(a) <- aux.(b);
      aux.(b) <- temp
    in
    let rec merge left pivot right =
      let rec swap a b =
        if cmp aux.(b) aux.(a) >= 0 then ()
        else (
          swap a b;
          let next_a = a + 1 and next_b = b + 1 in
          if next_a < pivot then swap next_a left;
          if next_b < right then swap next_b pivot)
      in
      for i = left to pivot - 1 do
        swap i left
      done;
      let next_left = ref pivot in
      for i = pivot to right - 1 do
        let next_pivot = !next_left in
        swap i next_pivot;
        incr next_left
      done
    in
    let rec split left right =
      let len = right - left in
      if len < 2 then ()
      else (
        let pivot = left + len / 2 in
        split left pivot;
        split pivot right;
        merge left pivot right)
    in
    split 0 (Array.length arr);
    aux

  let sort cmp arr = merge_sort cmp (Array.copy arr)
end