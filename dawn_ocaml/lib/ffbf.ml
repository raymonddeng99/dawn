module BloomFilter = struct
  type t = {
    size : int;
    vector : Bytes.t;
    hash_funcs : (Bytes.t -> int) list;
  }

  let create size hash_funcs =
    let vector = Bytes.make (size / 8 + 1) '\x00' in
    { size; vector; hash_funcs }

  let add filter element =
    let byte_element = Bytes.of_string element in
    List.iter
      (fun hash_func ->
         let bit_index = hash_func byte_element mod filter.size in
         Bytes.set filter.vector (bit_index / 8)
           (Char.chr
              (Char.code
                 (Bytes.get filter.vector (bit_index / 8))
              lor (1 lsl (bit_index mod 8)))))
      filter.hash_funcs

  let mem filter element =
    let byte_element = Bytes.of_string element in
    List.for_all
      (fun hash_func ->
         let bit_index = hash_func byte_element mod filter.size in
         Char.code (Bytes.get filter.vector (bit_index / 8))
         land (1 lsl (bit_index mod 8))
         <> 0)
      filter.hash_funcs
end