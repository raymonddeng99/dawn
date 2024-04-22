open Base

module SymbolSelector = struct
  let get_ngrams n s =
    let len = String.length s in
    let rec loop i acc =
      if i + n > len then List.rev acc
      else
        let ngram = String.sub s i n in
        loop (i + 1) (ngram :: acc)
    in
    loop 0 []

  let count_3gram_frequencies sample_keys =
    let trie = Hashtbl.create 1000 in
    List.iter
      (fun key ->
        let ngrams = get_ngrams 3 key in
        List.iter (fun ngram -> Hashtbl.replace trie ngram ((Hashtbl.find_opt trie ngram |> Option.value ~default:0) + 1)) ngrams)
      sample_keys;
    trie

  let select_3gram_intervals sample_keys dictionary_size =
    let frequencies = count_3gram_frequencies sample_keys in
    let sorted_frequencies = Hashtbl.fold (fun ngram freq acc -> (ngram, freq) :: acc) frequencies [] |> List.sort (fun (_, freq1) (_, freq2) -> freq2 - freq1) in
    let selected_ngrams = List.take (dictionary_size / 2) sorted_frequencies |> List.map fst in
    let intervals =
      List.sort String.compare selected_ngrams
      |> List.fold_left
           (fun acc ngram ->
             match acc with
             | [] -> [ngram, None]
             | (last_ngram, _) :: rest ->
                 let start = last_ngram ^ String.make 1 (char_of_int (int_of_char (String.get last_ngram (String.length last_ngram - 1)) + 1)) in
                 let end_ = ngram in
                 (start, Some end_) :: (ngram, None) :: rest)
           []
    in
    intervals
end