open Base

module EulerTourTree = struct
  type 'a tree = Node of 'a * 'a tree * 'a tree | Null

  let rec size t =
    match t with
    | Null -> 0
    | Node (_, left, right) -> 1 + size left + size right

  let build_euler_tour t =
    let n = ref 0 in
    let euler_tour = Array.make (2 * size t - 1) (-1) in
    let rec traverse t i =
      match t with
      | Null -> ()
      | Node (x, left, right) ->
          euler_tour.(i) <- x;
          let j = ref (i + 1) in
          traverse left !j;
          incr j;
          traverse right !j;
          incr j;
          euler_tour.(!j) <- x
    in
    traverse t 0;
    euler_tour

  let build_euler_tour_tree euler_tour =
    let n = Array.length euler_tour in
    let first = Array.make n (-1) in
    let last = Array.make n (-1) in
    let level = Array.make n 0 in
    let stack = Stack.create () in
    for i = 0 to n - 1 do
      if Stack.is_empty stack then (
        Stack.push i stack;
        level.(i) <- 0
      ) else (
        let j = Stack.top stack in
        if euler_tour.(i) = euler_tour.(j) then (
          Stack.pop stack |> ignore;
          last.(j) <- i
        ) else (
          Stack.push i stack;
          level.(i) <- level.(j) + 1;
          first.(i) <- j
        )
      )
    done;
    (euler_tour, first, last, level)
end