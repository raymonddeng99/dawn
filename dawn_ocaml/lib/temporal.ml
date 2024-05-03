open Base

(* Deque implementation using batched concatenation *)
module Deque : sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add_front : 'a -> 'a t -> 'a t
  val add_back : 'a -> 'a t -> 'a t
  val take_front : 'a t -> ('a * 'a t) option
  val take_back : 'a t -> ('a * 'a t) option
end = struct
  type 'a node =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a

  type 'a t = {
    front : 'a node list;
    rear : 'a node list;
  }

  let empty = { front = []; rear = [] }

  let is_empty { front; rear } = front = [] && rear = []

  let add_front x { front; rear } =
    let rec rev_nodes acc = function
      | [] -> acc
      | One x :: xs -> rev_nodes (Four (x, x, x, x) :: acc) xs
      | Two (x, y) :: xs -> rev_nodes (Three (x, x, y) :: Four (y, y, y, y) :: acc) xs
      | Three (x, y, z) :: xs -> rev_nodes (Two (x, y) :: Two (y, z) :: Four (z, z, z, z) :: acc) xs
      | Four (w, x, y, z) :: xs -> rev_nodes (One w :: One x :: One y :: One z :: acc) xs
    in
    let front = rev_nodes [One x] rear in
    { front; rear = [] }

  let add_back x { front; rear } =
    let rec rev_nodes acc = function
      | [] -> acc
      | One x :: xs -> rev_nodes (Four (x, x, x, x) :: acc) xs
      | Two (x, y) :: xs -> rev_nodes (Three (x, y, y) :: Four (x, x, x, x) :: acc) xs
      | Three (x, y, z) :: xs -> rev_nodes (Two (x, y) :: Two (z, z) :: Four (y, y, y, y) :: acc) xs
      | Four (w, x, y, z) :: xs -> rev_nodes (One w :: One x :: One y :: One z :: acc) xs
    in
    { front; rear = rev_nodes [One x] front }

  let take_front { front; rear } =
    match front with
    | [] -> None
    | One x :: front' ->
       let rear' = match rear with
         | [] -> []
         | [One x] -> []
         | nodes -> List.rev nodes
       in
       Some (x, { front = front'; rear = rear' })
    | Two (x, y) :: front' ->
       let rear' = match rear with
         | [] -> []
         | [Two (x, y)] -> [One x; One y]
         | One x :: nodes -> List.rev (Two (x, y) :: nodes)
         | _ -> List.rev rear
       in
       Some (x, { front = One y :: front'; rear = rear' })
    | Three (x, y, z) :: front' ->
       let rear' = match rear with
         | [] -> []
         | [Three (x, y, z)] -> [One x; One y; One z]
         | One x :: nodes -> List.rev (Three (x, y, z) :: nodes)
         | Two (x, y) :: nodes -> List.rev (Two (x, y) :: Two (y, z) :: nodes)
         | _ -> List.rev rear
       in
       Some (x, { front = Two (y, z) :: front'; rear = rear' })
    | Four (w, x, y, z) :: front' ->
       let rear' = match rear with
         | [] -> []
         | [Four (w, x, y, z)] -> [One w; One x; One y; One z]
         | One x :: nodes -> List.rev (Four (w, x, y, z) :: nodes)
         | Two (x, y) :: nodes -> List.rev (Two (x, y) :: Two (y, z) :: Two (z, w) :: nodes)
         | Three (x, y, z) :: nodes -> List.rev (Three (x, y, z) :: Two (z, w) :: One w :: nodes)
         | _ -> List.rev rear
       in
       Some (w, { front = Three (x, y, z) :: front'; rear = rear' })

  let take_back { front; rear } =
    let front', x = match List.rev front with
      | [] -> [], None
      | One x :: front' -> front', Some x
      | Two (x, y) :: front' -> List.rev (One y :: front'), Some x
      | Three (x, y, z) :: front' -> List.rev (Two (y, z) :: front'), Some x
      | Four (w, x, y, z) :: front' -> List.rev (Three (x, y, z) :: front'), Some w
    in
    match x with
    | None -> None
    | Some x -> Some (x, { front = front'; rear })
end