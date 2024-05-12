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

(* Partially retroactive priority queue *)
module PriorityQueue = struct
  type 'a t = {
    data: ('a * int) list;
    mutable time: int;
  }

  exception Empty

  let create () = { data = []; time = 0 }

  let is_empty q = q.data = []

  let insert q x =
    q.time <- q.time + 1;
    q.data <- (x, q.time) :: q.data

  let find_min q =
    if is_empty q then
      raise Empty
    else
      List.hd (List.sort (fun (_, t1) (_, t2) -> compare t1 t2) q.data)

  let delete_min q =
    if is_empty q then
      raise Empty
    else
      let (min_val, _) = find_min q in
      q.data <- List.filter (fun (x, _) -> x <> min_val) q.data;
      min_val

  let retroactive_update q t x =
    let data' = List.map (fun (v, t') -> if t' <= t then (x, t') else (v, t')) q.data in
    { q with data = data' }
end

(* Simple Confluently Persistent Catenable Lists, Tarjan et al *)
module Persistent_List = struct
  type 'a node = {
    mutable value : 'a;
    mutable left : 'a node option;
    mutable right : 'a node option;
  }

  type 'a t = {
    left : 'a node option;
    right : 'a node option;
  }

  let empty = { left = None; right = None }

  let is_empty list = list.left = None && list.right = None

  let make_node value left right =
    { value; left; right }

  let catenate left right =
    match left.right, right.left with
    | Some x, Some y when x == y ->
       { left = left.left; right = right.right }
    | _, _ ->
       let node = make_node (None, None) left.right right.left in
       { left = left.left; right = right.right; }

  let singleton value =
    let node = make_node value None None in
    { left = Some node; right = Some node }

  let cons value list =
    let node = make_node value None list.left in
    { left = Some node; right = list.right }

  let head list =
    match list.left with
    | Some node -> Some node.value
    | None -> None

  let tail list =
    match list.left, list.right with
    | Some node, Some right_node when node == right_node -> empty
    | _, Some right_node ->
       { left = (match right_node.left with
                 | Some node -> Some node
                 | None -> None);
         right = right_node.right }
    | _, None -> empty
end