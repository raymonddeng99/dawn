(* CRDTs *)

(* 
Specification: CRDTs = state-based | op-based

State-based require states form monotonic lattice and merge computes LUB (least upper bound)
Op-based require delivery order exists and concurrent updates commute
*)

open Base

type counterOp = Increment | Decrement
module Counter = struct
  type t = int * (counterOp list)

  let initial = (0, [])

  let apply (op, (value, ops)) =
    let new_value =
      match op with
      | Increment -> value + 1
      | Decrement -> value - 1
    in
    (new_value, ops)

  let merge (value1, ops1) (value2, ops2) =
    let merged_ops = ops1 @ ops2 in
    let final_value = List.fold_left (fun v op -> apply (op, (v, [])) |> fst) (max value1 value2) merged_ops in
    (final_value, [])

  let downstream () = []

  let update operation (value, ops) =
    let new_ops = operation :: ops in
    (value, new_ops)
end

(* State based increment-only counter *)
module GCounter = struct
  type t = int array

  let create size = Array.make size 0

  let update t i =
    if i < 0 || i >= Array.length t then
      invalid_arg "Index out of bounds"
    else
      t.(i) <- t.(i) + 1

  let query t i =
    if i < 0 || i >= Array.length t then
      invalid_arg "Index out of bounds"
    else
      t.(i)

  let compare t1 t2 =
    if Array.length t1 <> Array.length t2 then
      invalid_arg "Vectors have different lengths"
    else
      let rec loop i =
        if i >= Array.length t1 then
          0
        else if t1.(i) = t2.(i) then
          loop (i + 1)
        else
          compare t1.(i) t2.(i)
      in
      loop 0

  let merge t1 t2 =
    if Array.length t1 <> Array.length t2 then
      invalid_arg "Vectors have different lengths"
    else
      let t = Array.copy t1 in
      for i = 0 to Array.length t - 1 do
        t.(i) <- max t1.(i) t2.(i)
      done;
      t
end


(* State-based PN Counter *)
module PNCounter = struct
  type payload = int array
  type state = {
    p : payload;
    n : payload;
  }

  let initialize n p n =
    let zeros = Array.make n 0 in
    {p = Array.copy p; n = Array.copy n}

  let increment state =
    let g = myID () in
    state.p.(g) <- state.p.(g) + 1;
    state

  let decrement state =
    let g = myID () in
    state.n.(g) <- state.n.(g) + 1;
    state

  let value state =
    let sum_p = Array.fold_left (+) 0 state.p in
    let sum_n = Array.fold_left (+) 0 state.n in
    sum_p - sum_n

  let compare x y =
    let b = ref true in
    for i = 0 to Array.length x.p - 1 do
      b := !b && (x.p.(i) <= y.p.(i)) && (x.n.(i) <= y.n.(i))
    done;
    !b

  let merge x y =
    let z = {p = Array.make (Array.length x.p) 0; n = Array.make (Array.length x.n) 0} in
    for i = 0 to Array.length z.p - 1 do
      z.p.(i) <- max x.p.(i) y.p.(i);
      z.n.(i) <- max x.n.(i) y.n.(i)
    done;
    z
end

(* State-based last-writer-wins register *)
module LastWriterWinsRegister = struct
  type value = int
  type timestamp = float

  type register = {
    value : value;
    timestamp : timestamp;
  }

  let create initial_value =
    { value = initial_value; timestamp = 0.0 }

  let read register =
    register.value

  let write register new_value new_timestamp =
    if new_timestamp > register.timestamp then
      { value = new_value; timestamp = new_timestamp }
    else
      register

  let compare_and_swap register expected_value expected_timestamp new_value new_timestamp =
    if register.value = expected_value && register.timestamp = expected_timestamp then
      { value = new_value; timestamp = new_timestamp }
    else
      register
end


(* Op-based last-writer-wins register *)
module OpBasedLWWRegister = struct
  type value = int
  type tim
  type op =
    | Update of value * timestamp
    | Reset

  type register = {
    value    timestamp : timestamp;
    pending : op list;
  }

  let create initial_value =
    { value = initial_value; timestamp = 0.0; pending = [] }

  let read register =
    register.value

  let update register new_value new_timestamp =
    if new_timestamp > register.timestamp then
      { value = new_value; timestamp = new_timestamp; pending = [] }
    else
      { register with pending = Update (new_value, new_timestamp) :: register.pending }

  let reset register =
    { register with pending = Reset :: register.pending }

  let rec apply_pending register =
    match register.pending with
    | [] -> register
    | Update (v, t) :: rest ->
       let new_reg = update register v t in
       apply_pending { new_reg with pending = rest }
    | Reset :: rest ->
       let new_reg = { register with value = 0; timestamp = 0.0 } in
       apply_pending { new_reg with pending = rest }

  let downstream register =
    apply_pending register
end

(* State-based multi-value register *)
module MVRegister = struct
  type x = int
  type version = int array

  type payload = (x * version) list

  let initial = [(-1, Array.make 0 0)]

  let query_increment_vv () =
    let g = myID () in
    let vs = List.map snd initial in
    let v' = Array.map (fun v -> Array.fold_left max v vs |> (+) 1) vs in
    v'.(g) <- v'.(g) + 1;
    v'

  let update_assign (set_r : x list) =
    let v = query_increment_vv () in
    List.map (fun x -> (x, v)) set_r @ initial

  let query_value () =
    initial

  let compare (a : payload) (b : payload) =
    List.exists (fun (x, v) ->
      List.exists (fun (x', v') -> Array.exists ((<) v') v) b
    ) a

  let merge (a : payload) (b : payload) =
    let a' = List.filter (fun (x, v) ->
      List.exists (fun (y, w) ->
        Array.exists (fun v' -> v' >= v.(Array.length v - 1)) w ||
        Array.exists ((<) v') v
      ) b
    ) a in
    let b' = List.filter (fun (y, w) ->
      List.exists (fun (x, v) ->
        Array.exists (fun v' -> v' >= w.(Array.length w - 1)) v ||
        Array.exists ((<) w') w
      ) a
    ) b in
    a' @ b'
end

(* State-based grow-only set *)
module GSet = struct
  type t = {
    mutable data : P.t set
  }
  let empty () = { data = Set.empty }

  let add (s : t) (e : P.t) = s.data <- Set.add e s.data

  let lookup (s : t) (e : P.t) = Set.mem e s.data

  let compare (s1 : t) (s2 : t) = s1.data = s2.data
  
  let merge (s1 : t) (s2 : t) = { data = Set.union s1.data s2.data }
end


(* State-based 2P set *)
module StateBased2PSet = struct
  type t = {
    a : int list;
    r : int list;
  }

  let empty = { a = []; r = [] }

  let lookup set e =
    List.mem e set.a && not (List.mem e set.r)

  let add set e =
    if not (lookup set e) then
      { set with a = e :: set.a }
    else
      set

  let remove set e =
    if lookup set e then
      { set with r = e :: set.r }
    else
      set

  let compare set1 set2 =
    let subset xs ys =
      List.for_all (fun x -> List.mem x ys) xs
    in
    subset set1.a set2.a && subset set1.r set2.r

  let merge set1 set2 =
    let union xs ys =
      List.rev_append (List.rev_append xs (List.rev ys)) []
    in
    { a = union set1.a set2.a;
      r = union set1.r set2.r; }
end

(* Op based 2p set with unique elements *)
module USet = struct
  type element = int

  module IntSet = Set.Make(Int))
  let payload_set = ref IntSet.empty

  let lookup e =
    IntSet.mem e !payload_set

  let add e =
    if not (lookup e) then (
      payload_set := IntSet.add e !payload_set;
      true
    ) else false

  let remove e =
    if lookup e then (
      payload_set := IntSet.remove e !payload_set;
      true
    ) else false
end

(* Molli, Weiss, Skaf set *)
module MWSSet = struct
  type element = int
  type mwsset = (element * int) list

  let initial_set = []

  let lookup (e : element) (s : mwsset) =
    List.exists (fun (x, k) -> x = e && k > 0) s

  let add (e : element) (s : mwsset) =
    let j =
      try
        let (_, k) = List.find (fun (x, _) -> x = e) s in
        if k < 0 then abs k + 1 else 1
      with Not_found -> 1
    in
    List.remove_assocs e s @ [(e, j)]

  let remove (e : element) (s : mwsset) =
    let s' =
      if lookup e s then
        let (_, k') = List.find (fun (x, _) -> x = e) s in
        List.remove_assocs e s @ [(e, k' - 1)]
      else
        s
    in
    List.filter (fun (_, k) -> k <> 0) s'
end


(* Operation based observed-remove set *)
module ORSet = struct
  type unique_tag = unit

  type elem = int * unique_tag
  type set = (elem) list

  let unique_tag () = ()

  let rec unique_elements lst =
    match lst with
    | [] -> []
    | x :: xs -> x :: unique_elements (List.filter (fun y -> x <> y) xs)

  let empty_set = []

  let add e set =
    let new_tag = unique_tag () in
    let new_elem = (e, new_tag) in
    unique_elements (new_elem :: set)

  let lookup e set =
    List.exists (fun (x, _) -> x = e) set

  let remove e set =
    List.filter (fun (x, _) -> x <> e) set

  let downstream r = r

  let pre_condition f set = f set

  let atSource e = e
end

(* Operation based 2P2P graph *)
module Graph2P = struct
  type vertex = int
  type edge = vertex * vertex
  module VSet = Set.Make(Int)
  module ESet = Set.Make(struct
    type t = edge
    let compare (u, v) (x, y) =
      let c = compare u x in
      if c <> 0 then c else compare v y
  end)

  let payload_set va vr ea er = (va, vr, ea, er)

  let initial () = (VSet.empty, VSet.empty, ESet.empty, ESet.empty)

  let lookup_vertex (va, vr, _, _) v = VSet.mem v (VSet.diff va vr)

  let lookup_edge (va, vr, ea, er) (u, v) =
    VSet.mem u va && VSet.mem v va && ESet.mem (u, v) (ESet.union ea er)

  let add_vertex (va, vr, ea, er) w =
    let va' = VSet.add w va in
    (va', vr, ea, er)

  let add_edge (va, vr, ea, er) u v =
    let check_precond = VSet.mem u va && VSet.mem v va in
    if check_precond then
      let ea' = ESet.add (u, v) ea in
      (va, vr, ea', er)
    else
      (va, vr, ea, er)

  let remove_vertex (va, vr, ea, er) w =
    let check_precond = VSet.mem w va && VSet.for_all (fun (u, v) -> u <> w && v <> w) (ESet.union ea er) in
    if check_precond then
      let va' = VSet.remove w va in
      let vr' = VSet.add w vr in
      (va', vr', ea, er)
    else
      (va, vr, ea, er)

  let remove_edge (va, vr, ea, er) (u, v) =
    let check_precond = VSet.mem u va && VSet.mem v va in
    if check_precond then
      let er' = ESet.add (u, v) er in
      (va, vr, ea, er')
    else
      (va, vr, ea, er)
end


(* Op-based add-only monotonic DAG *)
module MonotonicDAG = struct
  type vertex = int
  type edge = vertex * vertex
  type graph = (vertex list, edge list) 

  let initial_graph = ([-1; 1], [(-1, 1)])

  let lookup_vertex (g: graph) (v: vertex) : bool =
    List.mem v (fst g)

  let lookup_edge (g: graph) (e: edge) : bool =
    List.mem e (snd g)

  let path (g: graph) (e: edge) : bool =
    let vertices, edges = g in
    let u, v = e in
    let rec path_exists w1 wm =
      w1 = u && wm = v && 
      (let rec path_between wj wj_next =
         if wj = wj_next then true
         else if lookup_edge g (wj, wj_next) then true
         else path_between wj_next (wj_next + 1)
       in path_between w1 (w1 + 1))
    in
    List.exists (path_exists u) vertices &&
    List.exists (path_exists v) vertices

  let add_edge (g: graph) (u: vertex) (v: vertex) : graph =
    let vertices, edges = g in
    if lookup_vertex g u && lookup_vertex g v && path g (u, v) then
      (vertices, (u, v) :: edges)
    else
      g

  let add_between (g: graph) (u: vertex) (v: vertex) (w: vertex) : graph =
    let vertices, edges = g in
    if lookup_vertex g u && lookup_vertex g v && lookup_vertex g w &&
       path g (u, w) && path g (v, w) && w <> u && w <> v then
      (w :: vertices, (u, w) :: (v, w) :: edges)
    else
      g
end


(* Add-Remove Partial Order *)
module AddRemovePartialOrder = struct
  type vertex = int
  type edge = vertex * vertex
  type t = {
    vertices : vertex list;
    removed : vertex list;
    edges : edge list;
  }

  let initial () = {
    vertices = [-1; 1];
    removed = [];
    edges = [((-1), 1)];
  }

  let lookup t v =
    List.mem v t.vertices

  let before t u v =
    let is_between w1 w2 =
      w1 = u && w2 = v || (lookup t w1 && lookup t w2 && List.exists (fun (x, y) -> x = w1 && y = w2) t.edges)
    in
    List.exists (fun w -> is_between u w && is_between w v) t.vertices

  let add_between t u v w =
    if not (lookup t w) || not (before t u w) || not (before t w v) then
      invalid_arg "addBetween precondition violated"
    else
      let vertices = w :: t.vertices in
      let edges = (u, w) :: (w, v) :: t.edges in
      { vertices; removed = t.removed; edges }

  let remove t v =
    if not (lookup t v) || v = -1 || v = 1 then
      invalid_arg "remove precondition violated"
    else
      let removed = v :: t.removed in
      let vertices = List.filter (fun x -> x <> v) t.vertices in
      let edges = List.filter (fun (x, y) -> x <> v && y <> v) t.edges in
      { vertices; removed; edges }
end

(* Replicated growth array *)
module RGA = struct
  type vertex = int * int
  type rga = {
    va : vertex list;
    vr : vertex list;
    edges : (vertex * vertex) list;
    now : unit -> int;
  }

  let empty now = { va = [((-1), -1)]; vr = [((-1), 0)]; edges = []; now }

  let lookup rga v = List.mem v rga.va && not (List.mem v rga.vr)

  let before rga u v =
    lookup rga u && lookup rga v &&
    let rec exists w1 wm = match wm with
      | [] -> false
      | w::rest -> w1 = u && w = v || exists (snd w) rest
    in
    exists u (List.filter (fun (a, _) -> a = fst u) rga.edges)

  let successor rga u =
    if not (lookup rga u) then failwith "Vertex not found";
    let rec succ v = match List.find_opt (fun w -> before rga u w && not (before rga w u)) rga.va with
      | Some w -> w
      | None -> succ ((-1), (rga.now ()) + 1)
    in
    succ ((-1), 0)

  let decompose rga u = u

  let addright rga u a =
    let t = rga.now () in
    let w = (a, t) in
    if List.mem w rga.va || List.mem w rga.vr then failwith "Timestamp conflict";
    { rga with
      va = w :: rga.va;
      edges = (u, w) :: (List.filter (fun (v, w') -> v <> u || w' <> w) rga.edges);
    }

  let remove rga w =
    if not (lookup rga w) then failwith "Vertex not found";
    { rga with vr = w :: rga.vr }
end

(* Continuous Sequence *)
module ContSequence = struct
  type identifier = string

  type 'a node = {
    left_child : 'a node option;
    right_child : 'a node option;
    data : 'a * identifier;
  }

  let mutable root : 'a node option = None

  let rec binary_search node id =
    match node with
    | None -> None
    | Some n ->
        let node_id = snd n.data in
        if id = node_id then
          Some n
        else if id < node_id then
          binary_search n.left_child id
        else
          binary_search n.right_child id

  let look_up id =
    match binary_search root id with
    | None -> false
    | Some _ -> true

  let decompose id =
    match binary_search root id with
    | None -> failwith "Identifier not found"
    | Some n -> n.data

  let before id1 id2 =
    let _, i1 = decompose id1 in
    let _, i2 = decompose id2 in
    i1 < i2

  let allocate_identifier_between id1 id2 =
    let min_char, max_char =
      if id1 < id2 then (id1, id2)
      else (id2, id1)
    in
    if String.length min_char <> 1 || String.length max_char <> 1 then
      failwith "Invalid input: input strings must have length 1"
    else begin
      let min_ascii = Char.code min_char.[0] in
      let max_ascii = Char.code max_char.[0] in
      let random_ascii = min_ascii + Random.int (max_ascii - min_ascii) in
      Char.chr random_ascii
    end

  let add_between e b e' as_child_of_e =
    let node_e = binary_search root e in
    let node_e' = binary_search root e' in
    match (node_e, node_e') with
    | (Some n1, None) ->
        let new_id = allocate_identifier_between (snd n1.data) e' in
        let new_node = { left_child = None; right_child = Some n1.right_child; data = (b, new_id) } in
        n1.right_child <- Some new_node
    | (None, Some n2) ->
        let new_id = allocate_identifier_between e n2.data in
        let new_node = { left_child = Some n2.left_child; right_child = None; data = (b, new_id) } in
        n2.left_child <- Some new_node
    | _ -> failwith "One or both identifiers not found"

  let remove e =
    let rec remove_node node =
        match node with
        | None -> None
        | Some n ->
            let id, _ = n.data in
            if id = e then
              (match n.left_child, n.right_child with
              | None, None -> None
              | Some left, None -> left
              | None, Some right -> right
              | Some left, Some right ->
                  let rec find_successor node =
                    match node with
                    | None -> None
                    | Some n ->
                        (match n.left_child with
                        | None -> Some n
                        | Some left -> find_successor (Some left))
                  in
                  match find_successor n.right_child with
                  | None -> failwith "Unexpected case"
                  | Some successor ->
                      successor.left_child <- n.left_child;
                      Some successor)
            else
              Some { n with
                     left_child = remove_node n.left_child;
                     right_child = remove_node n.right_child }
      in
      root <- remove_node root;
end

(* Op-based observed-remove shopping cart *)
module ORCart = struct
  type isbn = string
  type unique_tag = string
  type payload = (isbn * int * unique_tag) list

  let empty_payload = []

  let get_quantity (payload : payload) (k : isbn) : int =
    let quantities = List.filter (fun (k', _, _) -> k' = k) payload in
    List.fold_left (fun acc (_, n, _) -> acc + n) 0 quantities

  let add (payload : payload) (k : isbn) (n : int) : payload =
    let existing_quantity = get_quantity payload k in
    let new_unique_tag = Digest.string (Random.int max_int |> string_of_int) |> Digest.to_hex in
    (k, n + existing_quantity, new_unique_tag) :: List.filter (fun (k', _, _) -> k' <> k) payload

  let remove (payload : payload) (k : isbn) : payload =
    List.filter (fun (k', _, _) -> k' <> k) payload

  let upsert_precondition (payload : payload) (k : isbn) (n : int) (alpha : unique_tag) (r : payload) : payload =
    let r_set = List.filter (fun (k', _, _) -> k' = k) r in
    let union = List.concat [payload; r_set; [(k, n, alpha)]] in
    List.sort_uniq compare union

  let remove_elements_observed_at_source (payload : payload) (r : payload) : payload =
    List.filter (fun x -> not (List.mem x r)) payload
end