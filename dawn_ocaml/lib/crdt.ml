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