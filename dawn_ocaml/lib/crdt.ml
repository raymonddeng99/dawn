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