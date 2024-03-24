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