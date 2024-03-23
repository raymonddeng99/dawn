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