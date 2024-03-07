open Random

let target_density x = exp (-. (x *. x) /. 2.)

let proposal_density x = exp (-. (x *. x) /. 2.) /. sqrt (2. *. pi)

let metropolis_hastings initial_state num_iterations =
  let rec loop state num_accepted = function
    | 0 -> state, num_accepted
    | n ->
      let proposed_state = state +. (gaussian_float 1.) in
      let acceptance_ratio =
        (target_density proposed_state) /. (target_density state) *. (proposal_density (state -. proposed_state)) /. (proposal_density (proposed_state -. state))
      in
      let num_accepted' =
        if acceptance_ratio >= 1. || acceptance_ratio > random_float 1. then
          num_accepted + 1
        else
          num_accepted
      in
      let state' = if acceptance_ratio >= 1. || acceptance_ratio > random_float 1. then proposed_state else state in
      loop state' num_accepted' (n - 1)
  in
  loop initial_state 0 num_iterations