(* RL: Parametric Value Function Approximation *)

let linear_fa state theta =
  List.fold_left2 (+.) 0.0 state theta

let TD_VFA env policy gamma theta_0 num_episodes alpha =
  let rec aux theta episode_count =
    if episode_count = num_episodes then theta
    else
      let state = env#reset () in
      let rec aux_inner theta state =
        if env#is_done state then theta
        else
          let action = policy state in
          let next_state, reward = env#step action state in
          let delta = reward +. gamma *. linear_fa next_state theta -. linear_fa state theta in
          let theta' = List.map2 (fun t s -> t +. alpha *. delta *. s) theta state in
          aux_inner theta' next_state
      in
      aux (aux_inner theta state) (episode_count + 1)
  in
  aux theta_0 0