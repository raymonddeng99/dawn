(* RL: Parametric Value Function Approximation *)


(* TD with Function Approximation *)

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

(* SARSA with Function Approximation *)

type state = int array
type action = int

let q_approx weights state action =
  Array.fold_left2 (+.) 0.0 weights (featurize state action)

let epsilon_greedy q epsilon state =
  if Random.float 1.0 < epsilon then
    List.nth (List.permute (possible_actions state)) 0
  else
    fst @@ List.max (List.map (fun a -> (a, q state a)) (possible_actions state))

let sarsa_update weights state action reward next_state next_action gamma alpha =
  let q_cur = q_approx weights state action in
  let q_next = q_approx weights next_state next_action in
  let td_error = reward +. gamma *. q_next -. q_cur in
  Array.map2 (fun w f -> w +. alpha *. td_error *. f) weights (featurize state action)

let sarsa env max_episodes gamma alpha epsilon =
  let weights = Array.make (num_features env) 0.0 in
  for episode = 1 to max_episodes do
    let rec episode_loop state =
      let action = epsilon_greedy (q_approx weights) epsilon state in
      let next_state, reward = env_step env state action in
      let next_action = epsilon_greedy (q_approx weights) epsilon next_state in
      weights <- sarsa_update weights state action reward next_state next_action gamma alpha;
      if terminated env next_state then () else episode_loop next_state
    in
    episode_loop (env_reset env)
  done;
  weights