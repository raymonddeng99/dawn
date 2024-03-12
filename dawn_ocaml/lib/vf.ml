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

(* Q-learning with Function Approximation *)
let q_learning_func_approx gamma episodes target_func observe_func env =
  let rec loop theta s =
    let a = choose_action theta s in
    let (s', r) = env_step env s a in
    let target = r +. gamma *. (target_func theta s') in
    let theta' = update_weights theta s a target in
    if terminated env s' then theta'
    else loop theta' s'
  in
  let init_theta = initialize_weights () in
  let rec episode theta i =
    if i = 0 then theta
    else
      let s0 = env_reset env in
      let theta' = loop theta s0 in
      episode theta' (i - 1)
  in
  episode init_theta episodes


(* LS Kalman Filter *)
let fixed_point_kalman_filter samples =
  let rec fpkf theta p i =
    match samples with
    | [] -> theta
    | (s, a, q)::rest ->
      let phi = feature_vector s a in
      let p' = p -. (mat_mult (mat_mult p (outer_product phi phi)) p) /.
                    (1. +. (mat_mult (transpose phi) (mat_mult p phi))) in
      let k = mat_mult p' phi in
      let theta' = mat_add theta (scalar_mult (q -. (mat_mult (transpose phi) theta)) k) in
      fpkf theta' p' (i + 1) rest
  in
  fpkf zero_vector identity_matrix 0 samples