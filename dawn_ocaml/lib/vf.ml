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




(* Residual SGD *)
let residual_sgd iterations learning_rate initial_q transition_function reward_function discount_factor =
  let bellman_operator q s a =
    let next_states = transition_function s a in
    let next_state_values = List.map (fun s' ->
      let max_q_sa' = List.fold_left (fun acc a' ->
        max acc (q s' a')
      ) neg_infinity next_actions in
      reward_function s a s' +. discount_factor *. max_q_sa'
    ) next_states in
    List.fold_left (+.) 0.0 next_state_values /. float_of_int (List.length next_states)
  in

  let cost_function q =
    List.fold_left (fun acc (s, a) ->
      let q_sa = q s a
      and t_q_sa = bellman_operator q s a in
      acc +. (q_sa -. t_q_sa) ** 2.0
    ) 0.0 state_action_pairs
  in

  let residual_sgd_update q s a =
    let q_sa = q s a
    and t_q_sa = bellman_operator q s a in
    let gradient = 2.0 *. (q_sa -. t_q_sa) in
    q s a -. learning_rate *. gradient
  in

  let rec sgd_loop q iter =
    if iter = iterations then q
    else
      let state_action_pair = random_state_action_pair () in
      let s, a = state_action_pair in
      let q' = residual_sgd_update q s a in
      sgd_loop q' (iter + 1)
  in
  sgd_loop initial_q 0


(* Gaussian Process Temporal Difference *)
let gptd initial_mean initial_covariance states actions =
  let dim = Array.length (List.hd states) in

  let gaussian_process ~mean ~covariance =
    let mean state = mean state in
    let covariance state1 state2 = covariance state1 state2 in
    { GP.mean; covariance }
  in

  let transition_model state action =
    [], [||]
  in
  let reward state action =
    0.0
  in

  let bellman_operator gp state action =
    let next_states, rewards = transition_model state action in
    let next_values = Array.map gp.GP.mean next_states in
    Array.fold_left2 (+.) 0.0 next_values rewards
  in

  let gptd_cost gp =
    List.fold_left (fun cost (state, action) ->
      let target = bellman_operator gp state action in
      let value = gp.GP.mean state in
      let error = target -. value in
      cost +. error ** 2.0
    ) 0.0 (List.combine states actions)
  in

  let optimize gp cost =
    gp
  in

  let initial_gp = gaussian_process ~mean:initial_mean ~covariance:initial_covariance in
  let optimized_gp = optimize initial_gp gptd_cost in
  optimized_gp


(* Kalman Temporal Differences *)
let kalman_temporal_differences
    ~gamma
    ~lambda
    ~alpha_theta
    ~alpha_v
    ~alpha_w
    ~rho
    ~phi
    ~feature_map
    ~initial_theta
    ~initial_v
    ~initial_w =
  
  let dim_theta = Array.length initial_theta in
  let dim_v = Array.length initial_v in
  let dim_w = Array.length initial_w in

  let theta = ref initial_theta in
  let v = ref initial_v in
  let w = ref initial_w in
  let p = ref (Mat.mul rho (Mat.transpose rho)) in

  let rec loop state action =
    let x = feature_map state action in
    let next_state = get_state () in
    let reward = get_reward () in
    let next_action = get_action next_state in
    let x_next = feature_map next_state next_action in

    let delta = reward +. gamma *. (Mat.mul !theta x_next) -. (Mat.mul !theta x) in
    let phi_trans = Mat.transpose phi in
    let k = Mat.mul !p (Mat.mul phi_trans (Mat.add (Mat.mul phi !p phi_trans) lambda)) in

    theta := Mat.add !theta (Mat.mul (Mat.mul k delta) alpha_theta);
    v := Mat.add !v (Mat.mul (Mat.sub delta (Mat.mul phi !v)) alpha_v);
    w := Mat.add !w (Mat.mul (Mat.sub x (Mat.mul phi !w)) alpha_w);
    p := Mat.sub (Mat.mul rho (Mat.transpose rho)) (Mat.mul k (Mat.mul phi !p));

    loop next_state next_action
  in
  loop (get_state ()) (get_action (get_state ()))