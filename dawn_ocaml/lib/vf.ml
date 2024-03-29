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


(* Fixed-point least-squares temporal difference *)
let lstd_fixed_point phi gamma samples =
  let n = List.length samples in
  let phi_dim = List.hd samples |> fst |> Array.length in

  let a = Array.make_matrix phi_dim phi_dim 0.0 in
  let b = Array.make phi_dim 0.0 in
  List.iter (fun (x, r, xp) ->
    let phi_x = phi x in
    let phi_xp = phi xp in
    let phi_x_row = Array.map (fun v -> v *. gamma) phi_x in
    for i = 0 to phi_dim - 1 do
      for j = 0 to phi_dim - 1 do
        a.(i).(j) <- a.(i).(j) +. phi_x.(i) *. phi_x_row.(j)
      done;
      b.(i) <- b.(i) +. phi_x.(i) *. r
    done;
    for i = 0 to phi_dim - 1 do
      b.(i) <- b.(i) -. phi_xp.(i)
    done
  ) samples;

  let theta = Lacaml.D.gels a b in
  theta


(* Statistically linear least-squares temporal difference *)
let sl_lstd theta_0 m_0 p_0 sigma_0 transitions =
  let p, dim = Array.length theta_0, Array.length theta_0 in
  let lambda = 1e-5 +. 2.0 *. float p /. (1.0 -. float (2 * p)) in

  let unscented_transform mean cov =
    let n = Array.length mean in
    let sigma_points = Array.make (2 * n + 1) [||] in
    sigma_points, weights in

  let sherman_morrison_update mat vec =
    let k = 1.0 +. (vec **@ mat *@ vec) in
    mat -@ (mat *@ (vec *@ vec **@ mat) /@ k)
  in

  let rec loop theta m p_inv sigma sigma_inv transitions =
    match transitions with
    | [] -> theta, m, p_inv, sigma, sigma_inv
    | (s, a, r, s', _)::rest ->
       let sigma_points_theta, weights_theta = unscented_transform theta p in
       let sigma_points_sigma, weights_sigma = unscented_transform theta sigma in
       let q_sigma_points = Array.map (fun th -> f (s, a, th)) sigma_points_theta in
       let pq_sigma_points = Array.map (fun si -> pf (s, a, si)) sigma_points_sigma in
       let q_bar, p_qtheta = statistics_from q_sigma_points weights_theta in
       let pq_bar, p_sigma_pq = statistics_from pq_sigma_points weights_sigma in
       let a = p_inv *@ p_qtheta in
       let c = sigma_inv *@ p_sigma_pq in
       let k = m *@ (a -@ c) **@ a in
       let td_error = r +. gamma *. pq_bar -. q_bar in
       let theta' = theta +@ k *@ td_error in
       let m' = m -@ k *@ (m **@ (a -@ c)) in
       let p' = sherman_morrison_update p (a -@ c) in
       let p_inv' = sherman_morrison_update p_inv (p_qtheta -@ p_sigma_pq) in
       let sigma' = sherman_morrison_update sigma p_sigma_pq in
       let sigma_inv' = sherman_morrison_update sigma_inv p_sigma_pq in
       loop theta' m' p_inv' sigma' sigma_inv' rest
  in

  loop theta_0 m_0 p_0 sigma_0 transitions



(* Gaussian Temporal Difference, Sutton 2009 *)
let gtd2 alpha eta gamma features rewards =
  let p = Array.length features.(0) in
  let theta = Array.make p 0.0
  and w = Array.make p 0.0 in

  let update_theta theta w i =
    let td_error = rewards.(i) +. gamma *. (Array.fold_left (+.) 0.0
                                            (Array.mapi
                                               (fun j x -> x *. features.(i+1).(j))
                                               theta))
                   -. (Array.fold_left (+.) 0.0
                         (Array.mapi
                            (fun j x -> x *. features.(i).(j))
                            theta)) in
    Array.mapi (fun j x -> x +. alpha *. td_error *. (features.(i).(j) -. gamma *. features.(i+1).(j) *. w.(j))) theta
  in

  let update_w w i =
    Array.mapi (fun j x -> x +. eta *. alpha *. (rewards.(i) +. gamma *. (Array.fold_left (+.) 0.0
                                                                           (Array.mapi
                                                                              (fun k y -> y *. features.(i+1).(k))
                                                                              theta))
                                                -. (Array.fold_left (+.) 0.0
                                                     (Array.mapi
                                                        (fun k y -> y *. features.(i).(k))
                                                        theta))
                                                *. features.(i).(j)
                                                -. x *. (Array.fold_left (+.) 0.0
                                                           (Array.mapi
                                                              (fun k y -> y *. features.(i).(k))
                                                              features.(i)))))
               w
  in

  let rec aux theta w i =
    if i = Array.length rewards - 1 then (theta, w)
    else aux (update_theta theta w i) (update_w w i) (i + 1)
  in

  aux theta w 0


(* Temporal Difference with Correction *)
let tdc gamma alpha beta feature_function init_theta init_omega =
  let dim_theta = Array.length init_theta in
  let dim_omega = Array.length init_omega in

  let rec aux theta omega states actions rewards i =
    if i >= Array.length states then (theta, omega)
    else
      let current_state = states.(i) in
      let current_action = actions.(i) in
      let current_reward = rewards.(i) in
      let next_state = if i + 1 < Array.length states then states.(i+1) else current_state in
      let next_action = if i + 1 < Array.length actions then actions.(i+1) else current_action in

      let phi_current = feature_function current_state current_action in
      let phi_next = feature_function next_state next_action in
      let q_current = Array.fold_left2 (fun acc w f -> acc +. f *. w) 0.0 theta phi_current in
      let q_next = Array.fold_left2 (fun acc w f -> acc +. f *. w) 0.0 theta phi_next in

      let td_error = current_reward +. gamma *. q_next -. q_current in

      let theta_update = Array.map2 (fun w f -> w +. alpha *. td_error *. f) theta phi_current in
      let omega_update = Array.map2 (fun w f -> w +. beta *. (td_error -. (Array.fold_left2 (fun acc f2 f1 -> acc +. f2 *. f1) 0.0 phi_next omega) *. f)) omega phi_current in

      aux theta_update omega_update states actions rewards (i+1)
  in

  aux init_theta init_omega


(* Fitted Q *)
let fitted_q transitions initial_q_fun =
  let sampled_bellman_operator q (s, a, r, s_prime) =
    r +. (max_float_array (Array.map (fun s' -> (q s')) s_prime))
  in
  let update_q q (s, a, r, s_prime) =
    (s, a, sampled_bellman_operator q (s, a, r, s_prime))
  in
  let rec iterate q transitions =
    let q_prime =
      Array.fold_left
        (fun q (s, a, r, s_prime) ->
           Array.map (fun (s', a', q_val) ->
             if s' = s then (s', a', update_q q (s, a, r, s_prime))
             else (s', a', q_val))
           q)
        q transitions
    in
    if Array.for_all (fun (_, _, (q_val, q_val')) -> q_val = q_val') (Array.map2 (fun x y -> (x, y)) q q_prime)
    then q_prime
    else iterate q_prime transitions
  in
  iterate (Array.map (fun s -> Array.map (fun a -> initial_q_fun s a) actions) states) transitions


(* Least Squares Policy Evaluation *)
let lspe theta_init x_train y_train =
  let n = Array.length x_train in
  let d = Array.length x_train.(0) in

  let phi x = x in

  let sherman_morrison_update a b c d =
    let ab = Array.map2 ( *. ) a b in
    let denom = 1. +. Array.fold_left ( +. ) 0. (Array.map2 ( *. ) ab c) in
    Array.map (fun x -> x /. denom) d
  in

  let rec update theta =
    let phi_x = Array.map phi x_train in
    let phi_theta = Array.map (fun x -> Array.fold_left ( +. ) 0. (Array.map2 ( *. ) x theta)) phi_x in
    let errors = Array.map2 (fun y y_pred -> y -. y_pred) y_train phi_theta in
    let a = Array.map2 (fun x err -> x *. err) phi_x errors in
    let b = sherman_morrison_update theta phi_x errors a in
    let new_theta = Array.map2 ( +. ) theta b in
    if Array.fold_left (fun acc x -> acc +. x **. 2.) 0. errors < 1e-6 then
      new_theta
    else
      update new_theta
  in

  update theta_init


(* Q OSP, Yu and Bertsekas 2007 *)
let q_osp max_iterations gamma =
  let rec value_iteration n v =
    if n = 0 then v else
      let v' =
        Array.mapi (fun i v_i ->
          let max_q = Array.fold_left max (-infinity) (Array.map (fun v_j -> gamma *. (v_i +. v_j)) v) in
          max v_i max_q
        ) v
      in
      value_iteration (n - 1) v'
  in

  let rec bellman_operator v =
    Array.mapi (fun i v_i ->
      let max_q = Array.fold_left max (-infinity) (Array.map (fun v_j -> gamma *. v_j) v) in
      max v_i max_q
    ) v
  in

  let rec q_osp_iter n v =
    if n = max_iterations then v else
      q_osp_iter (n + 1) (bellman_operator v)
  in

  let initial_value = Array.make ... 0.0 in
  q_osp_iter 0 initial_value