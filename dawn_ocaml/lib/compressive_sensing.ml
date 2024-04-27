open Base

let rec recover_signal measurement_matrix measurements sparsity_level tolerance max_iter =
  let n = Array.length measurements in
  let m = Array.length measurement_matrix.(0) in
  let signal_estimate = Array.make m 0.0 in

  let rec lasso_iteration iteration =
    if iteration = max_iter then signal_estimate else
      let residual = vecsubstract measurements (matmul measurement_matrix signal_estimate) in
      let gradient = matmul (Array.map (fun row -> Array.map (fun x -> -.x) row) measurement_matrix) residual in
      let step_size = 1.0 /. (vecnorm gradient) in
      let soft_thresholding x =
        let threshold = tolerance *. step_size in
        if x > threshold then x -. threshold
        else if x < -. threshold then x +. threshold
        else 0.0
      in
      let new_signal_estimate = Array.map soft_thresholding (vecadd signal_estimate (Array.map (fun x -> x *. step_size) gradient)) in
      let k = ref 0 in
      Array.iter (fun x -> if x <> 0.0 then incr k) new_signal_estimate;
      if !k <= sparsity_level then new_signal_estimate
      else lasso_iteration (iteration + 1)
  in

  lasso_iteration 0