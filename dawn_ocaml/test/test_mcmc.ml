open Mcmc

let () = init 42

let initial_state = 10.
let num_iterations = 10000
let final_state, num_accepted = metropolis_hastings initial_state num_iterations
let acceptance_rate = float_of_int num_accepted /. float_of_int num_iterations

Printf.printf "Final state: %f\n" final_state;
Printf.printf "Acceptance rate: %f\n" acceptance_rate