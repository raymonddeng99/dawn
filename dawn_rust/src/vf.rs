// RL: Parametric Value Function Approximation


use rand::Rng;
use std::collections::HashMap;
use std::ops::{Add, Mul};



// TD with Function Approximation

fn linear_fa<T: Copy + Mul<Output = T> + Add<Output = T>>(state: &[T], theta: &[T]) -> T {
    state
        .iter()
        .zip(theta)
        .map(|(s, t)| s * t)
        .sum::<T>()
}

fn TD_VFA<T: Copy + Mul<Output = T> + Add<Output = T>>(
    env: &mut Environment<T>,
    policy: &Policy<T>,
    gamma: T,
    theta_0: Vec<T>,
    num_episodes: usize,
    alpha: T,
) -> Vec<T> {
    let mut theta = theta_0;
    for _ in 0..num_episodes {
        let mut state = env.reset();
        let mut done = false;
        while !done {
            let action = policy.get_action(&state);
            let (next_state, reward, done) = env.step(action);
            let delta = reward + gamma * linear_fa(&next_state, &theta) - linear_fa(&state, &theta);
            theta = theta
                .iter()
                .zip(&state)
                .map(|(t, s)| *t + alpha * delta * *s)
                .collect();
            state = next_state;
        }
    }
    theta
}

type State = Vec<f64>;
type Action = f64;

fn q_approx(weights: &HashMap<(State, Action), f64>, state: &State, action: &Action) -> f64 {
    weights
        .get(&(state.clone(), *action))
        .unwrap_or(&0.0)
        .to_owned()
}

fn epsilon_greedy<F>(
    epsilon: f64,
    q: &F,
    state: &State,
    actions: &[Action],
    rng: &mut impl Rng,
) -> Action
where
    F: Fn(&State, &Action) -> f64,
{
    if rng.gen::<f64>() < epsilon {
        // Explore - random action
        *actions.choose(rng).unwrap()
    } else {
        // Exploit - greedy action
        *actions
            .iter()
            .max_by(|a1, a2| {
                q(state, a1)
                    .partial_cmp(&q(state, a2))
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
            .unwrap()
    }
}

fn sarsa_update(
    weights: &mut HashMap<(State, Action), f64>,
    state: &State,
    action: &Action,
    reward: f64,
    next_state: &State,
    next_action: &Action,
    gamma: f64,
    alpha: f64,
    featurize: &impl Fn(&State, &Action) -> Vec<(usize, f64)>,
) {
    let q_cur = q_approx(weights, state, action);
    let q_next = q_approx(weights, next_state, next_action);
    let td_error = reward + gamma * q_next - q_cur;

    for (idx, feat_val) in featurize(state, action) {
        *weights.entry((state.clone(), *action)).or_insert(0.0) += alpha * td_error * feat_val;
    }
}

fn sarsa(
    env: &mut impl Environment,
    max_episodes: usize,
    gamma: f64,
    alpha: f64,
    epsilon: f64,
    featurize: impl Fn(&State, &Action) -> Vec<(usize, f64)>,
    mut rng: impl Rng,
) -> HashMap<(State, Action), f64> {
    let mut weights: HashMap<(State, Action), f64> = HashMap::new();

    for _ in 0..max_episodes {
        let mut state = env.reset();
        let action = epsilon_greedy(epsilon, &q_approx, &state, &env.actions(), &mut rng);
        let mut episode_loop = |state, action| {
            let (next_state, reward) = env.step(state, action);
            let next_action = epsilon_greedy(epsilon, &q_approx, &next_state, &env.actions(), &mut rng);
            sarsa_update(
                &mut weights,
                &state,
                &action,
                reward,
                &next_state,
                &next_action,
                gamma,
                alpha,
                &featurize,
            );
            if env.is_terminal(&next_state) {
                return;
            }
            episode_loop(next_state, next_action);
        };
        episode_loop(state, action);
    }

    weights
}

trait Environment {
    fn reset(&mut self) -> State;
    fn step(&mut self, state: State, action: Action) -> (State, f64);
    fn is_terminal(&self, state: &State) -> bool;
    fn actions(&self) -> Vec<Action>;
}

// Helper functions
fn featurize(state: &State, action: &Action) -> Vec<(usize, f64)> {
    vec![(0, 1.0)]
}



// Q-learning with Function Approximation
fn q_learning_func_approx<Theta, State, Action, Reward, Env>(
    gamma: f64,
    episodes: usize,
    mut target_func: impl FnMut(&Theta, &State) -> f64,
    mut observe_func: impl FnMut(&State, &Action) -> (State, Reward),
    env: &mut Env,
) -> Theta
where
    Theta: Clone,
    State: Clone,
    Action: Clone,
    Reward: Clone,
    Env: EnvironmentTrait<State, Action, Reward>,
{
    let mut loop = |theta: &Theta, s: &State| -> Theta {
        let a = choose_action(theta, s);
        let (s_prime, r) = observe_func(s, &a);
        let target = r + gamma * target_func(theta, &s_prime);
        let theta_prime = update_weights(theta, s, &a, target);
        if env.terminated(&s_prime) {
            theta_prime
        } else {
            loop(&theta_prime, &s_prime)
        }
    };

    let mut init_theta = initialize_weights();
    let mut episode = |theta: Theta, i: usize| -> Theta {
        if i == 0 {
            theta
        } else {
            let s0 = env.reset();
            let theta_prime = loop(&theta, &s0);
            episode(theta_prime, i - 1)
        }
    };

    episode(init_theta, episodes)
}


// LS Kalman Filter
use std::f64;

struct State;
struct Action;

fn feature_vector(_state: &State, _action: &Action) -> Vec<f64> {
    vec![1.0, 1.0, 1.0]
}

fn fixed_point_kalman_filter(samples: &[(State, Action, f64)]) -> Vec<f64> {
    let num_features = feature_vector(&State, &Action).len();
    let mut theta = vec![0.0; num_features];
    let mut p = vec![vec![0.0; num_features]; num_features];

    for i in 0..num_features {
        p[i][i] = 1.0;
    }

    for &(ref state, ref action, q) in samples.iter() {
        let phi = feature_vector(state, action);
        let mut k = vec![0.0; num_features];

        for i in 0..num_features {
            let mut sum = 0.0;
            for j in 0..num_features {
                sum += p[i][j] * phi[j];
            }
            k[i] = sum;
        }

        let k_denom = 1.0 + dot_product(&k, &phi);
        for i in 0..num_features {
            k[i] /= k_denom;
        }

        let q_hat = dot_product(&theta, &phi);
        for i in 0..num_features {
            theta[i] += (q - q_hat) * k[i];
        }

        let mut temp = vec![0.0; num_features];
        for i in 0..num_features {
            let mut sum = 0.0;
            for j in 0..num_features {
                sum += p[j][i] * k[j];
            }
            temp[i] = sum;
        }

        for i in 0..num_features {
            for j in 0..num_features {
                p[i][j] -= temp[i] * phi[j];
            }
        }
    }

    theta
}

fn dot_product(a: &[f64], b: &[f64]) -> f64 {
    let mut result = 0.0;
    for i in 0..a.len() {
        result += a[i] * b[i];
    }
    result
}




// Residual SGD
fn residual_sgd<F, G, H>(
    iterations: usize,
    learning_rate: f64,
    initial_q: F,
    transition_function: G,
    reward_function: H,
    discount_factor: f64,
) -> F
where
    F: FnMut(S, A) -> f64,
    G: Fn(S, A) -> Vec<S>,
    H: Fn(S, A, S) -> f64,
{
    let mut bellman_operator = |q: &mut F, s: S, a: A| {
        let next_states = transition_function(s, a);
        let next_state_values: f64 = next_states
            .iter()
            .map(|s_next| {
                let max_q_sa = next_actions.iter().fold(f64::NEG_INFINITY, |acc, &a_next| {
                    acc.max(q(*s_next, a_next))
                });
                reward_function(s, a, *s_next) + discount_factor * max_q_sa
            })
            .sum();
        next_state_values / next_states.len() as f64
    };

    let cost_function = |q: &F| {
        state_action_pairs
            .iter()
            .map(|&(s, a)| {
                let q_sa = q(s, a);
                let t_q_sa = bellman_operator(&mut q.clone(), s, a);
                (q_sa - t_q_sa).powi(2)
            })
            .sum()
    };

    let mut residual_sgd_update = |q: &mut F, s: S, a: A| {
        let q_sa = q(s, a);
        let t_q_sa = bellman_operator(&mut q.clone(), s, a);
        let gradient = 2.0 * (q_sa - t_q_sa);
        q_sa - learning_rate * gradient
    };

    let mut q = initial_q;
    for _ in 0..iterations {
        let state_action_pair = random_state_action_pair();
        let (s, a) = state_action_pair;
        q = residual_sgd_update(&mut q, s, a);
    }

    q
}



// Gaussian Process Temporal Difference
struct GaussianProcess {
    mean: Box<dyn Fn(i32) -> f64>,
    covariance: Box<dyn Fn(i32, i32) -> f64>,
}

fn gptd(
    initial_mean: Box<dyn Fn(i32) -> f64>,
    initial_covariance: Box<dyn Fn(i32, i32) -> f64>,
    states: Vec<Vec<i32>>,
    actions: Vec<Vec<i32>>,
) -> GaussianProcess {
    let dim = states[0].len();

    let gaussian_process = |mean: Box<dyn Fn(i32) -> f64>, covariance: Box<dyn Fn(i32, i32) -> f64>| -> GaussianProcess {
        GaussianProcess {
            mean,
            covariance,
        }
    };

    let transition_model = |state: i32, action: i32| -> (Vec<i32>, Vec<f64>) { (vec![], vec![]) };

    let reward = |state: i32, action: i32| -> f64 { 0.0 };

    let bellman_operator = |gp: &GaussianProcess, state: i32, action: i32| -> f64 {
        let (next_states, rewards) = transition_model(state, action);
        let next_values: Vec<_> = next_states.iter().map(|s| (gp.mean)(*s)).collect();
        next_values.iter().zip(rewards.iter()).map(|(v, r)| v * r).sum()
    };

    let gptd_cost = |gp: &GaussianProcess| -> f64 {
        states.iter().zip(actions.iter()).fold(0.0, |cost, (state, action)| {
            let target = bellman_operator(gp, 0, 0);
            let value = (gp.mean)(*state[0]);
            let error = target - value;
            cost + error.powf(2.0)
        })
    };

    let optimize = |gp: GaussianProcess, cost: f64| -> GaussianProcess { gp };

    let initial_gp = gaussian_process(initial_mean, initial_covariance);
    optimize(initial_gp, gptd_cost(&initial_gp))
}