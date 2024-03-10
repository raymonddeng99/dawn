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