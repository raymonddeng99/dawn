// RL: Parametric Value Function Approximation

use std::ops::{Add, Mul};

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