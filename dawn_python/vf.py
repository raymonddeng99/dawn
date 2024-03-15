# RL: Parametric Value Function Approximation

import numpy as np
import random
from collections import defaultdict

# TD with Function Approximation


# Linear function approximator
def linear_fa(state, theta):
    return np.dot(state, theta)

def TD_VFA(env, policy, gamma, theta_0, num_episodes, alpha):
    theta = theta_0
    for _ in range(num_episodes):
        state = env.reset()
        done = False
        while not done:
            action = policy(state)
            next_state, reward, done, _ = env.step(action)
            delta = reward + gamma * linear_fa(next_state, theta) - linear_fa(state, theta)
            theta += alpha * delta * state
            state = next_state
    return theta





# SARSA with Function Approximation

State = list[float]
Action = float

def q_approx(weights: dict[tuple[State, Action], float], state: State, action: Action) -> float:
    return weights.get((tuple(state), action), 0.0)

def epsilon_greedy(epsilon: float, q, state: State, actions: list[Action], rng=random.Random()) -> Action:
    if rng.random() < epsilon:
        # Explore - random action
        return rng.choice(actions)
    else:
        # Exploit - greedy action
        return max(actions, key=lambda a: q(state, a))

def sarsa_update(weights: dict[tuple[State, Action], float], state: State, action: Action, reward: float,
                 next_state: State, next_action: Action, gamma: float, alpha: float, featurize):
    q_cur = q_approx(weights, state, action)
    q_next = q_approx(weights, next_state, next_action)
    td_error = reward + gamma * q_next - q_cur

    for idx, feat_val in featurize(state, action):
        key = (tuple(state), action)
        weights[key] = weights.get(key, 0.0) + alpha * td_error * feat_val

def featurize(state: State, action: Action) -> list[tuple[int, float]]:
    return [(0, 1.0)]




# Q-learning with Function Approximation
def q_learning_func_approx(gamma, episodes, target_func, observe_func, env):
    def loop(theta, s):
        a = choose_action(theta, s)
        s_prime, r = observe_func(s, a)
        target = r + gamma * target_func(theta, s_prime)
        theta_prime = update_weights(theta, s, a, target)
        if env.terminated(s_prime):
            return theta_prime
        else:
            return loop(theta_prime, s_prime)

    init_theta = initialize_weights()

    def episode(theta, i):
        if i == 0:
            return theta
        s0 = env.reset()
        theta_prime = loop(theta, s0)
        return episode(theta_prime, i - 1)

    return episode(init_theta, episodes)


# LS Kalman Filter
def fixed_point_kalman_filter(samples):
    theta = np.zeros(num_features)
    P = np.eye(num_features)

    for s, a, q in samples:
        phi = feature_vector(s, a)
        k = P @ phi / (1 + phi.T @ P @ phi)
        q_hat = theta @ phi
        theta += k * (q - q_hat)
        P -= k @ phi.T @ P

    return theta

def feature_vector(s, a):
    return np.zeros(num_features)




# Residual SGD
def residual_sgd(iterations, learning_rate, initial_q, transition_function, reward_function, discount_factor):
    def bellman_operator(q, s, a):
        next_states = transition_function(s, a)
        next_state_values = [
            sum(reward_function(s, a, s_next) + discount_factor * max(q(s_next, a_next) for a_next in next_actions) for s_next in next_states)
            for a_next in next_actions
        ]
        return sum(next_state_values) / len(next_states)

    def cost_function(q):
        return sum((q_sa - bellman_operator(q, s, a))**2 for s, a in state_action_pairs)

    def residual_sgd_update(q, s, a):
        q_sa = q(s, a)
        t_q_sa = bellman_operator(q, s, a)
        gradient = 2.0 * (q_sa - t_q_sa)
        return q_sa - learning_rate * gradient

    def sgd_loop(q, iter):
        if iter == iterations:
            return q
        else:
            state_action_pair = random_state_action_pair()
            s, a = state_action_pair
            q_new = residual_sgd_update(q, s, a)
            return sgd_loop(q_new, iter + 1)

    return sgd_loop(initial_q, 0)



# Gaussian Process Temporal Difference
def gptd(initial_mean, initial_covariance, states, actions):
    dim = len(states[0])

    def gaussian_process(mean, covariance):
        return {"mean": mean, "covariance": covariance}

    def transition_model(state, action):
        return [], []

    def reward(state, action):
        return 0.0

    def bellman_operator(gp, state, action):
        next_states, rewards = transition_model(state, action)
        next_values = [gp["mean"](s) for s in next_states]
        return sum(v * r for v, r in zip(next_values, rewards))

    def gptd_cost(gp):
        total = 0.0
        for state, action in zip(states, actions):
            target = bellman_operator(gp, state[0], action[0])
            value = gp["mean"](state[0])
            error = target - value
            total += error ** 2.0
        return total

    def optimize(gp, cost):
        return gp

    initial_gp = gaussian_process(initial_mean, initial_covariance)
    return optimize(initial_gp, gptd_cost(initial_gp))


# Kalman Temporal Differences
def kalman_temporal_differences(
    gamma, lambda_, alpha_theta, alpha_v, alpha_w, rho, phi, feature_map,
    initial_theta, initial_v, initial_w
):
    dim_theta = len(initial_theta)
    dim_v = len(initial_v)
    dim_w = len(initial_w)

    theta = initial_theta
    v = initial_v
    w = initial_w
    p = rho @ rho.T

    def loop(state, action):
        x = feature_map(state, action)
        next_state = get_state()
        reward = get_reward()
        next_action = get_action(next_state)
        x_next = feature_map(next_state, next_action)

        delta = reward + gamma * (theta @ x_next) - (theta @ x)
        phi_trans = phi.T
        k = p @ (phi_trans @ (phi @ p @ phi_trans + lambda_))
        nonlocal theta, v, w, p
        theta += (k @ delta) * alpha_theta
        v += ((delta - phi @ v) * alpha_v)
        w += ((x - phi @ w) * alpha_w)
        p -= k @ (phi @ p)

        loop(next_state, next_action)

    loop(get_state(), get_action(get_state()))