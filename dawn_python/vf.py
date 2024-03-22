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



# Fixed-point least-squares temporal difference
def lstd_fixed_point(phi, gamma, samples):
    phi_dim = len(phi(samples[0][0]))
    a = np.zeros((phi_dim, phi_dim))
    b = np.zeros(phi_dim)

    for x, r, xp in samples:
        phi_x = np.array(phi(x))
        phi_xp = np.array(phi(xp))
        phi_x_row = gamma * phi_x
        a += np.outer(phi_x, phi_x_row)
        b += phi_x * r
        b -= phi_xp

    theta = np.linalg.solve(a, b)
    return theta


# Statistically linear least-squares temporal difference
def sl_lstd(theta_0, m_0, p_0, sigma_0, transitions):
    p = len(theta_0)
    dim = p
    lambda_ = 1e-5 + 2.0 * p / (1.0 - 2.0 * p)

    def unscented_transform(mean, cov):
        n = len(mean)
        gamma = math.sqrt(n + lambda_)
        sigma_points = np.zeros((2 * n + 1, n))
        weights = np.zeros(2 * n + 1)

        sigma_points[0] = mean
        weights[0] = lambda_ / (n + lambda_)

        for i in range(1, 2 * n + 1):
            idx = i - 1
            sign = 1.0 if i <= n else -1.0
            sigma_points[i] = mean + sign * gamma * np.sqrt(np.diag(cov))
            weights[i] = 1.0 / (2.0 * (n + lambda_))

        return sigma_points, weights

    def sherman_morrison_update(mat, vec):
        n = len(mat)
        k = 1.0 + np.dot(vec, np.dot(mat, vec))
        new_mat = mat - np.outer(np.dot(mat, vec), vec) / k
        return new_mat

    theta = theta_0
    m = m_0
    p_inv = p_0
    sigma = sigma_0
    sigma_inv = sigma_0

    for transition in transitions:
        s, a, r, s_prime, _ = transition
        sigma_points_theta, weights_theta = unscented_transform(theta, p_inv)
        sigma_points_sigma, weights_sigma = unscented_transform(theta, sigma)

        q_sigma_points = [f(s, a, th) for th in sigma_points_theta]
        pq_sigma_points = [pf(s, a, si) for si in sigma_points_sigma]

        q_bar, p_qtheta = statistics_from(q_sigma_points, weights_theta)
        pq_bar, p_sigma_pq = statistics_from(pq_sigma_points, weights_sigma)

        a = np.dot(p_inv, p_qtheta)
        c = np.dot(sigma_inv, p_sigma_pq)
        k = np.dot(m, a - c)
        td_error = r + 0.99 * pq_bar - q_bar

        theta = theta + td_error * k
        m = m - k * np.dot(m, a - c)
        p_inv = sherman_morrison_update(p_inv, a - c)
        sigma = sherman_morrison_update(sigma, p_sigma_pq)
        sigma_inv = sherman_morrison_update(sigma_inv, p_sigma_pq)

    return theta, m, p_inv, sigma, sigma_inv



# Gaussian Temporal Difference, Sutton 2009
def gtd2(alpha, eta, gamma, features, rewards):
    p = len(features[0])
    theta = np.zeros(p)
    w = np.zeros(p)

    for i in range(len(rewards) - 1):
        td_error = rewards[i] + gamma * np.dot(features[i+1], theta) - np.dot(features[i], theta)

        theta += alpha * td_error * (features[i] - gamma * features[i+1] * w)
        w += eta * alpha * (td_error * features[i] - np.dot(w, features[i] * features[i]))

    return theta, w


# Temporal Difference with Correction
def tdc(gamma, alpha, beta, feature_function, init_theta, init_omega, states, actions, rewards):
    theta = np.array(init_theta)
    omega = np.array(init_omega)

    for i, (s, a, r) in enumerate(zip(states, actions, rewards)):
        if i == len(states) - 1:
            s_next, a_next = s, a
        else:
            s_next, a_next = states[i+1], actions[i+1]

        phi_s = np.array(feature_function(s, a))
        phi_s_next = np.array(feature_function(s_next, a_next))
        q_s = np.dot(theta, phi_s)
        q_s_next = np.dot(theta, phi_s_next)
        td_error = r + gamma * q_s_next - q_s

        theta += alpha * td_error * phi_s
        omega += beta * (td_error - np.dot(phi_s_next, omega)) * phi_s

    return theta, omega


# Fitted Q
def fitted_q(transitions, initial_q, states, actions):
    def sampled_bellman_operator(q, transition):
        s, a, r, s_prime = transition
        max_q_s_prime = max(q[s_prime_val] for s_prime_val in s_prime)
        return r + max_q_s_prime

    def update_q(q, transition):
        s, a, r, s_prime = transition
        q[s] = sampled_bellman_operator(q, transition)

    def iterate(q, transitions):
        q_prime = q.copy()
        for transition in transitions:
            s, a, r, s_prime = transition
            update_q(q_prime, transition)
        if q == q_prime:
            return q_prime
        return iterate(q_prime, transitions)

    q = [[initial_q(s, a) for a in actions] for s in states]
    return iterate(q, transitions)


# Least Squares Policy Evaluation
def lspe(theta_init, x_train, y_train):
    n = len(x_train)
    d = len(x_train[0])

    def phi(x):
        return x

    def sherman_morrison_update(a, b, c, d):
        ab = np.dot(a, b.T)
        denom = 1 + np.sum(ab * c)
        return d / denom

    def update(theta):
        phi_x = [phi(x) for x in x_train]
        phi_theta = [np.dot(x, theta) for x in phi_x]
        errors = [y - y_pred for y, y_pred in zip(y_train, phi_theta)]
        a = [x * err for x, err in zip(phi_x, errors)]
        b = sherman_morrison_update(theta, np.sum(a, axis=0), phi_x, theta)
        new_theta = theta + b

        sum_squared_errors = sum(err ** 2 for err in errors)
        if sum_squared_errors < 1e-6:
            return new_theta
        else:
            return update(new_theta)

    return update(theta_init)


# Q OSP, Yu and Bertsekas 2007
def q_osp(max_iterations, gamma, initial_value):
    return q_osp_iter(0, initial_value, max_iterations, gamma)

def q_osp_iter(n, v, max_iterations, gamma):
    if n == max_iterations:
        return v
    return q_osp_iter(n + 1, bellman_operator(v, gamma), max_iterations, gamma)

def bellman_operator(v, gamma):
    return [max_q(v, v_i, gamma) for v_i in v]

def max_q(v, v_i, gamma):
    sum_v = sum(v)
    return max(v_i, gamma * sum_v)