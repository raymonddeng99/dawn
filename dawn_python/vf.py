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