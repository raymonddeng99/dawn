# RL: Parametric Value Function Approximation

import numpy as np

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