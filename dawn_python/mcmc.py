import math
import random

def target_density(x):
    return math.exp(-x ** 2 / 2.0)

def proposal_density(x):
    return math.exp(-x ** 2 / 2.0) / math.sqrt(2.0 * math.pi)

def uniform_random():
    return random.uniform(0, 1)

def gaussian_random():
    u1 = uniform_random()
    u2 = uniform_random()
    return math.sqrt(-2.0 * math.log(u1)) * math.cos(2.0 * math.pi * u2)

def metropolis_hastings(initial_state, num_iterations):
    state = initial_state
    num_accepted = 0

    for _ in range(num_iterations):
        proposed_state = state + gaussian_random()
        acceptance_ratio = (target_density(proposed_state) / target_density(state)) * \
                           (proposal_density(state - proposed_state) / proposal_density(proposed_state - state))

        if acceptance_ratio >= 1.0 or acceptance_ratio > uniform_random():
            state = proposed_state
            num_accepted += 1

    return state, num_accepted