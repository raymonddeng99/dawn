// RL: Parametric Value Function Approximation

#include <vector>
#include <iostream>
#include <unordered_map>
#include <random>
#include <algorithm>
#include <numeric>


// TD with Function Approximation

double linear_fa(const std::vector<double>& state, const std::vector<double>& theta) {
    double result = 0.0;
    for (int i = 0; i < state.size(); ++i) {
        result += state[i] * theta[i];
    }
    return result;
}

std::vector<double> TD_VFA(const Environment& env, const Policy& policy, double gamma,
                            const std::vector<double>& theta_0, int num_episodes, double alpha) {
    std::vector<double> theta = theta_0;
    for (int i = 0; i < num_episodes; ++i) {
        std::vector<double> state = env.reset();
        bool done = false;
        while (!done) {
            int action = policy(state);
            std::vector<double> next_state;
            double reward;
            done = env.step(action, next_state, reward);
            double delta = reward + gamma * linear_fa(next_state, theta) - linear_fa(state, theta);
            for (int j = 0; j < theta.size(); ++j) {
                theta[j] += alpha * delta * state[j];
            }
            state = next_state;
        }
    }
    return theta;
}



// SARSA with Function Approximation

using State = std::vector<double>;
using Action = double;

double qApprox(const std::unordered_map<std::pair<State, Action>, double>& weights,
               const State& state, const Action& action) {
    auto it = weights.find(std::make_pair(state, action));
    return it != weights.end() ? it->second : 0.0;
}

Action epsilonGreedy(double epsilon, const std::function<double(const State&, const Action&)>& q,
                     const State& state, const std::vector<Action>& actions,
                     std::mt19937& rng) {
    std::bernoulli_distribution dist(epsilon);
    if (dist(rng)) {
        // Explore - random action
        std::uniform_int_distribution<> actionDist(0, actions.size() - 1);
        return actions[actionDist(rng)];
    } else {
        // Exploit - greedy action
        return *std::max_element(actions.begin(), actions.end(),
                                 [&](const Action& a1, const Action& a2) {
                                     return q(state, a1) < q(state, a2);
                                 });
    }
}

void sarsaUpdate(std::unordered_map<std::pair<State, Action>, double>& weights,
                 const State& state, const Action& action, double reward,
                 const State& nextState, const Action& nextAction,
                 double gamma, double alpha,
                 const std::function<std::vector<std::pair<size_t, double>>(const State&, const Action&)>& featurize) {
    double qCur = qApprox(weights, state, action);
    double qNext = qApprox(weights, nextState, nextAction);
    double tdError = reward + gamma * qNext - qCur;

    for (const auto& [idx, featVal] : featurize(state, action)) {
        weights[std::make_pair(state, action)][idx] += alpha * tdError * featVal;
    }
}

std::vector<std::pair<size_t, double>> featurize(const State& state, const Action& action) {
    return {{0, 1.0}};
}