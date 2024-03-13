// RL: Parametric Value Function Approximation

#include <vector>
#include <iostream>
#include <unordered_map>
#include <random>
#include <algorithm>
#include <numeric>
#include <functional>
#include <cmath>

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


// Q-learning with Function Approximation
template <typename Theta, typename State, typename Action, typename Reward, typename Env>
Theta q_learning_func_approx(
    double gamma,
    int episodes,
    std::function<double(const Theta&, const State&)> target_func,
    std::function<std::pair<State, Reward>(const State&, const Action&)> observe_func,
    Env& env
) {
    auto loop = [&](const Theta& theta, const State& s) -> Theta {
        Action a = choose_action(theta, s);
        auto [s_prime, r] = observe_func(s, a);
        double target = r + gamma * target_func(theta, s_prime);
        Theta theta_prime = update_weights(theta, s, a, target);
        if (env.terminated(s_prime)) {
            return theta_prime;
        } else {
            return loop(theta_prime, s_prime);
        }
    };

    Theta init_theta = initialize_weights();
    auto episode = [&](const Theta& theta, int i) -> Theta {
        if (i == 0) {
            return theta;
        } else {
            State s0 = env.reset();
            Theta theta_prime = loop(theta, s0);
            return episode(theta_prime, i - 1);
        }
    };

    return episode(init_theta, episodes);
}



// LS Kalman Filter
std::vector<double> fixed_point_kalman_filter(const std::vector<std::tuple<int, int, double>>& samples) {
    size_t num_features = feature_vector(0, 0).size();
    std::vector<double> theta(num_features, 0.0);
    std::vector<std::vector<double>> P(num_features, std::vector<double>(num_features, 0.0));

    for (size_t i = 0; i < num_features; ++i) {
        P[i][i] = 1.0;
    }

    for (const auto& [state, action, q] : samples) {
        std::vector<double> phi = feature_vector(state, action);
        std::vector<double> k(num_features);

        for (size_t i = 0; i < num_features; ++i) {
            double sum = 0.0;
            for (size_t j = 0; j < num_features; ++j) {
                sum += P[i][j] * phi[j];
            }
            k[i] = sum;
        }

        double k_denom = 1.0 + dot_product(phi, k);
        for (size_t i = 0; i < num_features; ++i) {
            k[i] /= k_denom;
        }

        double q_hat = dot_product(theta, phi);
        theta = theta + (q - q_hat) * k;

        std::vector<double> temp(num_features);
        for (size_t i = 0; i < num_features; ++i) {
            double sum = 0.0;
            for (size_t j = 0; j < num_features; ++j) {
                sum += P[j][i] * k[j];
            }
            temp[i] = sum;
        }

        for (size_t i = 0; i < num_features; ++i) {
            for (size_t j = 0; j < num_features; ++j) {
                P[i][j] -= temp[i] * phi[j];
            }
        }
    }

    return theta;
}




// Residual SGD
auto residualSgd(int iterations, double learningRate, std::function<double(S, A)> initialQ, std::function<std::vector<S>(S, A)> transitionFunction, std::function<double(S, A, S)> rewardFunction, double discountFactor) -> std::function<double(S, A)> {
    auto bellmanOperator = [&](std::function<double(S, A)> q, S s, A a) -> double {
        auto nextStates = transitionFunction(s, a);
        double nextStateValues = 0.0;
        for (auto sNext : nextStates) {
            double maxQSa = std::numeric_limits<double>::lowest();
            for (auto aNext : nextActions) {
                maxQSa = std::max(maxQSa, q(sNext, aNext));
            }
            nextStateValues += rewardFunction(s, a, sNext) + discountFactor * maxQSa;
        }
        return nextStateValues / static_cast<double>(nextStates.size());
    };

    auto costFunction = [&](std::function<double(S, A)> q) -> double {
        double totalCost = 0.0;
        for (auto& pair : stateActionPairs) {
            S s = pair.first;
            A a = pair.second;
            double qSa = q(s, a);
            double tQSa = bellmanOperator(q, s, a);
            totalCost += std::pow(qSa - tQSa, 2);
        }
        return totalCost;
    };

    auto residualSgdUpdate = [&](std::function<double(S, A)> q, S s, A a) -> double {
        double qSa = q(s, a);
        double tQSa = bellmanOperator(q, s, a);
        double gradient = 2.0 * (qSa - tQSa);
        return qSa - learningRate * gradient;
    };

    std::function<double(S, A)> q = initialQ;
    for (int iter = 0; iter < iterations; ++iter) {
        auto stateActionPair = randomStateActionPair();
        S s = stateActionPair.first;
        A a = stateActionPair.second;
        q = residualSgdUpdate(q, s, a);
    }

    return q;
}