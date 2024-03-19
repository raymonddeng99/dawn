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




// Gaussian Process Temporal Difference
struct GaussianProcess {
    std::function<double(int)> mean;
    std::function<double(int, int)> covariance;
};

GaussianProcess gptd(std::function<double(int)> initial_mean, std::function<double(int, int)> initial_covariance, std::vector<std::vector<int>> states, std::vector<std::vector<int>> actions) {
    int dim = states[0].size();

    auto gaussian_process = [&](std::function<double(int)> mean, std::function<double(int, int)> covariance) -> GaussianProcess {
        return {mean, covariance};
    };

    auto transition_model = [&](int state, int action) -> std::pair<std::vector<int>, std::vector<double>> { return {{}, {}}; };

    auto reward = [&](int state, int action) -> double { return 0.0; };

    auto bellman_operator = [&](const GaussianProcess& gp, int state, int action) -> double {
        auto [next_states, rewards] = transition_model(state, action);
        std::vector<double> next_values(next_states.size());
        for (int i = 0; i < next_states.size(); ++i) {
            next_values[i] = gp.mean(next_states[i]);
        }
        double total = 0.0;
        for (int i = 0; i < next_values.size(); ++i) {
            total += next_values[i] * rewards[i];
        }
        return total;
    };

    auto gptd_cost = [&](const GaussianProcess& gp) -> double {
        double total = 0.0;
        for (int i = 0; i < states.size(); ++i) {
            double target = bellman_operator(gp, i, i);
            double value = gp.mean(i);
            double error = target - value;
            total += std::pow(error, 2.0);
        }
        return total;
    };

    auto optimize = [&](const GaussianProcess& gp, double cost) -> GaussianProcess { return gp; };

    GaussianProcess initial_gp = gaussian_process(initial_mean, initial_covariance);
    return optimize(initial_gp, gptd_cost(initial_gp));
}


// Kalman Temporal Differences
typedef MatrixXd Matrix;

Matrix kalman_temporal_differences(double gamma, double lambda, double alpha_theta, double alpha_v, double alpha_w, Matrix rho, Matrix phi, function<Matrix(int, int)> feature_map, Matrix initial_theta, Matrix initial_v, Matrix initial_w) {
    int dim_theta = initial_theta.rows();
    int dim_v = initial_v.rows();
    int dim_w = initial_w.rows();

    Matrix theta = initial_theta;
    Matrix v = initial_v;
    Matrix w = initial_w;
    Matrix p = rho * rho.transpose();

    function<void(int, int)> loop;
    loop = [&](int state, int action) {
        Matrix x = feature_map(state, action);
        int next_state = get_state();
        double reward = get_reward();
        int next_action = get_action(next_state);
        Matrix x_next = feature_map(next_state, next_action);

        Matrix delta = reward + gamma * (theta * x_next).sum() - (theta * x).sum();
        Matrix phi_trans = phi.transpose();
        Matrix k = p * (phi_trans * (phi * p * phi_trans + lambda));
        theta += k * delta * alpha_theta;
        v += (delta - phi * v) * alpha_v;
        w += (x - phi * w) * alpha_w;
        p -= k * (phi * p);

        loop(next_state, next_action);
    };

    loop(get_state(), get_action(get_state()));
}



// Fixed-point least-squares temporal difference
template <typename S>
VectorXd lstd_fixed_point(std::function<VectorXd(const S&)> phi, double gamma, const std::vector<std::tuple<S, double, S>>& samples) {
    int phi_dim = phi(std::get<0>(samples[0])).size();
    MatrixXd a = MatrixXd::Zero(phi_dim, phi_dim);
    VectorXd b = VectorXd::Zero(phi_dim);

    for (const auto& [x, r, xp] : samples) {
        VectorXd phi_x = phi(x);
        VectorXd phi_xp = phi(xp);
        VectorXd phi_x_row = gamma * phi_x;
        a += phi_x * phi_x_row.transpose();
        b += phi_x * r;
        b -= phi_xp;
    }

    return a.ldlt().solve(b);
}



// Statistically linear least-squares temporal difference
vector<double> sl_lstd(
    vector<double> theta_0,
    vector<double> m_0,
    vector<vector<double>> p_0,
    vector<vector<double>> sigma_0,
    vector<vector<double>> transitions
) {
    int p = theta_0.size();
    int dim = p;
    double lambda = 1e-5 + 2.0 * (double)p / (1.0 - 2.0 * (double)(2 * p));

    auto unscented_transform = [&](vector<double> mean, vector<vector<double>> cov) {
        int n = mean.size();
        double gamma = sqrt(n + lambda);
        vector<vector<double>> sigma_points(2 * n + 1, vector<double>(n));
        vector<double> weights(2 * n + 1);

        sigma_points[0] = mean;
        weights[0] = lambda / (n + lambda);

        for (int i = 1; i <= 2 * n; i++) {
            int idx = i - 1;
            double sign = i <= n ? 1.0 : -1.0;
            for (int j = 0; j < n; j++) {
                sigma_points[i][j] = mean[j] + sign * gamma * sqrt(cov[j][j]);
            }
            weights[i] = 1.0 / (2.0 * (n + lambda));
        }

        return make_pair(sigma_points, weights);
    };

    auto sherman_morrison_update = [&](vector<vector<double>> mat, vector<double> vec) {
        int n = mat.size();
        double k = 1.0;
        for (int i = 0; i < n; i++) {
            double sum = 0.0;
            for (int j = 0; j < n; j++) {
                sum += mat[i][j] * vec[j];
            }
            k += vec[i] * sum;
        }
        vector<vector<double>> new_mat(n, vector<double>(n));
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                new_mat[i][j] = mat[i][j] - mat[i][j] * vec[i] * vec[j] / k;
            }
        }
        return new_mat;
    };

    vector<double> theta = theta_0;
    vector<double> m = m_0;
    vector<vector<double>> p_inv = p_0;
    vector<vector<double>> sigma = sigma_0;
    vector<vector<double>> sigma_inv = sigma_0;

    for (auto transition : transitions) {
        double s = transition[0];
        double a = transition[1];
        double r = transition[2];
        double s_prime = transition[3];

        auto [sigma_points_theta, weights_theta] = unscented_transform(theta, p_inv);
        auto [sigma_points_sigma, weights_sigma] = unscented_transform(theta, sigma);

        vector<double> q_sigma_points(sigma_points_theta.size());
        vector<double> pq_sigma_points(sigma_points_sigma.size());

        for (int i = 0; i < sigma_points_theta.size(); i++) {
            q_sigma_points[i] = f(s, a, sigma_points_theta[i]);
        }
        for (int i = 0; i < sigma_points_sigma.size(); i++) {
            pq_sigma_points[i] = pf(s, a, sigma_points_sigma[i]);
        }

        auto [q_bar, p_qtheta] = statistics_from(q_sigma_points, weights_theta);
        auto [pq_bar, p_sigma_pq] = statistics_from(pq_sigma_points, weights_sigma);

        vector<double> a_vec(p);
        vector<double> c_vec(p);
        for (int i = 0; i < p; i++) {
            double sum_a = 0.0;
            double sum_c = 0.0;
            for (int j = 0; j < p; j++) {
                sum_a += p_inv[i][j] * p_qtheta[j];
                sum_c += sigma_inv[i][j] * p_sigma_pq[j];
            }
            a_vec[i] = sum_a;
            c_vec[i] = sum_c;
        }

        vector<double> k(p);
        for (int i = 0; i < p; i++) {
            double sum = 0.0;
            for (int j = 0; j < p; j++) {
                sum += m[j] * (a_vec[j] - c_vec[j]);
            }
            k[i] = sum;
        }

        double td_error = r + 0.99 * pq_bar - q_bar;

        for (int i = 0; i < p; i++) {
            theta[i] += k[i] * td_error;
            m[i] -= k[i] * (m[i] * (a_vec[i] - c_vec[i]));
        }

        p_inv = sherman_morrison_update(p_inv, vector<double>(p, 1.0));
        sigma = sherman_morrison_update(sigma, p_sigma_pq);
        sigma_inv = sherman_morrison_update(sigma_inv, p_sigma_pq);
    }

    return {theta, m, p_inv, sigma, sigma_inv};
}



// Gaussian Temporal Difference, Sutton 2009
std::vector<double> gtd2(double alpha, double eta, double gamma, const std::vector<std::vector<double>>& features, const std::vector<double>& rewards) {
    int p = features[0].size();
    std::vector<double> theta(p, 0.0), w(p, 0.0);

    for (int i = 0; i < rewards.size() - 1; ++i) {
        double td_error = rewards[i];
        for (int j = 0; j < p; ++j) {
            td_error += gamma * features[i+1][j] * theta[j];
        }
        for (int j = 0; j < p; ++j) {
            td_error -= features[i][j] * theta[j];
        }

        for (int j = 0; j < p; ++j) {
            theta[j] += alpha * td_error * (features[i][j] - gamma * features[i+1][j] * w[j]);
            double dot_product = std::inner_product(features[i].begin(), features[i].end(), features[i].begin(), 0.0);
            w[j] += eta * alpha * (td_error * features[i][j] - w[j] * dot_product);
        }
    }

    return std::make_pair(theta, w);
}

// Temporal Difference With Correction
template <typename FeatureFunction>
std::pair<std::vector<double>, std::vector<double>> tdc(
    double gamma,
    double alpha,
    double beta,
    FeatureFunction feature_function,
    const std::vector<double>& init_theta,
    const std::vector<double>& init_omega,
    const std::vector<int>& states,
    const std::vector<int>& actions,
    const std::vector<double>& rewards
) {
    std::vector<double> theta(init_theta);
    std::vector<double> omega(init_omega);

    for (size_t i = 0; i < states.size(); ++i) {
        int s = states[i];
        int a = actions[i];
        double r = rewards[i];
        int s_next = (i == states.size() - 1) ? s : states[i+1];
        int a_next = (i == actions.size() - 1) ? a : actions[i+1];

        auto phi_s = feature_function(s, a);
        auto phi_s_next = feature_function(s_next, a_next);
        double q_s = std::inner_product(theta.begin(), theta.end(), phi_s.begin(), 0.0);
        double q_s_next = std::inner_product(theta.begin(), theta.end(), phi_s_next.begin(), 0.0);
        double td_error = r + gamma * q_s_next - q_s;

        for (size_t j = 0; j < theta.size(); ++j) {
            theta[j] += alpha * td_error * phi_s[j];
        }
        for (size_t j = 0; j < omega.size(); ++j) {
            double omega_update = omega[j] + beta * (td_error - std::inner_product(phi_s_next.begin(), phi_s_next.end(), omega.begin(), 0.0) * phi_s[j]);
            omega[j] = omega_update;
        }
    }

    return std::make_pair(theta, omega);
}