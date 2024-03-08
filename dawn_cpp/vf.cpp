// RL: Parametric Value Function Approximation

#include <vector>

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