// RL: Parametric Value Function Approximation

import "math"

func linearFa(state []float64, theta []float64) float64 {
    var result float64
    for i := range state {
        result += state[i] * theta[i]
    }
    return result
}

func TDVFA(env *Environment, policy Policy, gamma float64, theta0 []float64, numEpisodes int, alpha float64) []float64 {
    theta := make([]float64, len(theta0))
    copy(theta, theta0)
    for i := 0; i < numEpisodes; i++ {
        state := env.Reset()
        done := false
        for !done {
            action := policy(state)
            nextState, reward, done := env.Step(action)
            delta := reward + gamma*linearFa(nextState, theta) - linearFa(state, theta)
            for j := range theta {
                theta[j] += alpha * delta * state[j]
            }
            state = nextState
        }
    }
    return theta
}
