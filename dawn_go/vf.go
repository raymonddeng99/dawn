// RL: Parametric Value Function Approximation

import (
    "fmt"
    "math/rand"
)



// TD with Function Approximation

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




// SARSA with Function Approximation

type State []float64
type Action float64

func qApprox(weights map[stateActionTuple]float64, state State, action Action) float64 {
    key := stateActionTuple{state, action}
    if val, ok := weights[key]; ok {
        return val
    }
    return 0.0
}

type stateActionTuple struct {
    state State
    action Action
}

func epsilonGreedy(epsilon float64, q func(State, Action) float64, state State, actions []Action, rng *rand.Rand) Action {
    if rng.Float64() < epsilon {
        // Explore - random action
        return actions[rng.Intn(len(actions))]
    } else {
        // Exploit - greedy action
        bestAction := actions[0]
        bestValue := q(state, bestAction)
        for _, action := range actions[1:] {
            value := q(state, action)
            if value > bestValue {
                bestValue = value
                bestAction = action
            }
        }
        return bestAction
    }
}

func sarsaUpdate(weights map[stateActionTuple]float64, state State, action Action, reward float64, nextState State, nextAction Action, gamma float64, alpha float64, featurize func(State, Action) []feature) {
    qCur := qApprox(weights, state, action)
    qNext := qApprox(weights, nextState, nextAction)
    tdError := reward + gamma*qNext - qCur

    key := stateActionTuple{state, action}
    for _, f := range featurize(state, action) {
        weights[key] += alpha * tdError * f.value
    }
}

type feature struct {
    index int
    value float64
}

func featurize(state State, action Action) []feature {
    return []feature{{0, 1.0}}
}


// Q-learning with Function Approximation
func qLearningFuncApprox(gamma float64, episodes int, targetFunc func(theta Theta, s State) float64, observeFunc func(s State, a Action) (State, float64), env Env) Theta {
    var loop func(theta Theta, s State) Theta
    loop = func(theta Theta, s State) Theta {
        a := chooseAction(theta, s)
        sPrime, r := envStep(env, s, a)
        target := r + gamma*targetFunc(theta, sPrime)
        thetaPrime := updateWeights(theta, s, a, target)
        if terminated(env, sPrime) {
            return thetaPrime
        }
        return loop(thetaPrime, sPrime)
    }

    initTheta := initializeWeights()
    var episode func(theta Theta, i int) Theta
    episode = func(theta Theta, i int) Theta {
        if i == 0 {
            return theta
        }
        s0 := envReset(env)
        thetaPrime := loop(theta, s0)
        return episode(thetaPrime, i-1)
    }

    return episode(initTheta, episodes)
}


// LS Kalman Filter
func fixedPointKalmanFilter(samples []sample) (theta, p mat.Dense) {
    theta = mat.NewDense(1, 1, nil) // initialize theta as a 1x1 matrix
    p = mat.NewDense(1, 1, []float64{1.0}) // initialize p as a 1x1 identity matrix

    for _, sample := range samples {
        s, a, q := sample.state, sample.action, sample.q
        phi := featureVector(s, a)
        k := mat.NewDense(1, 1, nil)
        k.Mul(p, phi)
        temp := mat.NewDense(1, 1, nil)
        temp.Mul(phi.T(), p)
        temp.Mul(temp, phi)
        temp.AddScalar(1.0)
        k.DivElemVec(k.RawMatrix().Data, temp.RawMatrix().Data)

        theta.AddScaledVec(q-mat.Dot(phi.T(), theta), k)
        temp.Mul(k, phi.T())
        p.Sub(p, temp)
    }
    return theta, p
}