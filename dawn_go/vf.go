// RL: Parametric Value Function Approximation

import (
    "fmt"
    "math"
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




// Residual SGD
func residualSgd(iterations int, learningRate float64, initialQ func(S, A) float64, transitionFunction func(S, A) []S, rewardFunction func(S, A, S) float64, discountFactor float64) func(S, A) float64 {
    bellmanOperator := func(q func(S, A) float64, s S, a A) float64 {
        nextStates := transitionFunction(s, a)
        var nextStateValues float64
        for _, sNext := range nextStates {
            maxQSa := math.Inf(-1)
            for _, aNext := range nextActions {
                maxQSa = math.Max(maxQSa, q(sNext, aNext))
            }
            nextStateValues += rewardFunction(s, a, sNext) + discountFactor*maxQSa
        }
        return nextStateValues / float64(len(nextStates))
    }

    costFunction := func(q func(S, A) float64) float64 {
        var totalCost float64
        for _, pair := range stateActionPairs {
            s, a := pair.s, pair.a
            qSa := q(s, a)
            tQSa := bellmanOperator(q, s, a)
            totalCost += math.Pow(qSa-tQSa, 2)
        }
        return totalCost
    }

    residualSgdUpdate := func(q func(S, A) float64, s S, a A) float64 {
        qSa := q(s, a)
        tQSa := bellmanOperator(q, s, a)
        gradient := 2.0 * (qSa - tQSa)
        return qSa - learningRate*gradient
    }

    q := initialQ
    for iter := 0; iter < iterations; iter++ {
        stateActionPair := randomStateActionPair()
        s, a := stateActionPair.s, stateActionPair.a
        q = residualSgdUpdate(q, s, a)
    }

    return q
}



// Gaussian Process Temporal Difference
type GaussianProcess struct {
    mean       func(int) float64
    covariance func(int, int) float64
}

func gptd(initialMean func(int) float64, initialCovariance func(int, int) float64, states [][]int, actions [][]int) GaussianProcess {
    dim := len(states[0])

    gaussianProcess := func(mean func(int) float64, covariance func(int, int) float64) GaussianProcess {
        return GaussianProcess{mean: mean, covariance: covariance}
    }

    transitionModel := func(state, action int) ([]int, []float64) {
        return []int{}, []float64{}
    }

    reward := func(state, action int) float64 {
        return 0.0
    }

    bellmanOperator := func(gp GaussianProcess, state, action int) float64 {
        nextStates, rewards := transitionModel(state, action)
        nextValues := make([]float64, len(nextStates))
        for i, nextState := range nextStates {
            nextValues[i] = gp.mean(nextState)
        }
        total := 0.0
        for i, nextValue := range nextValues {
            total += nextValue * rewards[i]
        }
        return total
    }

    gptdCost := func(gp GaussianProcess) float64 {
        total := 0.0
        for i, state := range states {
            target := bellmanOperator(gp, i, i)
            value := gp.mean(i)
            error := target - value
            total += math.Pow(error, 2.0)
        }
        return total
    }

    optimize := func(gp GaussianProcess, cost float64) GaussianProcess {
        return gp
    }

    initialGP := gaussianProcess(initialMean, initialCovariance)
    return optimize(initialGP, gptdCost(initialGP))
}


// Kalman Temporal Differences
func KalmanTemporalDifferences(
    gamma, lambda, alphaTheta, alphaV, alphaW float64,
    rho, phi Matrix,
    featureMap func(int, int) Matrix,
    initialTheta, initialV, initialW Matrix,
) {
    dimTheta := len(initialTheta)
    dimV := len(initialV)
    dimW := len(initialW)

    theta := initialTheta
    v := initialV
    w := initialW
    p := MatMul(rho, MatTranspose(rho))

    var loop func(int, int)
    loop = func(state, action int) {
        x := featureMap(state, action)
        nextState := getState()
        reward := getReward()
        nextAction := getAction(nextState)
        xNext := featureMap(nextState, nextAction)

        delta := reward + gamma*(MatMul(theta, xNext)[0][0]) - (MatMul(theta, x)[0][0])
        phiTrans := MatTranspose(phi)
        k := MatMul(p, MatMul(phiTrans, MatAdd(MatMul(phi, p), MatScalarMul(lambda, phiTrans))))

        theta = MatAdd(theta, MatScalarMul(MatMul(k, delta), alphaTheta))
        v = MatAdd(v, MatScalarMul(MatSub(delta, MatMul(phi, v)), alphaV))
        w = MatAdd(w, MatScalarMul(MatSub(x, MatMul(phi, w)), alphaW))
        p = MatSub(MatMul(rho, MatTranspose(rho)), MatMul(k, MatMul(phi, p)))

        loop(nextState, nextAction)
    }

    loop(getState(), getAction(getState()))
}


// Fixed-point least-squares temporal difference
func lstdFixedPoint(phi func(s interface{}) []float64, gamma float64, samples [][]interface{}) []float64 {
    n := len(phi(samples[0][0]))
    a := mat.NewDense(n, n, nil)
    b := make([]float64, n)

    for _, sample := range samples {
        x, r, xp := sample[0], sample[1].(float64), sample[2]
        phi_x := mat.NewVecDense(len(phi(x)), phi(x))
        phi_xp := mat.NewVecDense(len(phi(xp)), phi(xp))
        phi_x_row := mat.NewVecDense(n, nil)
        phi_x_row.MulVec(phi_x, mat.NewScalar(gamma, nil))
        a.Add(a, mat.OuterProduct(phi_x, phi_x_row))
        b = mat.Sum(b, phi_x.ScaleVec(r, nil))
        b = mat.SubVec(b, phi_xp)
    }

    theta := make([]float64, n)
    mat.Solve(a, b, theta)
    return theta
}


// Statistically linear least-squares temporal difference
func slLstd(theta0, m0, p0, sigma0 []float64, transitions [][]float64) ([]float64, []float64, []float64, []float64, []float64) {
    p := len(theta0)
    dim := p
    lambda := 1e-5 + 2.0*float64(p)/(1.0-float64(2*p))

    unscented_transform := func(mean, cov []float64) ([][]float64, []float64) {
        n := len(mean)
        gamma := math.Sqrt(float64(n) + lambda)
        sigma_points := make([][]float64, 2*n+1)
        weights := make([]float64, 2*n+1)

        sigma_points[0] = mean
        weights[0] = lambda / (float64(n) + lambda)

        for i := 1; i <= 2*n; i++ {
            idx := i - 1
            sign := 1.0
            if i > n {
                sign = -1.0
            }
            sigma_point := make([]float64, n)
            for j := 0; j < n; j++ {
                sigma_point[j] = mean[j] + sign*gamma*math.Sqrt(cov[j])
            }
            sigma_points[i] = sigma_point
            weights[i] = 1.0 / (2.0 * (float64(n) + lambda))
        }

        return sigma_points, weights
    }

    sherman_morrison_update := func(mat, vec []float64) []float64 {
        n := len(mat)
        k := 1.0 + dotProduct(vec, matVecProd(mat, vec))
        new_mat := make([]float64, n*n)
        for i := 0; i < n*n; i++ {
            new_mat[i] = mat[i]
        }
        for i := 0; i < n; i++ {
            for j := 0; j < n; j++ {
                new_mat[i*n+j] -= mat[i*n+j] * vec[i] * vec[j] / k
            }
        }
        return new_mat
    }

    loop := func(theta, m, p_inv, sigma, sigma_inv []float64, transitions [][]float64) ([]float64, []float64, []float64, []float64, []float64) {
        if len(transitions) == 0 {
            return theta, m, p_inv, sigma, sigma_inv
        }

        transition := transitions[0]
        s, a, r, s_prime, _ := transition[0], transition[1], transition[2], transition[3], transition[4]
        sigma_points_theta, weights_theta := unscented_transform(theta, p)
        sigma_points_sigma, weights_sigma := unscented_transform(theta, sigma)

        q_sigma_points := make([]float64, len(sigma_points_theta))
        pq_sigma_points := make([]float64, len(sigma_points_sigma))

        for i, th := range sigma_points_theta {
            q_sigma_points[i] = f(s, a, th)
        }
        for i, si := range sigma_points_sigma {
            pq_sigma_points[i] = pf(s, a, si)
        }

        q_bar, p_qtheta := statistics_from(q_sigma_points, weights_theta)
        pq_bar, p_sigma_pq := statistics_from(pq_sigma_points, weights_sigma)

        a := matVecProd(p_inv, p_qtheta)
        c := matVecProd(sigma_inv, p_sigma_pq)
        k := vecMatVecProd(m, vecSub(a, c))
        td_error := r + gamma*pq_bar - q_bar

        theta_prime := vecAdd(theta, vecScale(k, td_error))
        m_prime := vecSub(m, vecScale(k, vecMatVecProd(m, vecSub(a, c))))
        p_prime := sherman_morrison_update(p, vecSub(a, c))
        p_inv_prime := sherman_morrison_update(p_inv, vecSub(p_qtheta, p_sigma_pq))
        sigma_prime := sherman_morrison_update(sigma, p_sigma_pq)
        sigma_inv_prime := sherman_morrison_update(sigma_inv, p_sigma_pq)

        return loop(theta_prime, m_prime, p_inv_prime, sigma_prime, sigma_inv_prime, transitions[1:])
    }

    return loop(theta0, m0, p0, sigma0, sigma0, transitions)
}



// Gaussian Temporal Difference, Sutton 2009
func gtd2(alpha, eta, gamma float64, features [][]float64, rewards []float64) ([]float64, []float64) {
    p := len(features[0])
    theta := make([]float64, p)
    w := make([]float64, p)

    for i := 0; i < len(rewards)-1; i++ {
        tdError := rewards[i]
        for j := 0; j < p; j++ {
            tdError += gamma * features[i+1][j] * theta[j]
        }
        for j := 0; j < p; j++ {
            tdError -= features[i][j] * theta[j]
        }

        for j := 0; j < p; j++ {
            theta[j] += alpha * tdError * (features[i][j] - gamma*features[i+1][j]*w[j])
            w[j] += eta * alpha * (tdError*features[i][j] - w[j]*dotProduct(features[i], features[i]))
        }
    }

    return theta, w
}