-- RL: Parametric Value Function Approximation
import Data.Vect





-- TD With Function Approximation

linearFa : Vect n Double -> Vect n Double -> Double
linearFa state theta = sum $ zipWith (*) state theta

tdVFA : Environment -> (State -> Action) -> Double -> Vect n Double -> Int -> Double -> IO (Vect n Double)
tdVFA env policy gamma theta0 numEpisodes alpha = go theta0 0
  where
    go : Vect n Double -> Int -> IO (Vect n Double)
    go theta episodeCount
      = if episodeCount == numEpisodes
           then pure theta
           else do
             state <- envReset env
             theta' <- episode theta state
             go theta' (episodeCount + 1)

    episode : Vect n Double -> State -> IO (Vect n Double)
    episode theta state
      = if envIsDone env state
           then pure theta
           else do
             action <- policy state
             (nextState, reward) <- envStep env action state
             let delta = reward + gamma * linearFa nextState theta - linearFa state theta
                 theta' = zipWith (\t, s => t + alpha * delta * s) theta state
             episode theta' nextState



-- SARSA with Function Approximation

State : Type
State = Vect n Double

Action : Type
Action = Double

qApprox : Vect (State, Action, Double) -> State -> Action -> Double
qApprox weights state action = maybe 0.0 id $ lookup (state, action) (vectToList weights)

epsilonGreedy : Double -> (State -> Action -> Double) -> State -> Vect m Action -> (seed : Double ** Action)
epsilonGreedy epsilon q state actions = let explore = rand seed 0.0 1.0 in
  if explore < epsilon
    then (snd explore ** randomElem actions (fst explore))
    else (snd explore ** fst $ maximum [(action, q state action) | action <- toList actions])

sarsaUpdate : Vect (State, Action, Double) -> State -> Action -> Double -> State -> Action -> Double -> Double -> Vect (State, Action, Double)
sarsaUpdate weights state action reward nextState nextAction gamma alpha =
  zipWith (\w, f => if fst w == (state, action)
                    then (fst w, snd w + alpha * tdError * snd f)
                    else w)
          (vectToList weights)
          (featurize state action)
  where
    qCur = qApprox weights state action
    qNext = qApprox weights nextState nextAction
    tdError = reward + gamma * qNext - qCur

featurize : State -> Action -> Vect n (State, Action, Double)
featurize state action = [(state, action, 1.0)] 


-- Q-learning with Function Approximation
qLearningFuncApprox : Double -> Nat -> (Theta -> State -> Double) -> (State -> Action -> (State, Reward)) -> Env -> Theta
qLearningFuncApprox gamma episodes targetFunc observeFunc env = loop initTheta
  where
    loop : Theta -> State -> Theta
    loop theta s =
      let a = chooseAction theta s
          (s', r) = observeFunc s a
          target = r + gamma * targetFunc theta s'
          theta' = updateWeights theta s a target
      in if terminated env s' then theta'
         else loop theta' s'

    initTheta : Theta
    initTheta = initializeWeights

    episode : Theta -> Nat -> Theta
    episode theta 0 = theta
    episode theta i =
      let s0 = envReset env
          theta' = loop theta s0
      in episode theta' (i - 1)



-- LS Kalman Filter
fixedPointKalmanFilter : Vect n Double -> Vect n (Double, Double, Double) -> Vect n Double
fixedPointKalmanFilter {n} theta0 samples =
  let (theta, _) = foldl step (theta0, idris_mat n n) samples in theta
  where
    step : (Vect n Double, Matrix n n Double) -> (Double, Double, Double) -> (Vect n Double, Matrix n n Double)
    step (theta, p) (s, a, q) =
      let phi = featureVector s a
          k = p #> phi #/ (1 + phi `transpose` #> p #> phi)
          theta' = theta `plus` (k `scalarMult` (q - phi `transpose` #> theta))
          p' = p `minus` (k #> (phi `transpose` #> p))
      in (theta', p')

    featureVector : Double -> Double -> Vect n Double



-- Residual SGD
residualSgd : Nat -> Double -> (S -> A -> Double) -> (S -> A -> List S) -> (S -> A -> S -> Double) -> Double -> (S -> A -> Double)
residualSgd iterations learningRate initialQ transitionFunction rewardFunction discountFactor =
  let
    bellmanOperator : (S -> A -> Double) -> S -> A -> Double
    bellmanOperator q s a =
      let nextStates = transitionFunction s a in
      let nextStateValues = sum [rewardFunction s a s' + discountFactor * maximum [q s' a' | a' <- nextActions] | s' <- nextStates] in
      nextStateValues / (fromInteger (length nextStates))

    costFunction : (S -> A -> Double) -> Double
    costFunction q = sum [(q_sa - bellmanOperator q s a) ** 2 | (s, a) <- stateActionPairs]

    residualSgdUpdate : (S -> A -> Double) -> S -> A -> Double
    residualSgdUpdate q s a =
      let q_sa = initialQ s a in
      let t_q_sa = bellmanOperator q s a in
      let gradient = 2.0 * (q_sa - t_q_sa) in
      q_sa - learningRate * gradient

    sgdLoop : (S -> A -> Double) -> Nat -> (S -> A -> Double)
    sgdLoop q Z = q
    sgdLoop q (S iter) =
      let (s, a) = randomStateActionPair () in
      let q' = residualSgdUpdate q s a in
      sgdLoop q' iter
  in
    sgdLoop initialQ iterations



-- Gaussian Process Temporal Difference
data GaussianProcess : Type where
  MkGaussianProcess : (mean : Int -> Double) -> (covariance : Int -> Int -> Double) -> GaussianProcess

gptd : (initialMean : Int -> Double) -> (initialCovariance : Int -> Int -> Double) -> List (List Int) -> List (List Int) -> GaussianProcess
gptd initialMean initialCovariance states actions =
  let gaussianProcess : (Int -> Double) -> (Int -> Int -> Double) -> GaussianProcess
      gaussianProcess mean covariance = MkGaussianProcess mean covariance

      transitionModel : Int -> Int -> (List Int, List Double)
      transitionModel state action = ([], [])

      reward : Int -> Int -> Double
      reward state action = 0.0

      bellmanOperator : GaussianProcess -> Int -> Int -> Double
      bellmanOperator (MkGaussianProcess mean covariance) state action =
        let (nextStates, rewards) = transitionModel state action
            nextValues = map mean nextStates
        in sum (zipWith (*) nextValues rewards)

      gptdCost : GaussianProcess -> Double
      gptdCost gp =
        let f : Double -> (List Int, List Int) -> Double
            f cost (state, action) =
              let target = bellmanOperator gp (head state) (head action)
                  value = (mean gp) (head state)
                  error = target - value
              in cost + error ** 2.0
        in foldl f 0.0 (zip states actions)

      optimize : GaussianProcess -> Double -> GaussianProcess
      optimize gp cost = gp
  in optimize (gaussianProcess initialMean initialCovariance) (gptdCost (gaussianProcess initialMean initialCovariance))