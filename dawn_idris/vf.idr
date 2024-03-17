-- RL: Parametric Value Function Approximation
import Data.Vect
import Data.Matrix




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



-- Kalman Temporal Differences
kalmanTemporalDifferences : Double -> Double -> Double -> Double -> Double -> Matrix Double -> Matrix Double -> (Int -> Int -> Matrix Double) -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> IO ()
kalmanTemporalDifferences gamma lambda alphaTheta alphaV alphaW rho phi featureMap initialTheta initialV initialW =
  let
    dimTheta = nrows initialTheta
    dimV = nrows initialV
    dimW = nrows initialW

    loop : Int -> Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> IO ()
    loop state action theta v w p = do
      let x = featureMap state action
          next_state = getState
          reward = getReward
          next_action = getAction next_state
          xNext = featureMap next_state next_action

          delta = reward + gamma * (sum $ zipWith (*) (toList $ flatten $ theta * xNext) (repeat 1.0)) - (sum $ zipWith (*) (toList $ flatten $ theta * x) (repeat 1.0))
          phiTrans = transpose phi
          k = p * (phiTrans * (phi * p * phiTrans + scalar lambda))
          theta' = theta + (k * delta * scalar alphaTheta)
          v' = v + ((delta - (phi * v)) * scalar alphaV)
          w' = w + ((x - (phi * w)) * scalar alphaW)
          p' = rho * transpose rho - (k * (phi * p))
      loop next_state next_action theta' v' w' p'
  in
    loop (get_state ()) (get_action (get_state ())) initialTheta initialV initialW (rho * transpose rho)


-- Fixed-point least-squares temporal difference
lstdFixedPoint : (phi : s -> Vect n Double) -> (gamma : Double) -> (samples : List (s, Double, s)) -> Vect n Double
lstdFixedPoint phi gamma samples = solve a b
  where
    n : Nat
    n = length (phi (fst (head samples)))
    a : Vect n (Vect n Double)
    a = [[ sum [ (phi x `index` i) * gamma * (phi x `index` j) | x <- map fst samples ] | j <- [0..n-1] ] | i <- [0..n-1]]
    b : Vect n Double
    b = [ sum [ (phi x `index` i) * (r - sum (phi y)) | (x, r, y) <- samples ] | i <- [0..n-1]]

    solve : Vect n (Vect n Double) -> Vect n Double -> Vect n Double



-- Statistically linear least-squares temporal difference
import Data.Vect

slLstd : Vect n Double -> Vect n Double -> Vect n (Vect n Double) -> Vect n (Vect n Double) -> List (Double, Double, Double, Double, Double) -> (Vect n Double, Vect n Double, Vect n (Vect n Double), Vect n (Vect n Double), Vect n (Vect n Double))
slLstd theta0 m0 p0 sigma0 transitions = loop theta0 m0 p0 sigma0 sigma0 transitions
  where
    p : Nat
    p = length theta0

    dim : Nat
    dim = p

    lambda : Double
    lambda = 1e-5 + 2.0 * cast p / (1.0 - cast (2 * p))

    unscented_transform : Vect n Double -> Vect n (Vect n Double) -> (Vect (2 * n + 1) (Vect n Double), Vect (2 * n + 1) Double)
    unscented_transform mean cov = let n = length mean
                                       gamma = sqrt (cast n + lambda)
                                       sigma_points = index 0 mean :: [index (idx + 1) (zipWith (+) mean (map ((*) (gamma * sqrt)) (diag cov))) | idx <- [0..2 * n - 1]]
                                       weights = replicate (S (S (S O))) 0.0 [lambda / (cast n + lambda), replicate n (1.0 / (2.0 * (cast n + lambda)))]
                                   in (sigma_points, weights)
      where
        diag : Vect n (Vect n Double) -> Vect n Double
        diag [] = []
        diag ((x :: xs) :: xss) = x :: diag xss

    sherman_morrison_update : Vect n (Vect n Double) -> Vect n Double -> Vect n (Vect n Double)
    sherman_morrison_update mat vec = let k = 1.0 + (vec `dot` (mat `vmvprod` vec))
                                       in map (\row => map (\x => x - (x * (vec `dot` row) / k)) row) mat

    loop : Vect n Double -> Vect n Double -> Vect n (Vect n Double) -> Vect n (Vect n Double) -> Vect n (Vect n Double) -> List (Double, Double, Double, Double, Double) -> (Vect n Double, Vect n Double, Vect n (Vect n Double), Vect n (Vect n Double), Vect n (Vect n Double))
    loop theta m p_inv sigma sigma_inv [] = (theta, m, p_inv, sigma, sigma_inv)
    loop theta m p_inv sigma sigma_inv ((s, a, r, s_prime, _) :: rest) = let (sigma_points_theta, weights_theta) = unscented_transform theta p_inv
                                                                              (sigma_points_sigma, weights_sigma) = unscented_transform theta sigma
                                                                              q_sigma_points = map (f s a) sigma_points_theta
                                                                              pq_sigma_points = map (pf s a) sigma_points_sigma
                                                                              (q_bar, p_qtheta) = statistics_from q_sigma_points weights_theta
                                                                              (pq_bar, p_sigma_pq) = statistics_from pq_sigma_points weights_sigma
                                                                              a = p_inv `vmvprod` p_qtheta
                                                                              c = sigma_inv `vmvprod` p_sigma_pq
                                                                              k = m `vmvprod` (minus a c)
                                                                              td_error = r + 0.99 * pq_bar - q_bar
                                                                              theta_prime = zipWith (+) theta (map (* td_error) k)
                                                                              m_prime = minus m (map (* (m `vmvprod` (minus a c))) k)
                                                                              p_inv_prime = sherman_morrison_update p_inv (minus a c)
                                                                              sigma_prime = sherman_morrison_update sigma p_sigma_pq
                                                                              sigma_inv_prime = sherman_morrison_update sigma_inv p_sigma_pq
                                                                           in loop theta_prime m_prime p_inv_prime sigma_prime sigma_inv_prime rest

    f : Double -> Double -> Vect n Double -> Double
    f _ _ _ = 0.0

    pf : Double -> Double -> Vect n Double -> Vect n Double
    pf _ _ _ = replicate _ 0.0

    statistics_from : Vect (2 * n + 1) Double -> Vect (2 * n + 1) Double -> (Double, Vect n Double)
    statistics_from _ _ = (0.0, replicate _ 0.0)

    vmvprod : Vect n (Vect m Double) -> Vect m Double -> Vect n Double
    vmvprod [] [] = ?vmvprod_rhs
    vmvprod (row :: rows) vec = let res = row `dot` vec
                                 in res :: vmvprod rows vec

    dot : Vect n Double -> Vect n Double -> Double
    dot xs ys = sum (zipWith (*) xs ys)

    minus : Vect n Double -> Vect n Double -> Vect n Double
    minus xs ys = zipWith (-) xs ys