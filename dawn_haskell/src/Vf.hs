-- RL: Parametric Value Function Approximation

import Control.Monad (when, foldM)
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Array (Array, array, accumArray, assocs, (!))
import Data.List (maximumBy)
import System.Random (getStdRandom, randomR)
import Data.Vector (Vector, zip, fromList, zipWith, map, (!))
import qualified Data.Vector as V
import Numeric.LinearAlgebra

-- TD with Function Approximation

linearFa :: Vector Double -> Vector Double -> Double
linearFa state theta = V.sum $ V.zipWith (*) state theta

tdVFA :: Environment -> Policy -> Double -> Vector Double -> Int -> Double -> IO (Vector Double)
tdVFA env policy gamma theta0 numEpisodes alpha = go theta0 0
  where
    go theta episodeCount
      | episodeCount == numEpisodes = return theta
      | otherwise = do
          state <- envReset env
          theta' <- episode theta state
          go theta' (episodeCount + 1)

    episode theta state = go' theta state
      where
        go' theta state
          | envIsDone env state = return theta
          | otherwise = do
              action <- policy state
              (nextState, reward) <- envStep env action state
              let delta = reward + gamma * linearFa nextState theta - linearFa state theta
                  theta' = V.zipWith (\t s -> t + alpha * delta * s) theta state
              go' theta' nextState



-- SARSA with Function Approximation
type State = [Double]
type Action = Double

qApprox :: Array (State, Action) Double -> State -> Action -> Double
qApprox weights state action =
  sum $ zipWith (*) (map ($ state, action) featureIndices) (elems weights)

epsilonGreedy :: Double -> (State -> Action -> Double) -> State -> [Action] -> IO Action
epsilonGreedy epsilon q state actions = do
  explore <- randomIO :: IO Double
  if explore < epsilon
    then randomIO :: IO Action
    else return $ fst $ maximumBy (compare `on` snd) [(a, q state a) | a <- actions]

sarsaUpdate :: Array (State, Action) Double -> State -> Action -> Double -> State -> Action -> Double -> Double -> Array (State, Action) Double
sarsaUpdate weights state action reward nextState nextAction gamma alpha =
  accumArray (+) 0 [(f, alpha * tdError * feat) | f <- featureIndices]
    (assocs weights)
  where
    qCur = qApprox weights state action
    qNext = qApprox weights nextState nextAction
    tdError = reward + gamma * qNext - qCur
    feat = featureValue state action

sarsa :: StateT (Array (State, Action) Double) IO ()
sarsa = do
  get >>= \weights -> when (not $ null weights) $ do
    state <- getInitialState
    action <- epsilonGreedy epsilon qApprox state actions
    stepEpisode state action

stepEpisode :: State -> Action -> StateT (Array (State, Action) Double) IO ()
stepEpisode state action = do
  (nextState, reward) <- getTransition state action
  if isTerminal nextState
    then sarsa
    else do
      nextAction <- epsilonGreedy epsilon qApprox nextState actions
      weights <- get
      let newWeights = sarsaUpdate weights state action reward nextState nextAction gamma alpha
      put newWeights
      stepEpisode nextState nextAction

randomIO :: (RandomGen g) => StateT g IO Double
randomIO = getStdRandom (randomR (0, 1))

featureIndices :: [(State, Action) -> Double]

featureValue :: State -> Action -> Double
featureValue state action = sum $ map ($ (state, action)) featureIndices


-- Q-learning with Function Approximation
qLearningFuncApprox :: Double -> Int -> (Theta -> State -> Double) -> (State -> Action -> (State, Reward)) -> Env -> Theta
qLearningFuncApprox gamma episodes targetFunc observeFunc env = loop initTheta
  where
    loop theta s =
      let a = chooseAction theta s
          (s', r) = envStep env s a
          target = r + gamma * targetFunc theta s'
          theta' = updateWeights theta s a target
      in if terminated env s' then theta'
         else loop theta' s'

    initTheta = initializeWeights
    episode theta 0 = theta
    episode theta i =
      let s0 = envReset env
          theta' = loop theta s0
      in episode theta' (i - 1)

-- LS Kalman Filter
fixedPointKalmanFilter :: [(State, Action, Float)] -> Vector -> Matrix -> Vector
fixedPointKalmanFilter samples theta0 p0 =
  fst $ foldl' step (theta0, p0) samples
  where
    step (theta, p) (s, a, q) =
      let phi = featureVector s a
          k = p #> phi #/ (1 + phi `transpose` #> p #> phi)
          theta' = theta + k * (q - phi `transpose` #> theta)
          p' = p - k #> (phi `transpose` #> p)
      in (theta', p')

featureVector :: State -> Action -> Vector



-- Residual SGD
residualSgd :: Int -> Double -> (S -> A -> Double) -> (S -> A -> [S]) -> (S -> A -> S -> Double) -> Double -> (S -> A -> Double)
residualSgd iterations learningRate initialQ transitionFunction rewardFunction discountFactor = sgdLoop initialQ 0
  where
    bellmanOperator q s a =
      let nextStates = transitionFunction s a
          nextStateValues =
            sum [rewardFunction s a s' + discountFactor * maximum [q s' a' | a' <- nextActions] | s' <- nextStates]
       in nextStateValues / fromIntegral (length nextStates)

    costFunction q = sum [(q_sa - bellmanOperator q s a) ** 2 | (s, a) <- stateActionPairs]

    residualSgdUpdate q s a =
      let q_sa = initialQ s a
          t_q_sa = bellmanOperator q s a
          gradient = 2.0 * (q_sa - t_q_sa)
       in q_sa - learningRate * gradient

    sgdLoop q iter
      | iter == iterations = q
      | otherwise =
        let (s, a) = randomStateActionPair ()
            q' = residualSgdUpdate q s a
         in sgdLoop q' (iter + 1)



-- Gaussian Process Temporal Difference
data GaussianProcess = GaussianProcess { mean :: Int -> Double, covariance :: Int -> Int -> Double }

gptd :: (Int -> Double) -> (Int -> Int -> Double) -> [[Int]] -> [[Int]] -> GaussianProcess
gptd initial_mean initial_covariance states actions =
  let dim = length (head states)
      
      gaussianProcess mean covariance =
        GaussianProcess { mean = \state -> mean state, covariance = \state1 state2 -> covariance state1 state2 }
      
      transitionModel state action = ([], Array.array (0, -1) [])
      
      reward state action = 0.0
      
      bellmanOperator gp state action =
        let (nextStates, rewards) = transitionModel state action
            nextValues = Array.listArray (0, -1) (map (mean gp) nextStates)
        in sum (zipWith (*) (Array.elems nextValues) rewards)
      
      gptdCost gp =
        foldl' (\cost (state, action) ->
                  let target = bellmanOperator gp state action
                      value = mean gp state
                      error = target - value
                  in cost + error ** 2.0) 0.0 (zip states actions)
      
      optimize gp cost = gp
  in optimize (gaussianProcess initial_mean initial_covariance) (gptdCost (gaussianProcess initial_mean initial_covariance))


-- Kalman Temporal Difference
kalmanTemporalDifferences :: Double -> Double -> Double -> Double -> Double -> Matrix Double -> Matrix Double -> (Int -> Int -> Matrix Double) -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> IO ()
kalmanTemporalDifferences gamma lambda alphaTheta alphaV alphaW rho phi featureMap initialTheta initialV initialW = do
  let dimTheta = size (rows initialTheta)
      dimV = size (rows initialV)
      dimW = size (rows initialW)
      theta = initialTheta
      v = initialV
      w = initialW
      p = rho <> tr rho
      loop state action = do
        let x = featureMap state action
        next_state = getState
        reward = getReward
        next_action = getAction next_state
        xNext = featureMap next_state next_action
        delta = reward + gamma * (theta #> xNext) - (theta #> x)
        phiTrans = tr phi
        k = p <> phiTrans <> ((phi <> p <> phiTrans) + scalar lambda)
        theta' = theta + (k <> (scalar alphaTheta * delta))
        v' = v + ((delta - (phi #> v)) * scalar alphaV)
        w' = w + ((x - (phi #> w)) * scalar alphaW)
        p' = rho <> tr rho - (k <> (phi <> p))
        loop next_state next_action
  loop getState (getAction (getState))




-- Fixed-point least-squares temporal difference
lstdFixedPoint :: (Num a) => (s -> [a]) -> a -> [(s, a, s)] -> [a]
lstdFixedPoint phi gamma samples = solve (fromLists [zip [1..n] row | row <- transpose a]) b
  where
    n = length . head . map (phi . fst) $ samples
    a = [[(phi x !! i) * gamma * (phi x !! j) | j <- [1..n]] | x <- map fst samples, i <- [1..n]]
    b = [sum [(phi x !! i) * (r - sum (phi y)) | (x, r, y) <- samples] | i <- [1..n]]



-- Statistically linear least-squares temporal difference
slLstd theta0 m0 p0 sigma0 transitions = foldM loopStep (theta0, m0, p0, sigma0, sigma0) transitions
  where
    p = V.length theta0
    dim = p
    lambda = 1e-5 + 2.0 * (fromIntegral p) / (1.0 - (fromIntegral (2 * p)))
    unscented_transform mean cov =
      let n = V.length mean
          gamma = sqrt (fromIntegral n + lambda)
          sigma_points = V.generateM (2 * n + 1) $ \i ->
            if i == 0
            then return mean
            else
              let idx = i - 1
                  sign = if i <= n then 1 else -1
               in return $ V.zipWith (\x y -> x + sign * gamma * sqrt y) mean (cov `V.indexed` idx)
          weights = V.generateM (2 * n + 1) $ \i ->
            if i == 0
            then return (lambda / (fromIntegral n + lambda))
            else return (1.0 / (2.0 * (fromIntegral n + lambda)))
       in (sigma_points, weights)
    sherman_morrison_update mat vec =
      let k = 1.0 + V.dot vec (mat `V.mvprod` vec)
       in mat - (mat `V.outerProd` vec) `V.scale` (1.0 / k)
    loopStep (theta, m, p_inv, sigma, sigma_inv) (s, a, r, s', _) =
      let (sigma_points_theta, weights_theta) = unscented_transform theta p
          (sigma_points_sigma, weights_sigma) = unscented_transform theta sigma
          q_sigma_points = map (f s a) sigma_points_theta
          pq_sigma_points = map (pf s a) sigma_points_sigma
          (q_bar, p_qtheta) = statistics_from q_sigma_points weights_theta
          (pq_bar, p_sigma_pq) = statistics_from pq_sigma_points weights_sigma
          a = p_inv `V.mvprod` p_qtheta
          c = sigma_inv `V.mvprod` p_sigma_pq
          k = m `V.mvprod` (a - c)
          td_error = r + gamma * pq_bar - q_bar
          theta' = theta + k `V.scale` td_error
          m' = m - k `V.mvprod` (m `V.mvprod` (a - c))
          p' = sherman_morrison_update p (a - c)
          p_inv' = sherman_morrison_update p_inv (p_qtheta - p_sigma_pq)
          sigma' = sherman_morrison_update sigma p_sigma_pq
          sigma_inv' = sherman_morrison_update sigma_inv p_sigma_pq
       in (theta', m', p_inv', sigma', sigma_inv')



-- Gaussian Temporal Difference 2, Sutton 2009
import Data.Vector (Vector, fromList, toList, zipWith, (!))
import qualified Data.Vector as V

gtd2 :: Double -> Double -> Double -> [Vector Double] -> [Double] -> (Vector Double, Vector Double)
gtd2 alpha eta gamma features rewards =
    let p = V.length $ head features
        theta = V.replicate p 0
        w = V.replicate p 0
        updateTheta theta w i = V.zipWith (\x y -> x + alpha * tdError * (y - gamma * (features !! (i+1) ! j) * w ! j)) theta (features !! i)
            where tdError = rewards !! i + gamma * (V.sum $ V.zipWith (*) (features !! (i+1)) theta) - (V.sum $ V.zipWith (*) (features !! i) theta)
        updateW w i = V.zipWith (\x y -> x + eta * alpha * (tdError * y - x * (V.sum $ V.zipWith (*) (features !! i) (features !! i)))) w (features !! i)
            where tdError = rewards !! i + gamma * (V.sum $ V.zipWith (*) (features !! (i+1)) theta) - (V.sum $ V.zipWith (*) (features !! i) theta)
        aux theta w i
            | i == length rewards - 1 = (theta, w)
            | otherwise = aux (updateTheta theta w i) (updateW w i) (i+1)
    in aux theta w 0



-- Temporal Difference with Correction
tdc :: Double -> Double -> Double -> (Int -> Int -> Vector Double) -> Vector Double -> Vector Double -> [Int] -> [Int] -> [Double] -> (Vector Double, Vector Double)
tdc gamma alpha beta featureFunction initTheta initOmega states actions rewards =
  aux initTheta initOmega (zip3 states (tail states ++ [head states]) (zip actions (tail actions ++ [head actions]))) (zip3 (rewards ++ [0]) (tail rewards ++ [0]) [0..])
  where
    aux theta omega [] _ = (theta, omega)
    aux theta omega ((s, s', (a, a')):stateActions) ((r, r', i):rewardsIdx) =
      let phi_s = featureFunction s a
          phi_s' = featureFunction s' a'
          q_s = sum $ zipWith (*) theta phi_s
          q_s' = sum $ zipWith (*) theta phi_s'
          tdError = r + gamma * q_s' - q_s
          thetaUpdate = zipWith (\w f -> w + alpha * tdError * f) theta phi_s
          omegaUpdate = zipWith (\w f -> w + beta * (tdError - sum (zipWith (*) phi_s' omega) * f)) omega phi_s
      in aux thetaUpdate omegaUpdate stateActions (zip3 (tail rewardsIdx) (tail $ tail rewardsIdx) [i+1..])


-- Fitted Q
fittedQ transitions initialQ =
  let sampledBellmanOperator q (s, a, r, s') =
        r + maximum [q s'' | s'' <- s']
      updateQ q (s, a, r, s') =
        (s, a, sampledBellmanOperator q (s, a, r, s'))
      iterate q trs =
        let q' = foldr (\ (s, a, r, s') q ->
                          map (\ (s', a', qVal) ->
                                if s' == s
                                then (s', a', updateQ q (s, a, r, s'))
                                else (s', a', qVal)) q) q trs
        in if all (\ (_, _, (qVal, qVal')) -> qVal == qVal') (zip q q')
           then q'
           else iterate q' trs
  in iterate (map (\ s -> map (\ a -> initialQ s a) actions) states) transitions