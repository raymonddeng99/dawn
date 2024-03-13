-- RL: Parametric Value Function Approximation

import Control.Monad (when)
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Array (Array, array, accumArray, assocs, (!))
import Data.List (maximumBy)
import System.Random (getStdRandom, randomR)
import Data.Vector (Vector, zip, zipWith, (!))
import qualified Data.Vector as V



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