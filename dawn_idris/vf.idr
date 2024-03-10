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