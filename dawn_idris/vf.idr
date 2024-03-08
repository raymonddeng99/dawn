-- RL: Parametric Value Function Approximation
import Data.Vect

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