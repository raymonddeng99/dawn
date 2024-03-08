-- RL: Parametric Value Function Approximation

import Data.Vector (Vector, zip, zipWith, (!))
import qualified Data.Vector as V

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