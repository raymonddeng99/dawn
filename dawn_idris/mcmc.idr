module MetropolisHastings

import Data.So

%default total

targetDensity : Double -> Double
targetDensity x = exp (-x * x / 2)

proposalDensity : Double -> Double
proposalDensity x = exp (-x * x / 2) / sqrt (2 * pi)

randUniform : IO Double
randUniform = do
  n <- rand (0, topBound {to=Integer})
  let scaled = (cast n) / (topBound {to=Double})
  pure scaled

randGaussian : IO Double
randGaussian = do
  u1 <- randUniform
  u2 <- randUniform
  let z1 = sqrt (-2 * log u1) * cos (2 * pi * u2)
  pure z1

metropolisHastings : Double -> Nat -> IO (Double, Nat)
metropolisHastings initialState numIters = go initialState 0 numIters
  where
    go : Double -> Nat -> Nat -> IO (Double, Nat)
    go state numAccepted 0 = pure (state, numAccepted)
    go state numAccepted n = do
      proposedState <- (state +) <$> randGaussian
      let acceptanceRatio = (targetDensity proposedState / targetDensity state) *
                            (proposalDensity (state - proposedState) / proposalDensity (proposedState - state))
      u <- randUniform
      let numAccepted' = if acceptanceRatio >= 1 || acceptanceRatio > u
                            then numAccepted + 1
                            else numAccepted
      let state' = if acceptanceRatio >= 1 || acceptanceRatio > u
                      then proposedState
                      else state
      go state' numAccepted' (n - 1)