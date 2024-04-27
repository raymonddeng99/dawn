import Data.Vector (Vector, fromList, zipWith, map, sum, replicate, (!))
import Numeric.LinearAlgebra (Matrix, vector, flatten, transpose, (#>), scale)

softThresholding :: Double -> Double -> Double -> Double
softThresholding x threshold
  | x > threshold = x - threshold
  | x < -threshold = x + threshold
  | otherwise = 0

lassoIteration :: Matrix Double -> Vector Double -> Int -> Double -> Double -> Int -> Vector Double
lassoIteration measurementMatrix measurements sparsityLevel tolerance stepSize maxIter
  | iteration == maxIter = signalEstimate
  | otherwise =
    let residual = vecsubstract measurements (matmul measurementMatrix signalEstimate)
        gradient = matmul (transpose $ map (map negate) measurementMatrix) residual
        stepSize = 1 / vecnorm gradient
        newSignalEstimate = map (softThresholding tolerance stepSize) (vecadd signalEstimate (map (*stepSize) gradient))
        numNonZero = length $ filter (/=0) newSignalEstimate
    in if numNonZero <= sparsityLevel
       then newSignalEstimate
       else lassoIteration measurementMatrix measurements sparsityLevel tolerance stepSize (iteration + 1)
  where
    signalEstimate = replicate (ncols measurementMatrix) 0
    iteration = 0

recoverSignal :: Matrix Double -> Vector Double -> Int -> Double -> Int -> Vector Double
recoverSignal measurementMatrix measurements sparsityLevel tolerance maxIter =
  lassoIteration measurementMatrix measurements sparsityLevel tolerance (1 / vecnorm (matmul (transpose measurementMatrix) measurements)) maxIter