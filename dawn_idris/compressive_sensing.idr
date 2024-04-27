import Data.Vect

softThresholding : Double -> Double -> Double
softThresholding x threshold =
  if x > threshold
    then x - threshold
    else if x < -threshold
           then x + threshold
           else 0.0

lassoIteration : Vect n (Vect m Double) -> Vect n Double -> Int -> Double -> Double -> Int -> Vect m Double
lassoIteration measurementMatrix measurements sparsityLevel tolerance stepSize maxIter =
  let signalEstimate = replicate 0 m in
    go 0 signalEstimate
  where
    n : Nat
    n = length measurements

    m : Nat
    m = length $ index 0 measurementMatrix

    go : Int -> Vect m Double -> Vect m Double
    go iteration signalEstimate =
      if iteration == maxIter
        then signalEstimate
        else
          let residual = vecsubstract measurements $ matmul measurementMatrix signalEstimate
              gradient = matmul (map (map negate) measurementMatrix) residual
              stepSize = 1.0 / vecnorm gradient
              newSignalEstimate = map (\x => softThresholding (x + stepSize * index (cast x) gradient) (tolerance * stepSize)) signalEstimate
              numNonZero = length $ filter (/= 0.0) newSignalEstimate
          in
            if numNonZero <= sparsityLevel
              then newSignalEstimate
              else go (iteration + 1) newSignalEstimate

recoverSignal : Vect n (Vect m Double) -> Vect n Double -> Int -> Double -> Int -> Vect m Double
recoverSignal measurementMatrix measurements sparsityLevel tolerance maxIter =
  let stepSize = 1.0 / vecnorm (matmul (transpose measurementMatrix) measurements) in
    lassoIteration measurementMatrix measurements sparsityLevel tolerance stepSize maxIter