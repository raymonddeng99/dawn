package main

import (
	"fmt"
	"math"
)

func lassoIteration(measurementMatrix [][]float64, measurements []float64, sparsityLevel int, tolerance, stepSize float64, maxIter int) []float64 {
	n, m := len(measurements), len(measurementMatrix[0])
	signalEstimate := make([]float64, m)

	iteration := 0
	for iteration < maxIter {
		residual := vecsubstract(measurements, matmul(measurementMatrix, signalEstimate))
		gradient := matmul(transpose(negate(measurementMatrix)), residual)
		stepSize = 1.0 / vecnorm(gradient)

		newSignalEstimate := make([]float64, m)
		numNonZero := 0
		for i := range signalEstimate {
			x := signalEstimate[i] + stepSize*gradient[i]
			newSignalEstimate[i] = softThresholding(x, tolerance*stepSize)
			if newSignalEstimate[i] != 0.0 {
				numNonZero++
			}
		}

		if numNonZero <= sparsityLevel {
			return newSignalEstimate
		}

		signalEstimate = newSignalEstimate
		iteration++
	}

	return signalEstimate
}

func recoverSignal(measurementMatrix [][]float64, measurements []float64, sparsityLevel int, tolerance float64, maxIter int) []float64 {
	stepSize := 1.0 / vecnorm(matmul(transpose(measurementMatrix), measurements))
	return lassoIteration(measurementMatrix, measurements, sparsityLevel, tolerance, stepSize, maxIter)
}