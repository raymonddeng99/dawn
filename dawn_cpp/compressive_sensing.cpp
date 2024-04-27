#include <iostream>
#include <vector>
#include <cmath>

double softThresholding(double x, double threshold) {
    if (x > threshold) {
        return x - threshold;
    } else if (x < -threshold) {
        return x + threshold;
    } else {
        return 0.0;
    }
}

std::vector<double> lassoIteration(const std::vector<std::vector<double>>& measurementMatrix,
                                   const std::vector<double>& measurements,
                                   int sparsityLevel, double tolerance, double stepSize,
                                   int maxIter) {
    int n = measurements.size();
    int m = measurementMatrix[0].size();
    std::vector<double> signalEstimate(m, 0.0);

    int iteration = 0;
    while (iteration < maxIter) {
        std::vector<double> residual = vecsubstract(measurements, matmul(measurementMatrix, signalEstimate));
        std::vector<std::vector<double>> negatedMatrix(measurementMatrix.size(), std::vector<double>(measurementMatrix[0].size()));
        for (size_t i = 0; i < measurementMatrix.size(); ++i) {
            for (size_t j = 0; j < measurementMatrix[i].size(); ++j) {
                negatedMatrix[i][j] = -measurementMatrix[i][j];
            }
        }
        std::vector<double> gradient = matmul(negatedMatrix, residual);
        stepSize = 1.0 / vecnorm(gradient);

        std::vector<double> newSignalEstimate(m);
        int numNonZero = 0;
        for (int i = 0; i < m; ++i) {
            double x = signalEstimate[i] + stepSize * gradient[i];
            newSignalEstimate[i] = softThresholding(x, tolerance * stepSize);
            if (newSignalEstimate[i] != 0.0) {
                ++numNonZero;
            }
        }

        if (numNonZero <= sparsityLevel) {
            return newSignalEstimate;
        }

        signalEstimate = newSignalEstimate;
        ++iteration;
    }

    return signalEstimate;
}

std::vector<double> recoverSignal(const std::vector<std::vector<double>>& measurementMatrix,
                                  const std::vector<double>& measurements,
                                  int sparsityLevel, double tolerance, int maxIter) {
    std::vector<std::vector<double>> transposedMatrix(measurementMatrix[0].size(), std::vector<double>(measurementMatrix.size()));
    for (size_t i = 0; i < measurementMatrix.size(); ++i) {
        for (size_t j = 0; j < measurementMatrix[i].size(); ++j) {
            transposedMatrix[j][i] = measurementMatrix[i][j];
        }
    }
    double stepSize = 1.0 / vecnorm(matmul(transposedMatrix, measurements));
    return lassoIteration(measurementMatrix, measurements, sparsityLevel, tolerance, stepSize, maxIter);
}