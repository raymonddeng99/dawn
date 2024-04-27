import numpy as np

def soft_thresholding(x, threshold):
    if x > threshold:
        return x - threshold
    elif x < -threshold:
        return x + threshold
    else:
        return 0.0

def lasso_iteration(measurement_matrix, measurements, sparsity_level, tolerance, step_size, max_iter):
    n, m = measurements.shape[0], measurement_matrix.shape[1]
    signal_estimate = np.zeros(m)

    iteration = 0
    while iteration < max_iter:
        residual = vecsubstract(measurements, matmul(measurement_matrix, signal_estimate))
        gradient = matmul(measurement_matrix.T * -1, residual)
        step_size = 1.0 / vecnorm(gradient)

        new_signal_estimate = np.zeros(m)
        num_non_zero = 0
        for i in range(m):
            x = signal_estimate[i] + step_size * gradient[i]
            new_signal_estimate[i] = soft_thresholding(x, tolerance * step_size)
            if new_signal_estimate[i] != 0.0:
                num_non_zero += 1

        if num_non_zero <= sparsity_level:
            return new_signal_estimate

        signal_estimate = new_signal_estimate
        iteration += 1

    return signal_estimate

def recover_signal(measurement_matrix, measurements, sparsity_level, tolerance, max_iter):
    step_size = 1.0 / vecnorm(matmul(measurement_matrix.T, measurements))
    return lasso_iteration(measurement_matrix, measurements, sparsity_level, tolerance, step_size, max_iter)