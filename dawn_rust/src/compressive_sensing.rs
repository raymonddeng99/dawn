use std::f64::consts::SQRT_2;

fn soft_thresholding(x: f64, threshold: f64) -> f64 {
    if x > threshold {
        x - threshold
    } else if x < -threshold {
        x + threshold
    } else {
        0.0
    }
}

fn lasso_iteration(
    measurement_matrix: &[Vec<f64>],
    measurements: &[f64],
    sparsity_level: usize,
    tolerance: f64,
    step_size: f64,
    max_iter: usize,
) -> Vec<f64> {
    let n = measurements.len();
    let m = measurement_matrix[0].len();
    let mut signal_estimate = vec![0.0; m];

    let mut iteration = 0;
    while iteration < max_iter {
        let residual = vecsubstract(measurements, &matmul(measurement_matrix, &signal_estimate));
        let mut gradient = matmul(
            &measurement_matrix
                .iter()
                .map(|row| row.iter().map(|x| -x).collect())
                .collect(),
            &residual,
        );
        let step_size = 1.0 / vecnorm(&gradient);

        let mut new_signal_estimate = vec![0.0; m];
        let mut num_non_zero = 0;
        for i in 0..m {
            new_signal_estimate[i] = soft_thresholding(
                signal_estimate[i] + step_size * gradient[i],
                tolerance * step_size,
            );
            if new_signal_estimate[i] != 0.0 {
                num_non_zero += 1;
            }
        }

        if num_non_zero <= sparsity_level {
            return new_signal_estimate;
        }

        signal_estimate = new_signal_estimate;
        iteration += 1;
    }

    signal_estimate
}

fn recover_signal(
    measurement_matrix: &[Vec<f64>],
    measurements: &[f64],
    sparsity_level: usize,
    tolerance: f64,
    max_iter: usize,
) -> Vec<f64> {
    let step_size = 1.0 / vecnorm(&matmul(&measurement_matrix.iter().map(|row| row.iter().cloned().collect()).collect(), measurements));
    lasso_iteration(
        measurement_matrix,
        measurements,
        sparsity_level,
        tolerance,
        step_size,
        max_iter,
    )
}