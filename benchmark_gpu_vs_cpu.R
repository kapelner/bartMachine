# benchmark_gpu_vs_cpu.R
# This script compares the performance and numerical parity of 
# multi-threaded CPU paths vs. GPU-accelerated paths in bartMachine v1.4.2.

options(java.parameters = c("-Xmx10g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))
library(bartMachine)

# --- Configuration ---
n = 5000         # Number of observations
p = 10           # Number of predictors
num_trees = 50
burn_in = 200
post_samples = 500
set_bart_machine_num_cores(4) # CPU parallelism

# --- Data Generation ---
set.seed(42)
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) + 20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
y_bin = factor(ifelse(y > median(y), "1", "0"))

# Check for GPU
has_gpu = isTRUE(getOption("bartMachine.use_gpu", TRUE)) && 
          .jfield("bartMachine.GpuPredictorBridge", "Z", "GPU_AVAILABLE")

if (!has_gpu) {
  cat("\n!!! WARNING: GPU/TornadoVM not detected on this system. !!!\n")
  cat("The 'GPU' timings below will actually be running the CPU fallback path.\n\n")
}

run_bench = function(name, model, test_data, type = "regression", coverage = 0.95) {
  cat(sprintf("\n--- Benchmarking %s ---\n", name))
  
  # Ensure GPU is OFF for CPU baseline
  options(bartMachine.use_gpu = FALSE)
  
  # 1. Mean Prediction
  t_cpu_mean = system.time({ res_cpu_mean = if(type=="regression") predict(model, test_data) else predict(model, test_data, type="prob") })[3]
  
  # 2. Posterior Samples
  t_cpu_samp = system.time({ res_cpu_samp = bart_machine_get_posterior(model, test_data)$y_hat_posterior_samples })[3]
  
  # 3. Credible Intervals
  t_cpu_cred = system.time({ res_cpu_cred = calc_credible_intervals(model, test_data, coverage = coverage) })[3]
  
  # Enable GPU
  options(bartMachine.use_gpu = TRUE)
  
  # 1. Mean Prediction (GPU)
  t_gpu_mean = system.time({ res_gpu_mean = if(type=="regression") predict(model, test_data) else predict(model, test_data, type="prob") })[3]
  
  # 2. Posterior Samples (GPU)
  t_gpu_samp = system.time({ res_gpu_samp = bart_machine_get_posterior(model, test_data)$y_hat_posterior_samples })[3]
  
  # 3. Credible Intervals (GPU)
  t_gpu_cred = system.time({ res_gpu_cred = calc_credible_intervals(model, test_data, coverage = coverage) })[3]
  
  # Numerical Comparison
  diff_mean = max(abs(res_cpu_mean - res_gpu_mean))
  diff_samp = max(abs(res_cpu_samp - res_gpu_samp))
  diff_cred = max(abs(res_cpu_cred - res_gpu_cred))
  
  cat(sprintf("%-20s | Time CPU: %6.3fs | Time GPU: %6.3fs | Speedup: %5.2fx | Diff: %e\n", 
              "Mean Pred", t_cpu_mean, t_gpu_mean, t_cpu_mean/t_gpu_mean, diff_mean))
  cat(sprintf("%-20s | Time CPU: %6.3fs | Time GPU: %6.3fs | Speedup: %5.2fx | Diff: %e\n", 
              "Post Samples", t_cpu_samp, t_gpu_samp, t_cpu_samp/t_gpu_samp, diff_samp))
  cat(sprintf("%-20s | Time CPU: %6.3fs | Time GPU: %6.3fs | Speedup: %5.2fx | Diff: %e\n", 
              "Cred Intervals", t_cpu_cred, t_gpu_cred, t_cpu_cred/t_gpu_cred, diff_cred))
}

# --- Execution ---
cat("Training models...\n")
bm_reg = bartMachine(X, y, num_trees = num_trees, num_burn_in = burn_in, 
                     num_iterations_after_burn_in = post_samples, verbose = FALSE, serialize = FALSE)
run_bench("Regression", bm_reg, X, "regression")

bm_class = bartMachine(X, y_bin, num_trees = num_trees, num_burn_in = burn_in, 
                       num_iterations_after_burn_in = post_samples, verbose = FALSE, serialize = FALSE)
run_bench("Classification", bm_class, X, "classification")

cat("\nBenchmark Complete.\n")
