options(java.parameters = c("-Xmx4g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))
library(bartMachine)
set_bart_machine_num_cores(4) 

set.seed(42)
n = 500
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) + 20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

# 1. K-Fold CV
cat("Running K-Fold CV benchmark...\n")
t1 = system.time({
  cv_res = k_fold_cv(X = X, y = y, k_folds = 5, num_trees = 20, num_burn_in = 100, num_iterations_after_burn_in = 200, verbose = FALSE)
})
cat("CV RMSE:", cv_res$rmse, "Time:", t1[3], "s\n")

# 2. Variable Selection by Permute
cat("\nRunning Variable Selection Permutation benchmark...\n")
bm = bartMachine(X, y, num_trees = 20, num_burn_in = 100, num_iterations_after_burn_in = 200, verbose = FALSE, serialize = FALSE, seed = 42)
t2 = system.time({
  var_sel = var_selection_by_permute(bm, num_reps_for_avg = 5, num_permute_samples = 20, num_trees_for_permute = 10, plot = FALSE, verbose = FALSE)
})
cat("Important Vars:", paste(var_sel$important_vars_local_names, collapse=", "), "Time:", t2[3], "s\n")

# 3. Covariate Importance Test
cat("\nRunning Covariate Importance benchmark...\n")
t3 = system.time({
  cov_imp = cov_importance_test(bm, covariates = c("X1", "X2"), num_permutation_samples = 20, plot = FALSE, verbose = FALSE)
})
cat("P-val:", cov_imp$p_val, "Time:", t3[3], "s\n")
