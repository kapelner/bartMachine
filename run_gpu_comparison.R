# run_gpu_comparison.R
args = commandArgs(trailingOnly = TRUE)
lib_path = args[1]
output_file = args[2]
use_gpu = as.logical(args[3])

options(java.parameters = c("-Xmx10g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))
library(bartMachine, lib.loc = lib_path)
set_bart_machine_num_cores(1)
options(bartMachine.use_gpu = use_gpu)

# Load shared data and models
load("shared_comparison_data.RData")
load("shared_models.RData")

# Ensure the java object is active (it should be because we used serialize=TRUE and .jpackage is called on library())
# Actually, we need to call check_serialization or similar if it's a new session
# bartMachine's S3 class handles deserialization on first use if it was serialized.

results = list()

# 1. Regression Predictions
t1 = system.time({ pred_mean = predict(bm_reg, X) })
results$reg_mean = list(val = pred_mean, time = t1[3])

t2 = system.time({ post_samples = bart_machine_get_posterior(bm_reg, X)$y_hat_posterior_samples })
results$reg_samples = list(val = post_samples, time = t2[3])

t3 = system.time({ cred_int = calc_credible_intervals(bm_reg, X) })
results$reg_cred = list(val = cred_int, time = t3[3])

# 2. Classification
t4 = system.time({ pred_probs = predict(bm_class, X, type = "prob") })
results$class_probs = list(val = pred_probs, time = t4[3])

t5 = system.time({ post_samples_class = bart_machine_get_posterior(bm_class, X)$y_hat_posterior_samples })
results$class_samples = list(val = post_samples_class, time = t5[3])

saveRDS(results, output_file)
cat("Completed results for", lib_path, "(GPU:", use_gpu, ")\n")
