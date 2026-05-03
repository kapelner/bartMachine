# build_baseline_model.R
options(java.parameters = c("-Xmx10g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))
library(bartMachine, lib.loc = "R_libs/baseline")
set_bart_machine_num_cores(1)

load("shared_comparison_data.RData")

cat("Building baseline models...\n")
bm_reg = bartMachine(X, y, num_trees = 50, num_burn_in = 100, num_iterations_after_burn_in = 200, verbose = FALSE, serialize = TRUE, seed = 42)
bm_class = bartMachine(X, y_bin, num_trees = 50, num_burn_in = 100, num_iterations_after_burn_in = 200, verbose = FALSE, serialize = TRUE, seed = 42)

save(bm_reg, bm_class, file = "shared_models.RData")
cat("Models saved.\n")
