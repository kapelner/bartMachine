#!/usr/bin/env Rscript

options(java.parameters = "-Xmx10g")
suppressPackageStartupMessages({
  library(bartMachine)
})
set_bart_machine_num_cores(4)

plot_env <- asNamespace("bartMachine")
plot_source <- file.path("bartMachine", "R", "bart_package_plots.R")
if (file.exists(plot_source)) {
  plot_env <- new.env(parent = plot_env)
  sys.source(plot_source, envir = plot_env)
}
plot_fn <- function(name) {
  get(name, envir = plot_env, inherits = TRUE)
}

run_test <- function(name, expr) {
  message("Running ", name, "...")
  tryCatch(
    {
      expr
      TRUE
    },
    error = function(e) {
      stop(name, " failed: ", e$message)
    }
  )
}

set.seed(1984)
n <- 500
p <- 8
X <- data.frame(matrix(runif(n * p), ncol = p))
y <- 3 * X[, 1] + 2 * X[, 2] + rnorm(n, sd = 0.2)
num_replicates_for_avg = 4
num_trees_bottleneck = 10

bart_machine <- bartMachine(
  X,
  y,
  num_trees = 100,
  num_burn_in = 1000,
  num_iterations_after_burn_in = 1000,
  flush_indices_to_save_RAM = FALSE,
  verbose = FALSE
)

plot_file <- file.path("bartMachine", "inst", "scripts", "bartmachine_ggplot_tests.pdf")
grDevices::pdf(plot_file, width = 8, height = 6)
on.exit(grDevices::dev.off(), add = TRUE)

run_test("check_bart_error_assumptions", plot_fn("check_bart_error_assumptions")(bart_machine))
run_test("plot_y_vs_yhat_default", plot_fn("plot_y_vs_yhat")(bart_machine))
run_test("plot_y_vs_yhat_credible", plot_fn("plot_y_vs_yhat")(bart_machine, credible_intervals = TRUE))
run_test("get_sigsqs_plot", plot_fn("get_sigsqs")(bart_machine, plot_hist = TRUE))
run_test(
  "investigate_var_importance",
  plot_fn("investigate_var_importance")(
    bart_machine,
    plot = TRUE,
    num_replicates_for_avg = num_replicates_for_avg,
    num_trees_bottleneck = num_trees_bottleneck,
    num_var_plot = p
  )
)
run_test("plot_convergence_diagnostics", plot_fn("plot_convergence_diagnostics")(bart_machine))
run_test(
  "interaction_investigator",
  plot_fn("interaction_investigator")(
    bart_machine,
    plot = TRUE,
    num_replicates_for_avg = num_replicates_for_avg,
    num_trees_bottleneck = num_trees_bottleneck,
    num_var_plot = p
  )
)
run_test("pd_plot", plot_fn("pd_plot")(bart_machine, j = 1, prop_data = 0.3))
run_test("pd_plot", plot_fn("pd_plot")(bart_machine, j = 2, prop_data = 0.3))
run_test("pd_plot", plot_fn("pd_plot")(bart_machine, j = 3, prop_data = 0.3))
run_test(
  "rmse_by_num_trees",
  plot_fn("rmse_by_num_trees")(
    bart_machine,
    tree_list = c(10, 50),
    in_sample = TRUE,
    plot = TRUE,
    num_replicates = num_replicates_for_avg
  )
)

message("ggplot tests completed. Output PDF: ", plot_file)
