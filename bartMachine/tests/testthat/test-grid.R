library(withr)

test_files <- paste0("../grid_tests/", c(
  "test-exports-basic.R",
  "test-exports-diagnostics.R",
  "test-exports-model-selection.R"
))

run_test_for_combo <- function(combo) {
  model_num_trees <- ifelse(
    is.null(combo$num_trees),
    BART_TESTS$model_num_trees,
    combo$num_trees
  )
  model_burn_in <- ifelse(
    is.null(combo$num_burn_in),
    BART_TESTS$model_burn_in,
    combo$num_burn_in
  )
  model_iter <- ifelse(
    is.null(combo$num_iter),
    BART_TESTS$model_iter,
    combo$num_iter
  )
  env_vars <- c(
    N_TEST = as.character(combo$n),
    P_TEST = as.character(combo$p),
    N_CLASS_TEST = as.character(combo$n),
    P_CLASS_TEST = as.character(combo$p),
    MODEL_NUM_TREES = as.character(model_num_trees),
    MODEL_BURN_IN = as.character(model_burn_in),
    MODEL_ITER = as.character(model_iter)
  )
  combo_label <- sprintf("n=%s p=%s trees=%s burn_in=%s iter=%s", combo$n, combo$p, model_num_trees, model_burn_in, model_iter)
  message("Running test grid for ", combo_label)
  with_envvar(env_vars, {
    for (test_file in test_files) {
      testthat::test_file(test_file)
    }
  })
}

for (combo in BART_TESTS$np_grid) {
  run_test_for_combo(combo)
}
