
options(java.parameters = c("-Xmx20g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))
options(warn = 2)

library(bartMachine)

if (!exists("BART_TESTS")) {
  BART_TESTS <- list(
    core_count = 4,
    pdf_width = 8,
    pdf_height = 6,
    seed = 11,
    verbose = FALSE,
    default_n = 120,
    default_p = 4,
    default_class_p = 3,
    reg_coef_1 = 3,
    reg_coef_2 = 2,
    reg_noise_sd = 0.2,
    model_num_trees = 20,
    model_burn_in = 10,
    model_iter = 20,
    class_threshold = 0.5,
    small_data_n = 30,
    small_data_sd = 0.1,
    small_model_num_trees = 10,
    small_model_burn_in = 5,
    small_model_iter = 10,
    dummify_b_values = c(1, 2),
    arr_r = 2,
    posterior_subset_n = 5,
    credible_interval_cols = 2,
    prediction_samples_per_point = 20,
    covariate_index = 1,
    reg_feature_index_1 = 1,
    reg_feature_index_2 = 2,
    class_feature_index = 1,
    first_dim_index = 1,
    num_permute_samples = 5,
    linearity_num_trees = 10,
    linearity_num_burn_in = 5,
    linearity_num_iter = 10,
    num_replicates_for_avg = 1,
    num_trees_bottleneck = 5,
    num_var_plot = 3,
    min_plot_count = 1,
    pd_plot_prop_data = 0.2,
    rmse_tree_list = c(5, 10),
    rmse_num_replicates = 1,
    var_sel_k_folds = 2,
    var_sel_num_reps_for_avg = 1,
    var_sel_num_permute_samples = 5,
    var_sel_num_trees_for_permute = 5,
    var_sel_num_trees_pred_cv = 5,
    cv_data_n = 40,
    cv_data_sd = 0.1,
    cv_num_tree_cvs = c(5, 10),
    cv_num_tree_cvs_single = c(5),
    cv_k_cvs = c(2),
    cv_k_folds = 2,
    cv_num_burn_in = 5,
    cv_num_iter = 10,
    cv_num_trees = 5,
    raw_node_g = 1,
    test_idx_n = 20,
    test_files = c(
      "test-exports-basic.R",
      "test-exports-diagnostics.R",
      "test-exports-model-selection.R"
    ),
    np_grid = list(
      list(n = 60, p = 3, num_trees = 20, num_burn_in = 10, num_iter = 20),
      list(n = 120, p = 4, num_trees = 20, num_burn_in = 10, num_iter = 20),
      list(n = 200, p = 6, num_trees = 30, num_burn_in = 15, num_iter = 30),
      list(n = 240, p = 8, num_trees = 35, num_burn_in = 15, num_iter = 35),
      list(n = 320, p = 10, num_trees = 40, num_burn_in = 20, num_iter = 40),
      list(n = 400, p = 12, num_trees = 45, num_burn_in = 20, num_iter = 45),
      list(n = 480, p = 15, num_trees = 50, num_burn_in = 20, num_iter = 50)
    )
  )
}

get_test_env_int <- function(name, default) {
  val <- Sys.getenv(name, "")
  if (nzchar(val)) {
    as.integer(val)
  } else {
    default
  }
}

with_plot_device <- function(expr) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = BART_TESTS$pdf_width, height = BART_TESTS$pdf_height)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
  invisible(path)
}

build_regression_model <- function() {
  set.seed(BART_TESTS$seed)
  n <- as.integer(Sys.getenv("N_TEST", as.character(BART_TESTS$default_n)))
  p <- as.integer(Sys.getenv("P_TEST", as.character(BART_TESTS$default_p)))
  X <- data.frame(matrix(runif(n * p), ncol = p))
  y <- BART_TESTS$reg_coef_1 * X[, BART_TESTS$reg_feature_index_1] +
    BART_TESTS$reg_coef_2 * X[, BART_TESTS$reg_feature_index_2] +
    rnorm(n, sd = BART_TESTS$reg_noise_sd)
  set_bart_machine_num_cores(BART_TESTS$core_count, verbose = BART_TESTS$verbose)
  model <- bartMachine(
    X,
    y,
    num_trees = get_test_env_int("MODEL_NUM_TREES", BART_TESTS$model_num_trees),
    num_burn_in = get_test_env_int("MODEL_BURN_IN", BART_TESTS$model_burn_in),
    num_iterations_after_burn_in = get_test_env_int("MODEL_ITER", BART_TESTS$model_iter),
    flush_indices_to_save_RAM = FALSE,
    verbose = BART_TESTS$verbose
  )
  set_bart_machine_num_cores(1, verbose = BART_TESTS$verbose)
  model
}

build_classification_model <- function() {
  set.seed(BART_TESTS$seed)
  n <- as.integer(Sys.getenv("N_CLASS_TEST", Sys.getenv("N_TEST", as.character(BART_TESTS$default_n))))
  p <- as.integer(Sys.getenv("P_CLASS_TEST", Sys.getenv("P_TEST", as.character(BART_TESTS$default_class_p))))
  X <- data.frame(matrix(runif(n * p), ncol = p))
  y <- factor(ifelse(
    X[, BART_TESTS$class_feature_index] > BART_TESTS$class_threshold,
    "yes",
    "no"
  ))
  set_bart_machine_num_cores(BART_TESTS$core_count, verbose = BART_TESTS$verbose)
  bartMachine(
    X,
    y,
    num_trees = get_test_env_int("MODEL_NUM_TREES", BART_TESTS$model_num_trees),
    num_burn_in = get_test_env_int("MODEL_BURN_IN", BART_TESTS$model_burn_in),
    num_iterations_after_burn_in = get_test_env_int("MODEL_ITER", BART_TESTS$model_iter),
    flush_indices_to_save_RAM = FALSE,
    verbose = BART_TESTS$verbose
  )
}
