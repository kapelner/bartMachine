source("../testthat/helper-bartmachine-models.R")


test_that("variable selection helpers return expected structures", {
  reg_model <- build_regression_model()
  var_sel <- var_selection_by_permute(
    reg_model,
    num_reps_for_avg = BART_TESTS$var_sel_num_reps_for_avg,
    num_permute_samples = BART_TESTS$var_sel_num_permute_samples,
    num_trees_for_permute = BART_TESTS$var_sel_num_trees_for_permute,
    plot = FALSE,
    verbose = BART_TESTS$verbose
  )
  expect_true(is.list(var_sel))
  expect_true("permute_mat" %in% names(var_sel))

  var_sel_cv <- var_selection_by_permute_cv(
    reg_model,
    k_folds = BART_TESTS$var_sel_k_folds,
    num_reps_for_avg = BART_TESTS$var_sel_num_reps_for_avg,
    num_permute_samples = BART_TESTS$var_sel_num_permute_samples,
    num_trees_for_permute = BART_TESTS$var_sel_num_trees_for_permute,
    num_trees_pred_cv = BART_TESTS$var_sel_num_trees_pred_cv,
    verbose = BART_TESTS$verbose
  )
  expect_true(is.list(var_sel_cv))
  expect_true("best_method" %in% names(var_sel_cv))
})

test_that("cross-validation helpers run with small grids", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(
    x1 = runif(BART_TESTS$cv_data_n),
    x2 = runif(BART_TESTS$cv_data_n)
  )
  y <- X$x1 + rnorm(BART_TESTS$cv_data_n, sd = BART_TESTS$cv_data_sd)

  cv_model <- bartMachineCV(
    X,
    y,
    num_tree_cvs = BART_TESTS$cv_num_tree_cvs,
    k_cvs = BART_TESTS$cv_k_cvs,
    k_folds = BART_TESTS$cv_k_folds,
    num_burn_in = BART_TESTS$cv_num_burn_in,
    num_iterations_after_burn_in = BART_TESTS$cv_num_iter,
    verbose = BART_TESTS$verbose
  )
  expect_s3_class(cv_model, "bartMachine")

  cv_model2 <- build_bart_machine_cv(
    X,
    y,
    num_tree_cvs = BART_TESTS$cv_num_tree_cvs_single,
    k_cvs = BART_TESTS$cv_k_cvs,
    k_folds = BART_TESTS$cv_k_folds,
    num_burn_in = BART_TESTS$cv_num_burn_in,
    num_iterations_after_burn_in = BART_TESTS$cv_num_iter,
    verbose = BART_TESTS$verbose
  )
  expect_s3_class(cv_model2, "bartMachine")

  kfold <- k_fold_cv(
    X,
    y,
    k_folds = BART_TESTS$cv_k_folds,
    num_trees = BART_TESTS$cv_num_trees,
    num_burn_in = BART_TESTS$cv_num_burn_in,
    num_iterations_after_burn_in = BART_TESTS$cv_num_iter,
    verbose = BART_TESTS$verbose
  )
  expect_true(is.list(kfold))
  expect_true("rmse" %in% names(kfold))
})

test_that("node/raw data extraction works", {
  reg_model <- build_regression_model()
  raw_nodes <- extract_raw_node_data(reg_model, g = BART_TESTS$raw_node_g)
  expect_true(is.list(raw_nodes))
})

test_that("training/test prediction helper works", {
  reg_model <- build_regression_model()
  idx <- seq_len(BART_TESTS$test_idx_n)
  Xtest <- reg_model$X[idx, , drop = FALSE]
  ytest <- reg_model$y[idx]
  oos <- bart_predict_for_test_data(reg_model, Xtest, ytest, verbose = BART_TESTS$verbose)
  expect_true(is.list(oos))
  expect_true("rmse" %in% names(oos))
})
