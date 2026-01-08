source("../testthat/helper-bartmachine-models.R")

test_that("bartMachine builds regression and classification models", {
  reg_model <- build_regression_model()
  expect_s3_class(reg_model, "bartMachine")
  expect_equal(reg_model$pred_type, "regression")

  class_model <- build_classification_model()
  expect_s3_class(class_model, "bartMachine")
  expect_equal(class_model$pred_type, "classification")
})

test_that("build_bart_machine handles X/y and Xy inputs", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(
    x1 = runif(BART_TESTS$small_data_n),
    x2 = runif(BART_TESTS$small_data_n)
  )
  y <- X$x1 + rnorm(BART_TESTS$small_data_n, sd = BART_TESTS$small_data_sd)
  model_xy <- build_bart_machine(X, y,
    num_trees = BART_TESTS$small_model_num_trees,
    num_burn_in = BART_TESTS$small_model_burn_in,
    num_iterations_after_burn_in = BART_TESTS$small_model_iter,
    flush_indices_to_save_RAM = FALSE,
    verbose = BART_TESTS$verbose
  )
  expect_s3_class(model_xy, "bartMachine")

  Xy <- data.frame(X, y = y)
  model_Xy <- build_bart_machine(Xy = Xy,
    num_trees = BART_TESTS$small_model_num_trees,
    num_burn_in = BART_TESTS$small_model_burn_in,
    num_iterations_after_burn_in = BART_TESTS$small_model_iter,
    flush_indices_to_save_RAM = FALSE,
    verbose = BART_TESTS$verbose
  )
  expect_s3_class(model_Xy, "bartMachine")
})

test_that("core helpers set and read cores", {
  set_bart_machine_num_cores(BART_TESTS$core_count, verbose = BART_TESTS$verbose)
  expect_equal(bart_machine_num_cores(), BART_TESTS$core_count)
  set_bart_machine_num_cores(1, verbose = BART_TESTS$verbose)
})

test_that("dummify_data converts factors", {
  df <- data.frame(a = factor(c("a", "b")), b = BART_TESTS$dummify_b_values)
  dummified <- dummify_data(df)
  expect_true(is.data.frame(dummified))
  expect_true(any(grepl("^a_", names(dummified))))
})

test_that("bartMachineArr and predict_bartMachineArr work", {
  reg_model <- build_regression_model()
  arr <- bartMachineArr(reg_model, R = BART_TESTS$arr_r)
  expect_true(is.list(arr))
  preds <- predict_bartMachineArr(arr, reg_model$X)
  expect_length(preds, nrow(reg_model$X))
})

test_that("predict methods return expected shapes", {
  reg_model <- build_regression_model()
  preds <- predict(reg_model, reg_model$X, verbose = BART_TESTS$verbose)
  expect_length(preds, nrow(reg_model$X))

  class_model <- build_classification_model()
  probs <- predict(class_model, class_model$X, type = "prob", verbose = BART_TESTS$verbose)
  expect_length(probs, nrow(class_model$X))
  classes <- predict(class_model, class_model$X, type = "class", verbose = BART_TESTS$verbose)
  expect_length(classes, nrow(class_model$X))
})
