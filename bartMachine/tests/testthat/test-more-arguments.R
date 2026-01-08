test_that("bartMachine runs with Xy argument", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  Xy <- cbind(X, y)
  colnames(Xy)[3] <- "y"
  
  model <- bartMachine(
    Xy = Xy,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
  expect_equal(model$y, y)
})

test_that("bartMachine runs with prob_rule_class", {
  set.seed(BART_TESTS$seed)
  n <- BART_TESTS$small_data_n
  p <- 2
  X <- data.frame(matrix(runif(n * p), ncol = p))
  y <- factor(ifelse(X[, 1] > 0.5, "1", "0"))
  
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    prob_rule_class = 0.3,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
  # Note: The prob_rule_class is used during prediction, but stored in the model?
  # Let's check if it's stored. Based on documentation it might not be directly exposed 
  # in the list but used in internal Java object or classification logic.
  # However, it is an argument, so we test it doesn't crash.
})

test_that("bartMachine runs with run_in_sample = FALSE", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    run_in_sample = FALSE,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
  expect_null(model$y_hat_train)
  expect_null(model$residuals)
  expect_null(model$L1_err_train)
  expect_null(model$L2_err_train)
  expect_null(model$PseudoRsq)
  expect_null(model$rmse_train)
})

test_that("bartMachine runs with debug_log = TRUE", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  # It writes to a file, so we just check it runs without error
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    debug_log = TRUE,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
})

test_that("bartMachine runs with seed argument", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  # We should get identical results if we pass the same seed
  set.seed(BART_TESTS$seed)
  model1 <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    seed = BART_TESTS$seed,
    verbose = FALSE
  )
  
  set.seed(BART_TESTS$seed)
  model2 <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    seed = BART_TESTS$seed,
    verbose = FALSE
  )
  
  expect_equal(model1$y_hat_train, model2$y_hat_train)
})

test_that("bartMachine runs with replace_missing_data_with_x_j_bar", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  X[1:5, 1] <- NA
  y <- X[, 2] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    replace_missing_data_with_x_j_bar = TRUE,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
})

test_that("bartMachine runs with impute_missingness_with_rf_impute", {
  skip_if_not_installed("randomForest")
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  X[1:5, 1] <- NA
  y <- X[, 2] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    impute_missingness_with_rf_impute = TRUE,
    replace_missing_data_with_x_j_bar = TRUE,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
})

test_that("bartMachine runs with impute_missingness_with_x_j_bar_for_lm", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  X[1:5, 1] <- NA
  y <- X[, 2] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    use_missing_data = TRUE,
    impute_missingness_with_x_j_bar_for_lm = TRUE,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
})

test_that("bartMachine runs with print_tree_illustrations", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  # Capturing output because it prints to stdout
  capture.output({
    model <- bartMachine(
      X, y,
      num_trees = 2, # Small number to reduce output
      num_burn_in = 2,
      num_iterations_after_burn_in = 2,
      print_tree_illustrations = TRUE,
      verbose = FALSE
    )
  })
  expect_s3_class(model, "bartMachine")
})
