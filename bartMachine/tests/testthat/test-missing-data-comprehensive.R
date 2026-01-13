test_that("bartMachine handles missing data in both training and prediction", {
  set.seed(BART_TESTS$seed)
  n <- 100
  p <- 5
  X <- data.frame(matrix(runif(n * p), ncol = p))
  
  # Introduce missingness in training data
  X[1:10, 1] <- NA
  X[11:20, 2] <- NA
  
  y <- 5 * ifelse(is.na(X[, 1]), 0.5, X[, 1]) + 2 * X[, 3] + rnorm(n, sd = 0.1)
  
  # Train model with missing data support
  model <- bartMachine(
    X, 
    y,
    num_trees = 10,
    num_burn_in = 50,
    num_iterations_after_burn_in = 50,
    use_missing_data = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(model, "bartMachine")
  
  # Create prediction data with missing values
  X_pred <- data.frame(matrix(runif(20 * p), ncol = p))
  colnames(X_pred) <- colnames(X)
  X_pred[1:5, 1] <- NA
  X_pred[6:10, 3] <- NA
  
  # Predict on data with missing values
  preds <- predict(model, X_pred)
  expect_length(preds, 20)
  expect_false(any(is.na(preds)))
})

test_that("bartMachine handles missing data with mean imputation", {
  set.seed(BART_TESTS$seed)
  n <- 100
  X <- data.frame(matrix(runif(n * 3), ncol = 3))
  X[1:10, 1] <- NA
  y <- 3 * X[, 2] + rnorm(n, sd = 0.1)
  
  model <- bartMachine(
    X, 
    y,
    num_trees = 5,
    num_burn_in = 20,
    num_iterations_after_burn_in = 20,
    replace_missing_data_with_x_j_bar = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(model, "bartMachine")
  
  X_pred <- data.frame(matrix(runif(10 * 3), ncol = 3))
  colnames(X_pred) <- colnames(X)
  X_pred[1, 1] <- NA
  
  preds <- predict(model, X_pred)
  expect_length(preds, 10)
})

test_that("bartMachine warns on missing data in prediction when feature is off", {
  set.seed(BART_TESTS$seed)
  n <- 50
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- 2 * X$x1 + rnorm(n, sd = 0.1)
  
  # Train model WITHOUT missing data support
  model <- bartMachine(
    X, 
    y,
    num_trees = 5,
    num_burn_in = 10,
    num_iterations_after_burn_in = 10,
    use_missing_data = FALSE,
    verbose = FALSE
  )
  
  X_pred <- data.frame(x1 = runif(5), x2 = runif(5))
  X_pred[1, 1] <- NA
  
  # Should issue a warning but still predict
  expect_warning(predict(model, X_pred), "rows omitted due to missing data")
})

test_that("bartMachine handles missing data in classification", {
  set.seed(BART_TESTS$seed)
  n <- 100
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  X[1:10, 1] <- NA
  y <- factor(ifelse(ifelse(is.na(X$x1), 0.5, X$x1) + X$x2 > 1, "yes", "no"))
  
  model <- bartMachine(
    X, 
    y,
    num_trees = 5,
    num_burn_in = 20,
    num_iterations_after_burn_in = 20,
    use_missing_data = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(model, "bartMachine")
  
  X_pred <- data.frame(x1 = c(NA, 0.1, 0.9), x2 = c(0.5, 0.5, 0.5))
  preds <- predict(model, X_pred, type = "prob")
  expect_length(preds, 3)
  expect_false(any(is.na(preds)))
})
