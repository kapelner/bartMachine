test_that("bartMachine serialization works when serialize = TRUE", {
  skip_on_cran() # Serialization tests can be slow or problematic on CRAN
  
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  # Build model with serialization enabled
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    serialize = TRUE,
    verbose = FALSE
  )
  
  # Save to a temporary file
  tmp_path <- tempfile(fileext = ".rds")
  saveRDS(model, tmp_path)
  
  # Load it back
  model_loaded <- readRDS(tmp_path)
  
  # Check if the loaded model can predict
  # This verifies that the Java object was correctly restored
  pred <- predict(model_loaded, X, verbose = FALSE)
  expect_length(pred, nrow(X))
  expect_false(is.null(model_loaded$java_bart_machine))
  
  unlink(tmp_path)
})

test_that("bartMachine serialization fails/warns when serialize = FALSE", {
  skip_on_cran()
  
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  # Build model with serialization disabled (default)
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    serialize = FALSE,
    verbose = FALSE
  )
  
  # Save to a temporary file
  tmp_path <- tempfile(fileext = ".rds")
  saveRDS(model, tmp_path)
  
  # Load it back
  model_loaded <- readRDS(tmp_path)
  
  # The Java object pointer should be dead or null, or prediction should fail
  # Note: rJava objects that are not serialized become null or invalid pointers upon reload
  # attempting to use them usually throws an error.
  
  expect_error(predict(model_loaded, X, verbose = FALSE))
  
  unlink(tmp_path)
})
