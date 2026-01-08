
test_that("summary.bartMachine produces output", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  model <- bartMachine(X, y, 
                       num_trees = 10, 
                       num_burn_in = 5, 
                       num_iterations_after_burn_in = 5, 
                       verbose = FALSE)
  
  # Capture output to ensure it prints something and doesn't crash
  out <- capture.output(summary(model))
  expect_true(length(out) > 0)
  expect_true(any(grepl("bartMachine", out, ignore.case = TRUE)))
})

test_that("print.bartMachine produces output", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  model <- bartMachine(X, y, 
                       num_trees = 10, 
                       num_burn_in = 5, 
                       num_iterations_after_burn_in = 5, 
                       verbose = FALSE)
  
  out <- capture.output(print(model))
  expect_true(length(out) > 0)
  expect_true(any(grepl("bartMachine", out, ignore.case = TRUE)))
})
