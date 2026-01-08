
test_that("bartMachine validates integer arguments via checkmate", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
          # num_trees
  
          expect_error(bartMachine(X, y, num_trees = 0, verbose = FALSE))
  
      
  
  
  expect_error(bartMachine(X, y, num_trees = 1.5, verbose = FALSE))
  
  # num_burn_in
  expect_error(bartMachine(X, y, num_burn_in = -1, verbose = FALSE))
  expect_error(bartMachine(X, y, num_burn_in = 10.5, verbose = FALSE))
  
  # num_iterations_after_burn_in
  expect_error(bartMachine(X, y, num_iterations_after_burn_in = 0, verbose = FALSE))
})

test_that("bartMachine validates numeric hyperparameter arguments via checkmate", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  # alpha [0, 1]
  expect_error(bartMachine(X, y, alpha = -0.1, verbose = FALSE))
  expect_error(bartMachine(X, y, alpha = 1.1, verbose = FALSE))
  
  # beta >= 0
  expect_error(bartMachine(X, y, beta = -1, verbose = FALSE))
  
  # k >= 0
  expect_error(bartMachine(X, y, k = -2, verbose = FALSE))
  
  # q [0, 1]
  expect_error(bartMachine(X, y, q = 1.5, verbose = FALSE))
  
  # nu >= 0
  expect_error(bartMachine(X, y, nu = -1, verbose = FALSE))
  
  # prob_rule_class [0, 1]
  expect_error(bartMachine(X, y, prob_rule_class = 2, verbose = FALSE))
})

test_that("bartMachine validates vector arguments via checkmate", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  # mh_prob_steps: length 3
  expect_error(bartMachine(X, y, mh_prob_steps = c(0.5, 0.5), verbose = FALSE))
  expect_error(bartMachine(X, y, mh_prob_steps = c(-1, 0.5, 0.5), verbose = FALSE))
})

test_that("bartMachine validates boolean flags via checkmate", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  expect_error(bartMachine(X, y, debug_log = "yes", verbose = FALSE))
  expect_error(bartMachine(X, y, verbose = NULL))
  expect_error(bartMachine(X, y, use_missing_data = 1, verbose = FALSE))
})

test_that("bartMachineCV validates arguments via checkmate", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  expect_error(bartMachineCV(X, y, num_tree_cvs = c(10, -5), verbose = FALSE))
  expect_error(bartMachineCV(X, y, k_cvs = c(-1), verbose = FALSE))
  expect_error(bartMachineCV(X, y, k_folds = 1, verbose = FALSE))
  expect_error(bartMachineCV(X, y, k_folds = 0, verbose = FALSE))
})

test_that("predict.bartMachine validates arguments via checkmate", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  model <- bartMachine(X, y, num_trees = 5, num_burn_in = 2, num_iterations_after_burn_in = 2, verbose = FALSE)
  
  expect_error(predict(model, new_data = as.matrix(X), verbose = FALSE))
  expect_error(predict(model, X, type = "foo", verbose = FALSE))
  expect_error(predict(model, X, prob_rule_class = 1.5, verbose = FALSE))
})
