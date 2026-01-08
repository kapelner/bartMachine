test_that("bartMachine runs with custom tree hyperparameters", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)

  # Test alpha, beta, k
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    alpha = 0.5,
    beta = 3,
    k = 3,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
})

test_that("bartMachine runs with custom error variance prior", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)

  # Test q, nu, s_sq_y
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    q = 0.95,
    nu = 5.0,
    s_sq_y = "var",
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")

  # Test custom sig_sq_est
  model_custom_sig <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    sig_sq_est = 0.01,
    verbose = FALSE
  )
  expect_s3_class(model_custom_sig, "bartMachine")
  expect_equal(model_custom_sig$sig_sq_est, 0.01)
})

test_that("bartMachine runs with custom covariate priors", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 3), ncol = 3))
  y <- X[, 1] + X[, 2] + rnorm(BART_TESTS$small_data_n, sd = 0.1)

  # cov_prior_vec
  prior <- c(0.1, 0.8, 0.1) # Sums to 1
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    cov_prior_vec = prior,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
})

test_that("bartMachine runs with different MH steps", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)

  # mh_prob_steps: GROW, PRUNE, CHANGE
  steps <- c(1/3, 1/3, 1/3)
  model <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    mh_prob_steps = steps,
    verbose = FALSE
  )
  expect_s3_class(model, "bartMachine")
})

test_that("bartMachine runs with memory options", {
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)

  # mem_cache_for_speed = FALSE
  model_no_cache <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    mem_cache_for_speed = FALSE,
    verbose = FALSE
  )
  expect_s3_class(model_no_cache, "bartMachine")

  # flush_indices_to_save_RAM = TRUE
  model_flush <- bartMachine(
    X, y,
    num_trees = 10,
    num_burn_in = 5,
    num_iterations_after_burn_in = 5,
    flush_indices_to_save_RAM = TRUE,
    verbose = FALSE
  )
  expect_s3_class(model_flush, "bartMachine")
})

test_that("bartMachine runs with custom random sample library size", {
    set.seed(BART_TESTS$seed)
    X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
    y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
    
    model <- bartMachine(
        X, y,
        num_trees = 10,
        num_burn_in = 5,
        num_iterations_after_burn_in = 5,
        num_rand_samps_in_library = 1000,
        verbose = FALSE
    )
    expect_s3_class(model, "bartMachine")
})
