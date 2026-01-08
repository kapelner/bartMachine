test_that("bartMachine handles missing data", {
  # Generate data with missing values
  set.seed(BART_TESTS$seed)
  n <- BART_TESTS$small_data_n
  p <- BART_TESTS$default_p
  X <- data.frame(matrix(runif(n * p), ncol = p))
  
  # Introduce missingness
  X[1:5, 1] <- NA
  X[6:10, 2] <- NA
  
  y <- BART_TESTS$reg_coef_1 * ifelse(is.na(X[, 1]), 0.5, X[, 1]) +
       BART_TESTS$reg_coef_2 * ifelse(is.na(X[, 2]), 0.5, X[, 2]) +
       rnorm(n, sd = BART_TESTS$small_data_sd)
  
  # Test with use_missing_data = TRUE
  model_missing <- bartMachine(
    X, 
    y,
    num_trees = BART_TESTS$small_model_num_trees,
    num_burn_in = BART_TESTS$small_model_burn_in,
    num_iterations_after_burn_in = BART_TESTS$small_model_iter,
    use_missing_data = TRUE,
    verbose = BART_TESTS$verbose
  )
  expect_s3_class(model_missing, "bartMachine")
  expect_true(model_missing$use_missing_data)
  
  # Test with use_missing_data_dummies_as_covars = TRUE
  model_dummies <- bartMachine(
    X, 
    y,
    num_trees = BART_TESTS$small_model_num_trees,
    num_burn_in = BART_TESTS$small_model_burn_in,
    num_iterations_after_burn_in = BART_TESTS$small_model_iter,
    use_missing_data = TRUE,
    use_missing_data_dummies_as_covars = TRUE,
    verbose = BART_TESTS$verbose
  )
  expect_s3_class(model_dummies, "bartMachine")
  # Check if dummy columns were created (starts with M_)
  expect_true(any(grepl("^M_", model_dummies$training_data_features_with_missing_features)))
})

test_that("variable counts and proportions extraction works", {
  reg_model <- build_regression_model()
  
  # Check get_var_counts_over_chain
  counts_splits <- get_var_counts_over_chain(reg_model, type = "splits")
  expect_true(is.matrix(counts_splits))
  expect_equal(ncol(counts_splits), reg_model$p)
  expect_equal(nrow(counts_splits), reg_model$num_iterations_after_burn_in)
  
  counts_trees <- get_var_counts_over_chain(reg_model, type = "trees")
  expect_true(is.matrix(counts_trees))
  
  # Check get_var_props_over_chain
  props_splits <- get_var_props_over_chain(reg_model, type = "splits")
  expect_true(is.numeric(props_splits))
  expect_equal(length(props_splits), reg_model$p)
  expect_true(all(props_splits >= 0 & props_splits <= 1))
  
  props_trees <- get_var_props_over_chain(reg_model, type = "trees")
  expect_true(is.numeric(props_trees))
})

test_that("serialization works (save and load)", {
  # Build a model with serialize = TRUE
  set.seed(BART_TESTS$seed)
  X <- data.frame(matrix(runif(BART_TESTS$small_data_n * 2), ncol = 2))
  y <- X[, 1] + rnorm(BART_TESTS$small_data_n, sd = 0.1)
  
  model <- bartMachine(
    X, 
    y, 
    num_trees = BART_TESTS$small_model_num_trees,
    num_burn_in = BART_TESTS$small_model_burn_in,
    num_iterations_after_burn_in = BART_TESTS$small_model_iter,
    serialize = TRUE,
    verbose = BART_TESTS$verbose
  )
  
  # Save to temp file
  tmp_file <- tempfile(fileext = ".rds")
  saveRDS(model, tmp_file)
  
  # Load back
  loaded_model <- readRDS(tmp_file)
  expect_s3_class(loaded_model, "bartMachine")
  
  # Check if prediction works on loaded model
  preds <- predict(loaded_model, X, verbose = BART_TESTS$verbose)
  expect_length(preds, nrow(X))
  
  # Clean up
  unlink(tmp_file)
})

test_that("interaction constraints work", {
    set.seed(BART_TESTS$seed)
    n = 100
    p = 5
    X = data.frame(matrix(runif(n * p), ncol = p))
    y = 10 * X[ ,1] * X[,2] + X[,1] * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
    
    #constrain so that x1 and x2 interact and x1 and x3 interact and x4 and x5 interact
    interaction_constraints = list(c(1, 2, 3), c(4, 5))
    
    #build BART regression model
    bart_machine = bartMachine(X, y, num_trees = 20, num_burn_in = 100, num_iterations_after_burn_in = 100, interaction_constraints = interaction_constraints, verbose = FALSE)
    expect_s3_class(bart_machine, "bartMachine")
    
    #check invalid constraints throw error
    interaction_constraints_invalid = list(c(1, 2, 3), c(4, 6)) #6 is out of bounds
    expect_error(bartMachine(X, y, num_trees = 20, interaction_constraints = interaction_constraints_invalid, verbose = FALSE))
})
