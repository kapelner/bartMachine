source("../testthat/helper-bartmachine-models.R")


test_that("node prediction and projection helpers return expected shapes", {
  reg_model <- build_regression_model()
  idx <- node_prediction_training_data_indices(reg_model)
  expect_true(is.array(idx))
  expect_equal(dim(idx)[BART_TESTS$first_dim_index], reg_model$n)

  weights <- get_projection_weights(reg_model)
  expect_equal(nrow(weights), reg_model$n)
  expect_equal(ncol(weights), reg_model$n)

  weights_kludge <- get_projection_weights(reg_model, regression_kludge = TRUE)
  expect_equal(dim(weights_kludge), dim(weights))
})

test_that("posterior and interval helpers return expected structures", {
  reg_model <- build_regression_model()
  idx <- seq_len(BART_TESTS$posterior_subset_n)
  posterior <- bart_machine_get_posterior(reg_model, reg_model$X[idx, , drop = FALSE], verbose = BART_TESTS$verbose)
  expect_true(is.list(posterior))
  expect_true("y_hat" %in% names(posterior))

  cred <- calc_credible_intervals(reg_model, reg_model$X[idx, , drop = FALSE])
  expect_equal(nrow(cred), BART_TESTS$posterior_subset_n)
  expect_equal(ncol(cred), BART_TESTS$credible_interval_cols)

  pred <- calc_prediction_intervals(
    reg_model,
    reg_model$X[idx, , drop = FALSE],
    num_samples_per_data_point = BART_TESTS$prediction_samples_per_point
  )
  expect_true(is.list(pred))
  expect_equal(nrow(pred$interval), BART_TESTS$posterior_subset_n)
})

test_that("covariate tests and linearity test return p-values", {
  reg_model <- build_regression_model()
  cov_test <- cov_importance_test(
    reg_model,
    covariates = BART_TESTS$covariate_index,
    num_permutation_samples = BART_TESTS$num_permute_samples,
    plot = FALSE,
    verbose = BART_TESTS$verbose
  )
  expect_true(is.list(cov_test))
  expect_true("pval" %in% names(cov_test))

  lin_test <- linearity_test(
    X = reg_model$X,
    y = reg_model$y,
    num_permutation_samples = BART_TESTS$num_permute_samples,
    plot = FALSE,
    num_trees = BART_TESTS$linearity_num_trees,
    num_burn_in = BART_TESTS$linearity_num_burn_in,
    num_iterations_after_burn_in = BART_TESTS$linearity_num_iter,
    verbose = BART_TESTS$verbose
  )
  expect_true(is.list(lin_test))
  expect_true("pval" %in% names(lin_test))
})

test_that("plotting diagnostics return ggplot objects or lists", {
  reg_model <- build_regression_model()
  with_plot_device({
    plots <- check_bart_error_assumptions(reg_model, verbose = BART_TESTS$verbose)
    expect_true(inherits(plots$qq_plot, "ggplot"))
    expect_true(inherits(plots$hetero_plot, "ggplot"))
  })

  with_plot_device({
    plot_obj <- plot_y_vs_yhat(reg_model, verbose = BART_TESTS$verbose)
    expect_true(inherits(plot_obj, "ggplot"))
  })

  with_plot_device({
    plot_obj <- plot_y_vs_yhat(reg_model, credible_intervals = TRUE, verbose = BART_TESTS$verbose)
    expect_true(inherits(plot_obj, "ggplot"))
  })

  with_plot_device({
    sigsqs <- get_sigsqs(reg_model, plot_hist = TRUE, verbose = BART_TESTS$verbose)
    expect_true(inherits(attr(sigsqs, "plot"), "ggplot"))
  })

  with_plot_device({
    var_imp <- investigate_var_importance(
      reg_model,
      plot = TRUE,
      num_replicates_for_avg = BART_TESTS$num_replicates_for_avg,
      num_trees_bottleneck = BART_TESTS$num_trees_bottleneck,
      num_var_plot = BART_TESTS$num_var_plot,
      verbose = BART_TESTS$verbose
    )
    expect_true(inherits(var_imp$plot, "ggplot"))
  })

  with_plot_device({
    plots <- plot_convergence_diagnostics(reg_model, plots = c("sigsqs", "mh_acceptance"), verbose = BART_TESTS$verbose)
    expect_true(length(plots) >= BART_TESTS$min_plot_count)
  })

  with_plot_device({
    inter <- interaction_investigator(
      reg_model,
      plot = TRUE,
      num_replicates_for_avg = BART_TESTS$num_replicates_for_avg,
      num_trees_bottleneck = BART_TESTS$num_trees_bottleneck,
      num_var_plot = BART_TESTS$num_var_plot,
      verbose = BART_TESTS$verbose
    )
    expect_true(inherits(inter$plot, "ggplot"))
  })

  with_plot_device({
    pd <- pd_plot(
      reg_model,
      j = BART_TESTS$covariate_index,
      prop_data = BART_TESTS$pd_plot_prop_data,
      verbose = BART_TESTS$verbose
    )
    expect_true(inherits(pd$plot, "ggplot"))
  })

  with_plot_device({
    rmses <- rmse_by_num_trees(
      reg_model,
      tree_list = BART_TESTS$rmse_tree_list,
      in_sample = TRUE,
      plot = TRUE,
      num_replicates = BART_TESTS$rmse_num_replicates,
      verbose = BART_TESTS$verbose
    )
    expect_true(is.numeric(rmses))
    plot_obj <- attr(rmses, "plot")
    if (!is.null(plot_obj)) {
      expect_true(inherits(plot_obj, "ggplot"))
    }
  })
})
