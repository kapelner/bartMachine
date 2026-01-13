
##check BART error assumptions via plot
#' Check BART Error Assumptions
#'
#' @description
#' Diagnostic tools to assess whether the errors of the BART model for regression are normally distributed and homoskedastic, as assumed by the model. This function generates a normal quantile plot of the residuals with a Shapiro-Wilks p-value as well as a residual plot.
#' @param bart_machine An object of class ``bartMachine''.
#' @param hetero_plot If ``yhats'', the residuals are plotted against the fitted values of the response. If ``ys'', the residuals are plotted against the actual values of the response.
#' @param verbose If TRUE, prints plots to the active device.
#'
#' @return
#' None.
#'
#' @seealso
#' \code{\link{plot_convergence_diagnostics}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @examples
#' \dontrun{
#' #generate Friedman data
#' set.seed(11)
#' n  = 300
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' 
#' #check error diagnostics
#' check_bart_error_assumptions(bart_machine)
#' }
#' @export
check_bart_error_assumptions = function(bart_machine, hetero_plot = "yhats", verbose = TRUE){
  assert_class(bart_machine, "bartMachine")
  assert_choice(hetero_plot, c("yhats", "ys"))
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (!(hetero_plot %in% c("ys", "yhats"))){
		stop("You must specify the parameter \"hetero_plot\" as \"ys\" or \"yhats\"")
	}
	if (bart_machine$pred_type == "classification"){
		stop("There are no convergence diagnostics for classification.")
	}	
	es = bart_machine$residuals
	y_hat = bart_machine$y_hat
	
	#plot/test for normality
	if (length(es) > 5000){
		normal_p_val = shapiro.test(sample(es, 5000))$p.value
	} else {
		normal_p_val = shapiro.test(es)$p.value
	}

	n = length(es)
	qq_df = data.frame(
		theoretical = qnorm(ppoints(n)),
		sample = sort(es)
	)
	qq_plot = ggplot2::ggplot(qq_df, ggplot2::aes(x = theoretical, y = sample)) +
		ggplot2::geom_point(color = "blue") +
		ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
		ggplot2::labs(
			title = paste("Assessment of Normality\n", "p-val for shapiro-wilk test of normality of residuals:", round(normal_p_val, 3)),
			x = "Theoretical Quantiles",
			y = "Sample Quantiles"
		)
	
	if (hetero_plot == "yhats"){
		hetero_df = data.frame(x = y_hat, residual = es)
		x_label = "Fitted Values"
	} else {
		hetero_df = data.frame(x = bart_machine$y, residual = es)
		x_label = "Actual Values"
	}
	hetero_plot_obj = ggplot2::ggplot(hetero_df, ggplot2::aes(x = x, y = residual)) +
		ggplot2::geom_point(color = "blue") +
		ggplot2::geom_hline(yintercept = 0, color = "black") +
		ggplot2::labs(
			title = "Assessment of Heteroskedasticity\nFitted vs residuals",
			x = x_label,
			y = "Residuals"
		)
	
	if (verbose){
		print(qq_plot)
		print(hetero_plot_obj)
	}
	invisible(list(qq_plot = qq_plot, hetero_plot = hetero_plot_obj))
}

##private function for plotting tree depths
plot_tree_depths = function(bart_machine){
	
	tree_depths_after_burn_in = get_tree_depths(bart_machine)

	num_after_burn_in = nrow(tree_depths_after_burn_in)
	num_trees = ncol(tree_depths_after_burn_in)
	iter = seq_len(num_after_burn_in)
	tree_df = data.frame(
		iter = rep(iter, times = num_trees),
		tree = factor(rep(seq_len(num_trees), each = num_after_burn_in)),
		depth = as.vector(tree_depths_after_burn_in)
	)
	row_mins = matrixStats::rowMins(tree_depths_after_burn_in)
	row_maxs = matrixStats::rowMaxs(tree_depths_after_burn_in)
	summary_df = data.frame(
		iter = iter,
		mean = rowMeans(tree_depths_after_burn_in),
		min = row_mins,
		max = row_maxs
	)
	
	p = ggplot2::ggplot(tree_df, ggplot2::aes(x = iter, y = depth, group = tree)) +
		ggplot2::geom_line(color = "gray70", alpha = 0.4) +
		ggplot2::geom_line(
			data = summary_df,
			ggplot2::aes(x = iter, y = mean),
			color = "blue",
			linewidth = 1,
			inherit.aes = FALSE
		) +
		ggplot2::geom_line(
			data = summary_df,
			ggplot2::aes(x = iter, y = min),
			color = "black",
			inherit.aes = FALSE
		) +
		ggplot2::geom_line(
			data = summary_df,
			ggplot2::aes(x = iter, y = max),
			color = "black",
			inherit.aes = FALSE
		) +
		ggplot2::labs(
			title = "Tree Depth by MCMC Iteration After Burn-in",
			x = "MCMC Iteration",
			y = "Tree Depth for all cores"
		)
	
	if (bart_machine$num_cores > 1){
		core_lines = (1 : (bart_machine$num_cores - 1)) *
			bart_machine$num_iterations_after_burn_in / bart_machine$num_cores
		p = p + ggplot2::geom_vline(xintercept = core_lines, color = "gray")
	}
	
	p
}

#private function for getting tree depths to plot
get_tree_depths = function(bart_machine){
	tree_depths_list = vector("list", bart_machine$num_cores)
	for (c in 1 : bart_machine$num_cores){
		tree_depths_list[[c]] = 
			.jcall(bart_machine$java_bart_machine, "[[I", "getDepthsForTreesInGibbsSampAfterBurnIn", as.integer(c), simplify = TRUE)
	}
	do.call(rbind, tree_depths_list)
}

#private function for plotting number of nodes in the trees
plot_tree_num_nodes = function(bart_machine){
	
	tree_num_nodes_and_leaves_after_burn_in = get_tree_num_nodes_and_leaves(bart_machine)

	num_after_burn_in = nrow(tree_num_nodes_and_leaves_after_burn_in)
	num_trees = ncol(tree_num_nodes_and_leaves_after_burn_in)
	iter = seq_len(num_after_burn_in)
	tree_df = data.frame(
		iter = rep(iter, times = num_trees),
		tree = factor(rep(seq_len(num_trees), each = num_after_burn_in)),
		num_nodes = as.vector(tree_num_nodes_and_leaves_after_burn_in)
	)
	row_mins = matrixStats::rowMins(tree_num_nodes_and_leaves_after_burn_in)
	row_maxs = matrixStats::rowMaxs(tree_num_nodes_and_leaves_after_burn_in)
	summary_df = data.frame(
		iter = iter,
		mean = rowMeans(tree_num_nodes_and_leaves_after_burn_in),
		min = row_mins,
		max = row_maxs
	)
	
	p = ggplot2::ggplot(tree_df, ggplot2::aes(x = iter, y = num_nodes, group = tree)) +
		ggplot2::geom_line(color = "gray70", alpha = 0.4) +
		ggplot2::geom_line(
			data = summary_df,
			ggplot2::aes(x = iter, y = mean),
			color = "blue",
			linewidth = 1,
			inherit.aes = FALSE
		) +
		ggplot2::geom_line(
			data = summary_df,
			ggplot2::aes(x = iter, y = min),
			color = "black",
			inherit.aes = FALSE
		) +
		ggplot2::geom_line(
			data = summary_df,
			ggplot2::aes(x = iter, y = max),
			color = "black",
			inherit.aes = FALSE
		) +
		ggplot2::labs(
			title = "Tree Num Nodes And Leaves by\nMCMC Iteration After Burn-in",
			x = "MCMC Iteration",
			y = "Tree Num Nodes and Leaves for all cores"
		)
	
	if (bart_machine$num_cores > 1){
		core_lines = (1 : (bart_machine$num_cores - 1)) *
			bart_machine$num_iterations_after_burn_in / bart_machine$num_cores
		p = p + ggplot2::geom_vline(xintercept = core_lines, color = "gray")
	}
	
	p
}
##private function for getting the number of nodes in the trees
get_tree_num_nodes_and_leaves = function(bart_machine){
	tree_num_nodes_and_leaves_list = vector("list", bart_machine$num_cores)
	for (c in 1 : bart_machine$num_cores){
		tree_num_nodes_and_leaves_list[[c]] = 
			.jcall(bart_machine$java_bart_machine, "[[I", "getNumNodesAndLeavesForTreesInGibbsSampAfterBurnIn", as.integer(c), simplify = TRUE)
	}
	do.call(rbind, tree_num_nodes_and_leaves_list)
}

#private function for plotting the MH acceptance proportions by core
plot_mh_acceptance_reject = function(bart_machine){
	
	mh_acceptance_reject = get_mh_acceptance_reject(bart_machine)
	a_r_before_burn_in = mh_acceptance_reject[["a_r_before_burn_in"]]
	a_r_before_burn_in_avg_over_trees = rowSums(a_r_before_burn_in) / bart_machine$num_trees
	
	a_r_after_burn_in_avgs_over_trees = list()
	for (c in 1 : bart_machine$num_cores){
		a_r_after_burn_in = mh_acceptance_reject[["a_r_after_burn_in"]][[c]]
		a_r_after_burn_in_avgs_over_trees[[c]] = rowSums(a_r_after_burn_in) / bart_machine$num_trees		
	}	
	
	num_after_burn_in_per_core = length(a_r_after_burn_in_avgs_over_trees[[1]])
	num_gibbs_per_core = bart_machine$num_burn_in + num_after_burn_in_per_core

	burn_in_df = data.frame(
		iter = 1 : bart_machine$num_burn_in,
		acceptance = a_r_before_burn_in_avg_over_trees
	)
	after_iter = (bart_machine$num_burn_in + 1) : num_gibbs_per_core
	after_df = do.call(
		rbind,
		lapply(1 : bart_machine$num_cores, function(c){
			data.frame(
				iter = after_iter,
				acceptance = a_r_after_burn_in_avgs_over_trees[[c]],
				core = factor(c)
			)
		})
	)
	
	p = ggplot2::ggplot() +
		ggplot2::geom_point(
			data = burn_in_df,
			ggplot2::aes(x = iter, y = acceptance),
			color = "grey"
		) +
		ggplot2::geom_line(
			data = burn_in_df,
			ggplot2::aes(x = iter, y = acceptance),
			color = "black",
			linewidth = 0.8
		) +
		ggplot2::geom_point(
			data = after_df,
			ggplot2::aes(x = iter, y = acceptance, color = core),
			alpha = 0.7
		) +
		ggplot2::geom_line(
			data = after_df,
			ggplot2::aes(x = iter, y = acceptance, color = core),
			linewidth = 0.8,
			alpha = 0.7
		) +
		ggplot2::geom_vline(xintercept = bart_machine$num_burn_in, color = "grey") +
		ggplot2::scale_color_manual(values = COLORS[1 : bart_machine$num_cores]) +
		ggplot2::coord_cartesian(ylim = c(0, 1)) +
		ggplot2::labs(
			title = "Percent Acceptance by MCMC Iteration",
			x = "MCMC Iteration",
			y = "% of Trees Accepting",
			color = "Core"
		)

	smooth_layer <- plot_mh_acceptance_smooth(after_df)
	if (!is.null(smooth_layer)) {
		p <- p + smooth_layer
	}

	p
}

## helper to only smooth cores with enough data
plot_mh_acceptance_smooth <- function(after_df) {
  counts <- table(after_df$core)
  valid_cores <- names(counts[counts >= 4])
  if (length(valid_cores) == 0) {
    return(NULL)
  }
  smooth_df <- after_df[after_df$core %in% valid_cores, , drop = FALSE]
  smooth_layer <- tryCatch(
    suppressMessages(
      ggplot2::geom_smooth(
        data = smooth_df,
        ggplot2::aes(x = iter, y = acceptance, color = core),
        method = "loess",
        formula = y ~ x,
        se = FALSE,
        linewidth = 1
      )
    ),
    error = function(e) NULL
  )
  smooth_layer
}

##private function for getting the MH acceptance proportions by core
get_mh_acceptance_reject = function(bart_machine){
	a_r_before_burn_in = 
		.jcall(bart_machine$java_bart_machine, "[[Z", "getAcceptRejectMHsBurnin", simplify = TRUE) * 1
	
	a_r_after_burn_in = list()
	for (c in 1 : bart_machine$num_cores){
		a_r_after_burn_in[[c]] = 
			.jcall(bart_machine$java_bart_machine, "[[Z", "getAcceptRejectMHsAfterBurnIn", as.integer(c), simplify = TRUE) * 1
	}
	
	list(
			a_r_before_burn_in = a_r_before_burn_in,
			a_r_after_burn_in = a_r_after_burn_in	
	)
}

#plot y vs yhat for training or test data
#' Plot the fitted Versus Actual Response
#'
#' @description
#' Generates a plot actual versus fitted values and corresponding credible intervals or prediction intervals for the fitted values.
#' @param bart_machine An object of class ``bartMachine''.
#' @param Xtest Optional argument for test data. If included, BART computes fitted values at the rows of \code{Xtest}. Else, the fitted values from the training data are used.
#' @param ytest Optional argument for test data. Vector of observed values corresponding to the rows of \code{Xtest} to be plotted against the predictions for the rows of \code{Xtest}.
#' @param credible_intervals If TRUE, Bayesian credible intervals are computed using the quantiles of the posterior distribution of \eqn{\hat{f}(x)}. See \code{\link{calc_credible_intervals}} for details.
#' @param prediction_intervals If TRUE, Bayesian predictive intervals are computed using the a draw of from \eqn{\hat{f}(x)}. See \code{\link{calc_prediction_intervals}} for details.
#' @param interval_confidence_level Desired level of confidence for credible or prediction intervals.
#' @param verbose If TRUE, prints plots to the active device.
#'
#' @return
#' None.
#'
#' @seealso
#' \code{\link{bart_machine_get_posterior}}, \code{\link{calc_credible_intervals}}, \code{\link{calc_prediction_intervals}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
#'
#' @examples
#' \dontrun{
#' #generate linear data
#' set.seed(11)
#' n  = 500
#' p = 3
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 3*X[ ,1] + 2*X[ ,2] +X[ ,3] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' 
#' ##generate plot
#' plot_y_vs_yhat(bart_machine)
#' 
#' #generate plot with prediction bands
#' plot_y_vs_yhat(bart_machine, prediction_intervals = TRUE)
#' }
#' @export
plot_y_vs_yhat = function(bart_machine, Xtest = NULL, ytest = NULL, credible_intervals = FALSE, prediction_intervals = FALSE, interval_confidence_level = 0.95, verbose = TRUE){
  assert_class(bart_machine, "bartMachine")
  assert_data_frame(Xtest, null.ok = TRUE)
  if (!is.null(ytest)) assert_atomic_vector(ytest)
  assert_flag(credible_intervals)
  assert_flag(prediction_intervals)
  assert_number(interval_confidence_level, lower = 0, upper = 1)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if( (!bart_machine$run_in_sample) & (is.null(Xtest) | is.null(ytest)) ){
		stop("To run on training data, you must set \"run_in_sample\" option to TRUE in \"build_bart_machine\"")
	}
  
	if (credible_intervals && prediction_intervals){
		stop("Cannot plot both credibility intervals and prediction intervals simultaneously.")
	}
	if (bart_machine$pred_type == "classification"){
		stop("Cannot plot y vs y_hat for classification.")
	}
  	if( (is.null(Xtest) & !is.null(ytest)) | (!is.null(Xtest) & is.null(ytest)) ){
    	stop("Must pass both X and y to use on test data")
  	}
	
	if (is.null(Xtest) & is.null(ytest)){
		Xtest = bart_machine$X
		ytest = bart_machine$y
		y_hat = bart_machine$y_hat_train
		in_sample = TRUE
	} else {
		predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest, verbose = verbose)
		y_hat = predict_obj$y_hat
		in_sample = FALSE
	}
	
	plot_obj = NULL
	if (credible_intervals){
		credible_intervals = calc_credible_intervals(bart_machine, Xtest, interval_confidence_level)
		ci_a = credible_intervals[, 1]
		ci_b = credible_intervals[, 2]
		y_in_ppi = ytest >= ci_a & ytest <= ci_b
		prop_ys_in_ppi = sum(y_in_ppi) / length(y_in_ppi)
		
		plot_title = paste(
			ifelse(in_sample, "In-Sample", "Out-of-Sample"),
			" Fitted vs. Actual Values\nwith ",
			round(interval_confidence_level * 100),
			"% Cred. Int.'s (",
			round(prop_ys_in_ppi * 100, 2),
			"% coverage)",
			sep = ""
		)
		min_val = min(min(ytest), min(y_hat))
		max_val = max(max(ytest), max(y_hat))
		plot_df = data.frame(
			actual = ytest,
			fitted = y_hat,
			lower = ci_a,
			upper = ci_b,
			in_interval = factor(y_in_ppi, levels = c(TRUE, FALSE))
		)
		p = ggplot2::ggplot(plot_df, ggplot2::aes(x = actual, y = fitted)) +
			ggplot2::geom_segment(
				ggplot2::aes(xend = actual, y = lower, yend = upper),
				color = "gray54",
				linewidth = 0.3
			) +
			ggplot2::geom_point(
				ggplot2::aes(color = in_interval, shape = in_interval),
				size = 1.6
			) +
			ggplot2::scale_color_manual(values = c("TRUE" = "darkblue", "FALSE" = "red")) +
			ggplot2::scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 4)) +
			ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
			ggplot2::coord_cartesian(xlim = c(min_val, max_val), ylim = c(min_val, max_val)) +
			ggplot2::labs(
				title = plot_title,
				x = "Actual Values",
				y = "Fitted Values",
				color = "In Interval",
				shape = "In Interval"
			)
		if (verbose){
			print(p)
		}
		plot_obj = p
	} else if (prediction_intervals){
		prediction_intervals = calc_prediction_intervals(bart_machine, Xtest, interval_confidence_level)
		ci_a = prediction_intervals[, 1]
		ci_b = prediction_intervals[, 2]
		y_in_ppi = ytest >= ci_a & ytest <= ci_b
		prop_ys_in_ppi = sum(y_in_ppi) / length(y_in_ppi)
		
		plot_title = paste(
			ifelse(in_sample, "In-Sample", "Out-of-Sample"),
			" Fitted vs. Actual Values\nwith ",
			round(interval_confidence_level * 100),
			"% Pred. Int.'s (",
			round(prop_ys_in_ppi * 100, 2),
			"% coverage)",
			sep = ""
		)
		min_val = min(min(ytest), min(y_hat))
		max_val = max(max(ytest), max(y_hat))
		plot_df = data.frame(
			actual = ytest,
			fitted = y_hat,
			lower = ci_a,
			upper = ci_b,
			in_interval = factor(y_in_ppi, levels = c(TRUE, FALSE))
		)
		p = ggplot2::ggplot(plot_df, ggplot2::aes(x = actual, y = fitted)) +
			ggplot2::geom_segment(
				ggplot2::aes(xend = actual, y = lower, yend = upper),
				color = "gray54",
				linewidth = 0.3
			) +
			ggplot2::geom_point(
				ggplot2::aes(color = in_interval, shape = in_interval),
				size = 1.6
			) +
			ggplot2::scale_color_manual(values = c("TRUE" = "darkblue", "FALSE" = "red")) +
			ggplot2::scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 4)) +
			ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
			ggplot2::coord_cartesian(xlim = c(min_val, max_val), ylim = c(min_val, max_val)) +
			ggplot2::labs(
				title = plot_title,
				x = "Actual Values",
				y = "Fitted Values",
				color = "In Interval",
				shape = "In Interval"
			)
		if (verbose){
			print(p)
		}
		plot_obj = p
	} else {
		min_val = min(min(ytest), min(y_hat))
		max_val = max(max(ytest), max(y_hat))
		plot_df = data.frame(actual = ytest, fitted = y_hat)
		p = ggplot2::ggplot(plot_df, ggplot2::aes(x = actual, y = fitted)) +
			ggplot2::geom_point(color = "blue") +
			ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
			ggplot2::coord_cartesian(xlim = c(min_val, max_val), ylim = c(min_val, max_val)) +
			ggplot2::labs(
				title = "Fitted vs. Actual Values",
				x = "Actual Values",
				y = "Fitted Values"
			)
		if (verbose){
			print(p)
		}
		plot_obj = p
	}
	invisible(plot_obj)
}

##get sigsqs and plot a histogram, if desired
#' Get Posterior Error Variance Estimates
#'
#' @description
#' Returns the posterior estimates of the error variance from the Gibbs samples with an option to create a histogram of the posterior estimates of the error variance  with a credible interval overlaid.
#' @param bart_machine An object of class ``bartMachine''.
#' @param after_burn_in If TRUE, only the \eqn{\sigma^2} draws after the burn-in period are returned.
#' @param plot_hist If TRUE, a histogram of the posterior \eqn{\sigma^2} draws is generated.
#' @param plot_CI Confidence level for credible interval on histogram.
#' @param plot_sigma If TRUE, plots \eqn{\sigma} instead of \eqn{\sigma^2}.
#' @param verbose If TRUE, prints plots to the active device.
#'
#' @return
#' Returns a vector of posterior \eqn{\sigma^2} draws (with or without the burn-in samples).
#'
#' @seealso
#' \code{\link{get_sigsqs}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @examples
#' \dontrun{
#' #generate Friedman data
#' set.seed(11)
#' n  = 300
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' 
#' #get posterior sigma^2's after burn-in and plot
#' sigsqs = get_sigsqs(bart_machine, plot_hist = TRUE)
#' }
#' @export
get_sigsqs = function(bart_machine, after_burn_in = TRUE, plot_hist = FALSE, plot_CI = .95, plot_sigma = F, verbose = TRUE){
  assert_class(bart_machine, "bartMachine")
  assert_flag(after_burn_in)
  assert_flag(plot_hist)
  assert_number(plot_CI, lower = 0, upper = 1)
  assert_flag(plot_sigma)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (bart_machine$pred_type == "classification"){
		stop("There are no sigsq's for classification.")
	}
	
	sigsqs = .jcall(bart_machine$java_bart_machine, "[D", "getGibbsSamplesSigsqs")
	
	num_iterations_after_burn_in = bart_machine$num_iterations_after_burn_in
	num_burn_in = bart_machine$num_burn_in
	num_gibbs = bart_machine$num_gibbs
	num_trees = bart_machine$num_trees
	
	sigsqs_after_burnin = tail(sigsqs, num_iterations_after_burn_in)
	avg_sigsqs = mean(sigsqs_after_burnin, na.rm = TRUE)
	
	if(plot_hist){
    if(plot_sigma){
      var_est_to_plot = sqrt(sigsqs_after_burnin)
    }
    else{
      var_est_to_plot = sigsqs_after_burnin
    }

	  ppi_a = quantile(var_est_to_plot, (.5 - plot_CI/2))
	  ppi_b = quantile(var_est_to_plot, (.5 + plot_CI/2))
	  plot_df = data.frame(var_est = var_est_to_plot)
	  plot_title = paste("Histogram of ", ifelse(plot_sigma ==T, "Sigmas", "Sigma^2s"),  " After Burn-in", sep = "")
	  plot_xlab = paste(
	  	"Avg = ",
	  	round(mean(var_est_to_plot, na.rm = TRUE), 2),
	  	", ",
	  	100 * plot_CI,
	  	"% Credible Interval = [",
	  	round(ppi_a, 2),
	  	", ",
	  	round(ppi_b, 2),
	  	"]",
	  	sep = ""
	  )
	  p = ggplot2::ggplot(plot_df, ggplot2::aes(x = var_est)) +
	  	ggplot2::geom_histogram(bins = 100, fill = "gray70", color = "white") +
	  	ggplot2::geom_vline(xintercept = mean(var_est_to_plot, na.rm = TRUE), color = "blue") +
	  	ggplot2::geom_vline(xintercept = c(ppi_a, ppi_b), color = "red") +
	  	ggplot2::labs(title = plot_title, x = plot_xlab, y = "Count")
	  if (verbose){
	  	print(p)
	  }
	}
  
	if(after_burn_in == T){
    sigsqs_out = sigsqs_after_burnin
	}else{
    sigsqs_out = sigsqs
	}
	if (plot_hist){
		attr(sigsqs_out, "plot") = p
	}
	return(sigsqs_out)
}

#private function for plotting convergence diagnostics for sigma^2
plot_sigsqs_convergence_diagnostics = function(bart_machine){	
	if (bart_machine$pred_type == "classification"){
		stop("There are no convergence diagnostics for classification.")
	}	
	
	sigsqs = get_sigsqs(bart_machine, after_burn_in = FALSE)
	
	num_iterations_after_burn_in = bart_machine$num_iterations_after_burn_in
	num_burn_in = bart_machine$num_burn_in
	num_gibbs = bart_machine$num_gibbs
	num_trees = bart_machine$num_trees
	
	#first look at sigsqs
	sigsqs_after_burnin = sigsqs[(length(sigsqs) - num_iterations_after_burn_in) : length(sigsqs)]
	avg_sigsqs_after_burn_in = mean(sigsqs_after_burnin, na.rm = TRUE)

	ppi_sigsqs = quantile(sigsqs[num_burn_in : length(sigsqs)], c(.025, .975))
	plot_df = data.frame(
		iter = seq_along(sigsqs),
		sigsq = sigsqs
	)
	y_limits = c(quantile(sigsqs, 0.01), quantile(sigsqs, 0.99))
	
	p = ggplot2::ggplot(plot_df, ggplot2::aes(x = iter, y = sigsq)) +
		ggplot2::geom_point(color = "gray70", alpha = 0.5, size = 0.6) +
		ggplot2::geom_point(color = "red", alpha = 0.4, size = 0.6) +
		ggplot2::geom_hline(yintercept = ppi_sigsqs, color = "darkgreen") +
		ggplot2::geom_hline(yintercept = avg_sigsqs_after_burn_in, color = "blue") +
		ggplot2::geom_vline(xintercept = num_burn_in, color = "gray") +
		ggplot2::coord_cartesian(ylim = y_limits) +
		ggplot2::labs(
			title = "Sigsq Estimates over MCMC Iteration",
			x = "MCMC Iteration (green lines: after burn-in 95% CI)",
			y = paste("Sigsq by MCMC Iteration, avg after burn-in =", round(avg_sigsqs_after_burn_in, 3))
		)
	
	if (bart_machine$num_cores > 1){
		core_lines = num_burn_in + (1 : (bart_machine$num_cores - 1)) *
			bart_machine$num_iterations_after_burn_in / bart_machine$num_cores
		p = p + ggplot2::geom_vline(xintercept = core_lines, color = "gray")
	}
	
	p
}

##function for investigating variable inclusion proportions
#' Explore Variable Inclusion Proportions in BART Model
#'
#' @description
#' Explore the variable inclusion proportions for a BART model to learn about the relative influence of the different covariates. This function includes an option to generate a plot of the variable inclusion proportions.
#'
#' @details
#' In the plot, the red bars correspond to the standard error of the variable inclusion proportion estimates.
#' @param bart_machine An object of class ``bartMachine''.
#' @param type If ``splits'', then the proportion of times each variable is chosen for a splitting rule is computed. If ``trees'', then the proportion of times each variable appears in a tree is computed.
#' @param plot If TRUE, a plot of the variable inclusion proportions is generated.
#' @param num_replicates_for_avg The number of replicates of BART to be used to generate variable inclusion proportions. Averaging across multiple BART models improves stability of the estimates. See Bleich et al. (2013) for more details.
#' @param num_trees_bottleneck Number of trees to be used in the sum-of-trees for computing the variable inclusion proportions. A small number of trees should be used to force the variables to compete for entry into the model. Chipman et al. (2010) recommend 20. See this reference for more details.
#' @param num_var_plot Number of variables to be shown on the plot. If ``Inf'', all variables are plotted.
#' @param bottom_margin A display parameter that adjusts the bottom margin of the graph if labels are clipped. The scale of this parameter is the same as set with \code{par(mar = c(....))} in R.
#'   Higher values allow for more space if the covariate names are long. Note that making this parameter too large will prevent plotting and the plot function in R will throw an error.
#' @param verbose If TRUE, prints progress messages and plots to the active device.
#'
#' @return
#' Invisibly, returns a list with the following components:
#' \item{avg_var_props}{The average variable inclusion proportions for each variable\cr (across \code{num_replicates_for_avg})}
#' \item{sd_var_props}{The standard deviation of the variable inclusion proportions for each variable (across \code{num_replicates_for_avg})}
#'
#' @references
#' Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
#' with Bayesian Additive Regression Trees. Journal of Statistical
#' Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
#' 
#' J Bleich, A Kapelner, ST Jensen, and EI George. Variable Selection Inference for Bayesian
#' Additive Regression Trees. ArXiv e-prints, 2013.
#' 
#' HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive Regressive Trees.
#' The Annals of Applied Statistics, 4(1): 266--298, 2010.
#'
#' @seealso
#' \code{\link{interaction_investigator}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
#'
#' @examples
#' \dontrun{
#' #generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 10
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y, num_trees = 20)
#' 
#' #investigate variable inclusion proportions
#' investigate_var_importance(bart_machine)
#' }
#' @export
investigate_var_importance = function(bart_machine, type = "splits", plot = TRUE, num_replicates_for_avg = 5, num_trees_bottleneck = 20, num_var_plot = Inf, bottom_margin = 10, verbose = TRUE){
  assert_class(bart_machine, "bartMachine")
  assert_choice(type, c("splits", "trees"))
  assert_flag(plot)
  assert_int(num_replicates_for_avg, lower = 1)
  assert_int(num_trees_bottleneck, lower = 1)
  assert_number(num_var_plot, lower = 1) # Inf is allowed
  assert_number(bottom_margin, lower = 0)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	var_props = array(0, c(num_replicates_for_avg, bart_machine$p))
	for (i in 1 : num_replicates_for_avg){
		if (i == 1 & num_trees_bottleneck == bart_machine$num_trees){ ##if original BART is using right number of trees
			var_props[i, ] = get_var_props_over_chain(bart_machine, type)
		} else {
			bart_machine_dup = bart_machine_duplicate(bart_machine, num_trees = num_trees_bottleneck, run_in_sample = FALSE, verbose = FALSE)			
			var_props[i, ] = get_var_props_over_chain(bart_machine_dup, type)				
		}
		if (verbose){
			cat(".")
		}
	}
	if (verbose){
		cat("\n")
	}
	
	avg_var_props = colMeans(var_props)
	names(avg_var_props) = bart_machine$training_data_features_with_missing_features
	sd_var_props = matrixStats::colSds(var_props)
	names(sd_var_props) = bart_machine$training_data_features_with_missing_features
	
	if (num_var_plot == Inf){
		num_var_plot = bart_machine$p
	}
	
	avg_var_props_sorted_indices = sort(avg_var_props, decreasing = TRUE, index.return = TRUE)$ix
	avg_var_props = avg_var_props[avg_var_props_sorted_indices][1 : num_var_plot]
	sd_var_props = sd_var_props[avg_var_props_sorted_indices][1 : num_var_plot]		
	
	plot_obj = NULL
	if (plot){
		if (is.na(sd_var_props[1])){
			moe = 0
		} else {
			moe = 1.96 * sd_var_props / sqrt(num_replicates_for_avg)
		}
		conf_upper = avg_var_props + 1.96 * sd_var_props / sqrt(num_replicates_for_avg)
		conf_lower = avg_var_props - 1.96 * sd_var_props / sqrt(num_replicates_for_avg)
		plot_df = data.frame(
			variable = factor(names(avg_var_props), levels = names(avg_var_props)),
			value = as.numeric(avg_var_props),
			conf_lower = as.numeric(conf_lower),
			conf_upper = as.numeric(conf_upper)
		)
		p = ggplot2::ggplot(plot_df, ggplot2::aes(x = variable, y = value)) +
			ggplot2::geom_col(fill = "gray") +
			ggplot2::labs(y = "Inclusion Proportion", x = NULL) +
			ggplot2::coord_cartesian(ylim = c(0, max(avg_var_props + moe))) +
			ggplot2::theme(
				axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
				plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = bottom_margin, l = 5.5)
			)
		if (!is.na(sd_var_props[1])){
			p = p + ggplot2::geom_errorbar(
				ggplot2::aes(ymin = conf_lower, ymax = conf_upper),
				color = rgb(0.59, 0.39, 0.39),
				width = 0.2,
				linewidth = 0.6
			)
		}
		if (verbose){
			print(p)
		}
		plot_obj = p
	}
	invisible(list(avg_var_props = avg_var_props, sd_var_props = sd_var_props, plot = plot_obj))	
}

##user function calling private plotting methods
#' Plot Convergence Diagnostics
#'
#' @description
#' A suite of plots to assess convergence diagnostics and features of the BART model.
#'
#' @details
#' The ``sigsqs'' option plots the posterior error variance estimates by the Gibbs sample number. This is a standard tool to assess convergence of MCMC algorithms. This option is not applicable to classification BART models.\cr
#' The ``mh_acceptance'' option plots the proportion of Metropolis-Hastings steps accepted for each Gibbs sample (number accepted divided by number of trees).\cr
#' The ``num_nodes'' option plots the average number of nodes across each tree in the sum-of-trees model by the Gibbs sample number (for post burn-in only). The blue line
#' is the average number of nodes over all trees.\cr
#' The ``tree_depths'' option plots the average tree depth across each tree in the sum-of-trees model by the Gibbs sample number (for post burn-in only). The blue line
#' is the average number of nodes over all trees.
#' @param bart_machine An object of class ``bartMachine''.
#' @param plots The list of plots to be displayed. The four options are: "sigsqs", "mh_acceptance", "num_nodes", "tree_depths".
#' @param verbose If TRUE, prints plots to the active device.
#'
#' @return
#' None.
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' The ``sigsqs'' plot separates the burn-in \eqn{\sigma^2}'s for the first core by post burn-in \eqn{\sigma^2}'s estimates for all cores by grey vertical lines.
#' The ``mh_acceptance'' plot separates burn-in from post-burn in by a grey vertical line. Post burn-in, the different core proportions plot in different colors.
#' The ``num_nodes'' plot separates different core estimates by vertical lines (post burn-in only).
#' The `tree_depths'' plot separates different core estimates by vertical lines (post burn-in only).
#'
#' @examples
#' \dontrun{
#' #generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' 
#' #plot convergence diagnostics
#' plot_convergence_diagnostics(bart_machine)
#' }
#' @export
plot_convergence_diagnostics = function(bart_machine, plots = c("sigsqs", "mh_acceptance", "num_nodes", "tree_depths"), verbose = TRUE){
  assert_class(bart_machine, "bartMachine")
  assert_subset(plots, c("sigsqs", "mh_acceptance", "num_nodes", "tree_depths"))
  assert_flag(verbose)

  check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
  plot_list = list()
  if ("sigsqs" %in% plots){
    if (bart_machine$pred_type == "regression"){
      plot_list = c(plot_list, list(plot_sigsqs_convergence_diagnostics(bart_machine)))
    }
  }
	if ("mh_acceptance" %in% plots){
	  plot_list = c(plot_list, list(plot_mh_acceptance_reject(bart_machine)))
	}
	if ("num_nodes" %in% plots){
	  plot_list = c(plot_list, list(plot_tree_num_nodes(bart_machine)))
	}
	if ("tree_depths" %in% plots){
	  plot_list = c(plot_list, list(plot_tree_depths(bart_machine)))
	}
	num_plots = length(plot_list)
	if (num_plots == 0){
		return(invisible(list()))
	}
	if (num_plots == 1){
		if (verbose){
			print(plot_list[[1]])
		}
		return(invisible(plot_list))
	}
	if (num_plots <= 2){
		num_cols = 2
		num_rows = 1
	} else {
		num_cols = 2
		num_rows = 2
	}
	if (verbose){
		grid::grid.newpage()
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(num_rows, num_cols)))
		for (i in seq_len(num_plots)){
			row_idx = ceiling(i / num_cols)
			col_idx = i - (row_idx - 1) * num_cols
			suppressMessages(print(
				plot_list[[i]],
				vp = grid::viewport(layout.pos.row = row_idx, layout.pos.col = col_idx)
			))
		}
		grid::popViewport()
	}
	invisible(plot_list)
}

##private function
shapiro_wilk_p_val = function(vec){
	tryCatch(shapiro.test(vec)$p.value, error = function(e){})
}

##function for investigating interactions
#' Explore Pairwise Interactions in BART Model
#'
#' @description
#' Explore the pairwise interaction counts for a BART model to learn about interactions fit by the model. This function includes an option to generate a plot of the pairwise interaction counts.
#'
#' @details
#' An interaction between two variables is considered to occur whenever a path from any node of a tree to
#' any of its terminal node contains splits using those two variables. See Kapelner and Bleich, 2013, Section 4.11.
#' @param bart_machine An object of class ``bartMachine''.
#' @param plot If TRUE, a plot of the pairwise interaction counts is generated.
#' @param num_replicates_for_avg The number of replicates of BART to be used to generate pairwise interaction inclusion counts.
#'   Averaging across multiple BART models improves stability of the estimates.
#' @param num_trees_bottleneck Number of trees to be used in the sum-of-trees model for computing pairwise interactions counts.
#'   A small number of trees should be used to force the variables to compete for entry into the model.
#' @param num_var_plot Number of variables to be shown on the plot. If ``Inf,'' all variables are plotted (not recommended if
#'   the number of predictors is large). Default is 50.
#' @param cut_bottom A display parameter between 0 and 1 that controls where the y-axis is plotted. A value of 0 would begin the y-axis at 0; a value of 1 begins
#'   the y-axis at the minimum of the average pairwise interaction inclusion count (the smallest bar in the bar plot). Values between 0 and 1 begin the
#'   y-axis as a percentage of that minimum.
#' @param bottom_margin A display parameter that adjusts the bottom margin of the graph if labels are clipped. The scale of this parameter is the same as set with \code{par(mar = c(....))} in R.
#'   Higher values allow for more space if the crossed covariate names are long. Note that making this parameter too large will prevent plotting and the plot function in R will throw an error.
#' @param verbose If TRUE, prints progress messages and plots to the active device.
#'
#' @return
#' \item{interaction_counts}{For each of the \eqn{p \times p}{p times p} interactions, what is the count across all \code{num_replicates_for_avg}
#' BART model replicates' post burn-in Gibbs samples in all trees.}
#' 	\item{interaction_counts_avg}{For each of the \eqn{p \times p}{p times p} interactions, what is the average count across all \code{num_replicates_for_avg}
#' BART model replicates' post burn-in Gibbs samples in all trees.}
#' 	\item{interaction_counts_sd}{For each of the \eqn{p \times p}{p times p} interactions, what is the sd of the interaction counts across the \code{num_replicates_for_avg}
#' BART models replicates.}
#' 	\item{interaction_counts_avg_and_sd_long}{For each of the \eqn{p \times p}{p times p} interactions, what is the average and sd of the interaction counts across the \code{num_replicates_for_avg}
#' BART models replicates. The output is organized as a convenient long table of class \code{data.frame}.}
#'
#' @references
#' Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
#' with Bayesian Additive Regression Trees. Journal of Statistical
#' Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
#'
#' @seealso
#' \code{\link{investigate_var_importance}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' In the plot, the red bars correspond to the standard error of the variable inclusion proportion estimates (since multiple replicates were used).
#'
#' @examples
#' \dontrun{
#' #generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 10
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y, num_trees = 20)
#' 
#' #investigate interactions
#' interaction_investigator(bart_machine)
#' }
#' @export
interaction_investigator = function(bart_machine, plot = TRUE, num_replicates_for_avg = 5, num_trees_bottleneck = 20, num_var_plot = 50, cut_bottom = NULL, bottom_margin = 10, verbose = TRUE){
  assert_class(bart_machine, "bartMachine")
  assert_flag(plot)
  assert_int(num_replicates_for_avg, lower = 1)
  assert_int(num_trees_bottleneck, lower = 1)
  assert_number(num_var_plot, lower = 1)
  assert_number(cut_bottom, lower = 0, upper = 1, null.ok = TRUE)
  assert_number(bottom_margin, lower = 0)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	interaction_counts = array(NA, c(bart_machine$p, bart_machine$p, num_replicates_for_avg))
	
	for (r in 1 : num_replicates_for_avg){
		if (r == 1 & num_trees_bottleneck == bart_machine$num_trees){
			interaction_counts[, , r] = .jcall(bart_machine$java_bart_machine, "[[I", "getInteractionCounts", simplify = TRUE)
		} else {
			bart_machine_dup = bart_machine_duplicate(bart_machine, num_trees = num_trees_bottleneck)			
			interaction_counts[, , r] = .jcall(bart_machine_dup$java_bart_machine, "[[I", "getInteractionCounts", simplify = TRUE)
			if (verbose){
				cat(".")
				if (r %% 40 == 0){
					cat("\n")
				}
			}
		}
	}
	if (verbose){
		cat("\n")
	}
	
	interaction_counts_avg = matrix(NA, nrow = bart_machine$p, ncol = bart_machine$p)
	interaction_counts_sd = matrix(NA, nrow = bart_machine$p, ncol = bart_machine$p)
	
	if (bart_machine$use_missing_data){
		rownames(interaction_counts_avg) = bart_machine$training_data_features_with_missing_features
	    colnames(interaction_counts_avg) = bart_machine$training_data_features_with_missing_features
		rownames(interaction_counts_sd) = bart_machine$training_data_features_with_missing_features
		colnames(interaction_counts_sd) = bart_machine$training_data_features_with_missing_features
	} else {
	    rownames(interaction_counts_avg) = bart_machine$training_data_features
	    colnames(interaction_counts_avg) = bart_machine$training_data_features
		rownames(interaction_counts_sd) = bart_machine$training_data_features
		colnames(interaction_counts_sd) = bart_machine$training_data_features	
	}
	
	#now vectorize the interaction counts
	n_interactions = bart_machine$p * (bart_machine$p - 1) / 2
	avg_counts = array(NA, n_interactions)
	sd_counts = array(NA, n_interactions)
	interaction_counts_avg_and_sd_long = data.frame(
		var1 = as.character(rep(NA, n_interactions)),
		var2 = as.character(rep(NA, n_interactions)),
		avg_interaction = as.numeric(rep(NA, n_interactions)),
		se_interaction = as.numeric(rep(NA, n_interactions))
	)
	iter = 1
	for (i in 1 : bart_machine$p){
		for (j in 1 : bart_machine$p){
			if (i <= j){
				interactions_i_j = c(interaction_counts[i, j, ], interaction_counts[j, i, ]) #triangularize the data
				interaction_counts_avg[i, j] = mean(interactions_i_j)
				interaction_counts_sd[i, j] = sd(interactions_i_j)
				
				avg_counts[iter] = interaction_counts_avg[i, j]
				sd_counts[iter] = interaction_counts_sd[i, j]
				names(avg_counts)[iter] = paste(rownames(interaction_counts_avg)[i], "x", rownames(interaction_counts_avg)[j])
				
				interaction_counts_avg_and_sd_long[iter, ] = c(
					rownames(interaction_counts_avg)[i],
					rownames(interaction_counts_avg)[j],
					interaction_counts_avg[i, j],
					interaction_counts_sd[i, j] / sqrt(num_replicates_for_avg)
				)
				iter = iter + 1
			}
		}
	}
	
	if (num_var_plot == Inf || num_var_plot > n_interactions){
		num_var_plot = n_interactions
	}
	
	avg_counts_sorted_indices = sort(avg_counts, decreasing = TRUE, index.return = TRUE)$ix
	avg_counts = avg_counts[avg_counts_sorted_indices][1 : num_var_plot]
	sd_counts = sd_counts[avg_counts_sorted_indices][1 : num_var_plot]
	interaction_counts_avg_and_sd_long = interaction_counts_avg_and_sd_long[avg_counts_sorted_indices, ]
	
	if (is.null(cut_bottom)){
		ylim_bottom = 0
	} else {
		ylim_bottom = cut_bottom * min(avg_counts)
	}
	plot_obj = NULL
	if (plot){
		#now create the bar plot
		if (is.na(sd_counts[1])){
			moe = 0
		} else {
			moe = 1.96 * sd_counts / sqrt(num_replicates_for_avg)
		}
		conf_upper = avg_counts + 1.96 * sd_counts / sqrt(num_replicates_for_avg)
		conf_lower = avg_counts - 1.96 * sd_counts / sqrt(num_replicates_for_avg)
		plot_df = data.frame(
			interaction = factor(names(avg_counts), levels = names(avg_counts)),
			value = as.numeric(avg_counts),
			conf_lower = as.numeric(conf_lower),
			conf_upper = as.numeric(conf_upper)
		)
		p = ggplot2::ggplot(plot_df, ggplot2::aes(x = interaction, y = value)) +
			ggplot2::geom_col(fill = "gray") +
			ggplot2::labs(y = "Relative Importance", x = NULL) +
			ggplot2::coord_cartesian(ylim = c(ylim_bottom, max(avg_counts + moe))) +
			ggplot2::theme(
				axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
				plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = bottom_margin, l = 5.5)
			)
		if (!is.na(sd_counts[1])){
			p = p + ggplot2::geom_errorbar(
				ggplot2::aes(ymin = conf_lower, ymax = conf_upper),
				color = rgb(0.59, 0.39, 0.39),
				width = 0.2,
				linewidth = 0.6
			)
		}
		if (verbose){
			print(p)
		}
		plot_obj = p
	}
		
	invisible(list(
		interaction_counts = interaction_counts,
		interaction_counts_avg = interaction_counts_avg,
		interaction_counts_sd = interaction_counts_sd,
		interaction_counts_avg_and_sd_long = interaction_counts_avg_and_sd_long,
		plot = plot_obj
	))
}

##partial dependence plot
#' Partial Dependence Plot
#'
#' @description
#' Creates a partial dependence plot for a BART model for regression or classification.
#'
#' @details
#' For regression models, the units on the y-axis are the same as the units of the response. For classification models, the units on the y-axis are probits.
#' @param bart_machine An object of class ``bartMachine''.
#' @param j The number or name of the column in the design matrix for which the partial dependence plot is to be created.
#' @param levs Quantiles at which the partial dependence function should be evaluated. Linear extrapolation is performed between these points.
#' @param lower_ci Lower limit for credible interval
#' @param upper_ci Upper limit for credible interval
#' @param prop_data The proportion of the training data to use. Default is 1. Use a lower proportion for speedier pd_plots. The closer to 1, the more resolution
#'   the PD plot will have; the closer to 0, the lower but faster.
#' @param verbose If TRUE, prints progress messages and plots to the active device.
#'
#' @return
#' Invisibly, returns a list with the following components:
#' 
#'   \item{x_j_quants}{Quantiles at which the partial dependence function is evaluated}
#'   \item{bart_avg_predictions_by_quantile_by_gibbs}{All samples of \eqn{\hat{f}(x)}}
#'   \item{bart_avg_predictions_by_quantile}{Posterior means for \eqn{\hat{f}(x)} at \code{x_j_quants}}
#'   \item{bart_avg_predictions_lower}{Lower bound of the desired confidence of the credible interval of \eqn{\hat{f}(x)}}
#'   \item{bart_avg_predictions_upper}{Upper bound of the desired confidence of the credible interval of \eqn{\hat{f}(x)}}
#'   \item{prop_data}{The proportion of the training data to use as specified when this function was executed}
#' %% ...
#'
#' @references
#' Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
#' with Bayesian Additive Regression Trees. Journal of Statistical
#' Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
#' 
#' HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive Regressive Trees.
#' The Annals of Applied Statistics, 4(1): 266--298, 2010.
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
#'
#' @examples
#' \dontrun{
#' #Regression example
#' 
#' #generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' 
#' #partial dependence plot for quadratic term
#' pd_plot(bart_machine, "X3")
#' 
#' 
#' #Classification example
#' 
#' #get data and only use 2 factors
#' data(iris)
#' iris2 = iris[51:150,]
#' iris2$Species = factor(iris2$Species)
#' 
#' #build BART classification model
#' bart_machine = bartMachine(iris2[ ,1:4], iris2$Species)
#' 
#' #partial dependence plot
#' pd_plot(bart_machine, "Petal.Width")
#' }
#' @export
pd_plot = function(bart_machine, j, levs = c(0.05, seq(from = 0.10, to = 0.90, by = 0.10), 0.95), lower_ci = 0.025, upper_ci = 0.975, prop_data = 1, verbose = TRUE){
  assert_class(bart_machine, "bartMachine")
  # j checked manually later because it can be numeric or character
  assert_numeric(levs, lower = 0, upper = 1, min.len = 1)
  assert_number(lower_ci, lower = 0, upper = 1)
  assert_number(upper_ci, lower = 0, upper = 1)
  assert_number(prop_data, lower = 0, upper = 1)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	if (inherits(j, "integer")){
		j = as.numeric(j)
	}
	if (inherits(j, "numeric") && (j < 1 || j > bart_machine$p)){
		stop(paste("You must set j to a number between 1 and p =", bart_machine$p))
	} else if (inherits(j, "character") && !(j %in% bart_machine$training_data_features)){
		stop("j must be the name of one of the training features (see \"<bart_model>$training_data_features\")")
	} else if (!(inherits(j, "numeric") || inherits(j, "character"))){
		stop("j must be a column number or column name")
	}
	
	x_j = bart_machine$model_matrix_training_data[, j]
	
	#fail with a warning if there's only one value (we don't want an error because it would fail on loops).
	if (length(unique(na.omit(x_j))) <= 1){
		warning("There must be more than one unique value in this training feature. PD plot not generated.")
		return()
	}
	x_j_quants = unique(quantile(x_j, levs, na.rm = TRUE))
	
	#fail with a warning if there's only one value (we don't want an error because it would fail on loops).
	if (length(unique(x_j_quants)) <= 1){
		warning("There must be more than one unique value among the quantiles selected. PD plot not generated.")
		return()
	}
	
	n_pd_plot = round(bart_machine$n * prop_data)
	bart_predictions_by_quantile = array(NA, c(length(x_j_quants), n_pd_plot, bart_machine$num_iterations_after_burn_in))
	
	for (q in 1 : length(x_j_quants)){
		#pull out a certain proportion of the data randomly
		indices = sample(1 : bart_machine$n, n_pd_plot)
		#now create test data matrix
		test_data = bart_machine$X[indices, ]
		test_data[, j] = rep(x_j_quants[q], n_pd_plot)
		
		bart_predictions_by_quantile[q, , ] = bart_machine_get_posterior(bart_machine, test_data, verbose = verbose)$y_hat_posterior_samples
		if (verbose){
			cat(".")
		}
	}
	if (verbose){
		cat("\n")
	}
	
  	if (bart_machine$pred_type == "classification"){ ##convert to probits
    	bart_predictions_by_quantile = qnorm(bart_predictions_by_quantile)
  	}
  
	bart_avg_predictions_by_quantile_by_gibbs = apply(bart_predictions_by_quantile, c(1, 3), mean)
	
	bart_avg_predictions_by_quantile = rowMeans(bart_avg_predictions_by_quantile_by_gibbs)
	bart_avg_predictions_lower = as.numeric(matrixStats::rowQuantiles(
		bart_avg_predictions_by_quantile_by_gibbs,
		probs = lower_ci
	))
	bart_avg_predictions_upper = as.numeric(matrixStats::rowQuantiles(
		bart_avg_predictions_by_quantile_by_gibbs,
		probs = upper_ci
	))
	
	var_name = ifelse(class(j) == "character", j, bart_machine$training_data_features[j])
    ylab_name = ifelse(bart_machine$pred_type == "classification", "Partial Effect (Probits)", "Partial Effect")
	pd_df = data.frame(
		quantile = x_j_quants,
		avg = bart_avg_predictions_by_quantile,
		lower = bart_avg_predictions_lower,
		upper = bart_avg_predictions_upper
	)
	y_limits = c(min(bart_avg_predictions_lower, bart_avg_predictions_upper),
		max(bart_avg_predictions_lower, bart_avg_predictions_upper))
	p = ggplot2::ggplot(pd_df, ggplot2::aes(x = quantile, y = avg)) +
		ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "gray87") +
		ggplot2::geom_line(color = "blue", ggplot2::aes(y = lower)) +
		ggplot2::geom_line(color = "blue", ggplot2::aes(y = upper)) +
		ggplot2::geom_line(linewidth = 1) +
		ggplot2::geom_point() +
		ggplot2::coord_cartesian(ylim = y_limits) +
		ggplot2::labs(
			title = "Partial Dependence Plot",
			y = ylab_name,
			x = paste(var_name, "plotted at specified quantiles")
		)
	if (verbose){
		print(p)
	}
	
	invisible(list(
		x_j_quants = x_j_quants, 
		bart_avg_predictions_by_quantile_by_gibbs = bart_avg_predictions_by_quantile_by_gibbs, 
		bart_avg_predictions_by_quantile = bart_avg_predictions_by_quantile, 
		bart_avg_predictions_lower = bart_avg_predictions_lower,
		bart_avg_predictions_upper = bart_avg_predictions_upper,
		prop_data = prop_data,
		plot = p
	))
}

##plot and invisibly return out-of-sample RMSE by the number of trees
#' Assess the Out-of-sample RMSE by Number of Trees
#'
#' @description
#' Assess out-of-sample RMSE of a BART model for varying numbers of trees in the sum-of-trees model.
#' @param bart_machine An object of class ``bartMachine''.
#' @param tree_list List of sizes for the sum-of-trees models.
#' @param in_sample If TRUE, the RMSE is computed on in-sample data rather than an out-of-sample holdout.
#' @param plot If TRUE, a plot of the RMSE by the number of trees in the ensemble is created.
#' @param holdout_pctg Percentage of the data to be treated as an out-of-sample holdout.
#' @param num_replicates Number of replicates to average the results over. Each replicate uses a randomly sampled holdout of the data, (which could have overlap).
#' @param verbose If TRUE, prints progress messages and plots to the active device.
#' @param ... Other arguments to be passed to the plot function.
#'
#' @return
#' Invisibly, returns the out-of-sample average RMSEs for each tree size.
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' Since using a large number of trees can substantially increase computation time, this plot can help assess whether a smaller ensemble size is sufficient to obtain desirable predictive performance.
#' This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
#'
#' @examples
#' \dontrun{
#' #generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 10
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y, num_trees = 20)
#' 
#' #explore RMSE by number of trees
#' rmse_by_num_trees(bart_machine)
#' }
#' @export
rmse_by_num_trees = function(bart_machine, tree_list = c(5, seq(10, 50, 10), 100, 150, 200), in_sample = FALSE, plot = TRUE, holdout_pctg = 0.3, num_replicates = 4, verbose = TRUE, ...){
  assert_class(bart_machine, "bartMachine")
  assert_integerish(tree_list, lower = 1, min.len = 1)
  assert_flag(in_sample)
  assert_flag(plot)
  assert_number(holdout_pctg, lower = 0, upper = 1)
  assert_int(num_replicates, lower = 1)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (bart_machine$pred_type == "classification"){
		stop("This function does not work for classification.")
	}		
	X = bart_machine$X
	y = bart_machine$y
	n = bart_machine$n
	
	rmses = array(NA, c(num_replicates, length(tree_list)))
	if (verbose){
		cat("num_trees = ")
	}
	for (t in 1 : length(tree_list)){
		for (r in 1 : num_replicates){
			if (in_sample){
				bart_machine_dup = bart_machine_duplicate(bart_machine, num_trees = tree_list[t], run_in_sample = TRUE)
				rmses[r, t] = bart_machine_dup$rmse_train				
			} else {
				holdout_indicies = sample(1 : n, holdout_pctg * n)
				Xtrain = X[setdiff(1 : n, holdout_indicies), ]
				ytrain = y[setdiff(1 : n, holdout_indicies)]
				Xtest = X[holdout_indicies, ]
				ytest = y[holdout_indicies]
				
				bart_machine_dup = bart_machine_duplicate(bart_machine, Xtrain, ytrain, num_trees = tree_list[t])
				predict_obj = suppressWarnings(bart_predict_for_test_data(
					bart_machine_dup,
					Xtest,
					ytest,
					verbose = verbose
				)) ##predict on holdout
				rmses[r, t] = predict_obj$rmse				
			}
			if (verbose){
				cat("..")
				cat(tree_list[t])
			}			
		}
	}
	if (verbose){
		cat("\n")
	}
	
	rmse_means = colMeans(rmses, na.rm = TRUE)

	if (plot){ ##plotting
		if (num_replicates > 1){
			rmse_sds = matrixStats::colSds(rmses)
		} else {
			rmse_sds = rep(0, length(tree_list))
		}
		y_mins = rmse_means - 2 * rmse_sds
		y_maxs = rmse_means + 2 * rmse_sds
		plot_df = data.frame(
			num_trees = tree_list,
			rmse = rmse_means,
			lower = rmse_means - 1.96 * rmse_sds / sqrt(num_replicates),
			upper = rmse_means + 1.96 * rmse_sds / sqrt(num_replicates)
		)
		plot_df = plot_df[is.finite(plot_df$rmse), , drop = FALSE]
		finite_vals = c(plot_df$rmse, plot_df$lower, plot_df$upper)
		finite_vals = finite_vals[is.finite(finite_vals)]
		if (length(finite_vals) == 0){
			warning("RMSE estimates are not finite; skipping plot.")
			return(invisible(rmse_means))
		}
		y_limits = range(finite_vals)
		p = ggplot2::ggplot(plot_df, ggplot2::aes(x = num_trees, y = rmse)) +
			ggplot2::geom_line() +
			ggplot2::geom_point() +
			ggplot2::coord_cartesian(ylim = y_limits) +
			ggplot2::labs(
				title = paste(ifelse(in_sample, "In-Sample", "Out-Of-Sample"), "RMSE by Number of Trees"),
				x = "Number of Trees",
				y = paste(ifelse(in_sample, "In-Sample", "Out-Of-Sample"), "RMSE")
			)
		if (num_replicates > 1){
			p = p + ggplot2::geom_errorbar(
				ggplot2::aes(ymin = lower, ymax = upper),
				color = "grey",
				width = 0.2,
				linewidth = 0.3
			)
		}
		if (verbose){
			print(p)
		}
	}
	if (plot){
		attr(rmse_means, "plot") = p
	}
	invisible(rmse_means)
}
