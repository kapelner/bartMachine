##function to permute columns of X and check BART's performance
#' Importance Test for Covariate(s) of Interest
#'
#' @description
#' This function tests the null hypothesis \eqn{H_0}: These covariates of interest
#' do not affect the response under the assumptions of the BART
#' model.
#'
#' @details
#' To test the importance of a covariate or a set of covariates of interest on the response, this function generates
#' \code{num_permutations} BART models with the covariate(s) of interest permuted (differently each time).
#' On each run, a measure of fit is recorded. For regression, the metric is Pseudo-Rsq; for classification, it is
#' total misclassification error.\cr A
#' p-value can then be generated as follows. For regression, the p-value is the number of
#' permutation-sampled Pseudo-Rsq's greater than the observed Pseudo-Rsq divided by
#' \code{num_permutations + 1}. For classification, the p-value is the number of permutation-sampled
#' total misclassification errors less than the observed total misclassification error divided by \code{num_permutations + 1}.
#' @param bart_machine An object of class ``bart_machine''.
#' @param covariates A vector of names of covariates of interest to be tested for having an effect on the response. A value of NULL
#'   indicates an omnibus test for all covariates having an effect on the response. If the name of a covariate is a factor,
#'   the entire factor will be permuted. We do not recommend entering the names of factor covariate dummies.
#' @param num_permutation_samples The number of times to permute the covariates of interest and create a corresponding new BART model (see details).
#' @param plot If \code{TRUE}, this produces a histogram of the Pseudo-Rsq's / total misclassification error rates from
#'   the \code{num_permutations} BART models created with the \code{covariates} permuted. The plot also illustrates
#'   the observed Pseudo-Rsq's / total misclassification error rate from the original training data and indicates
#'   the test's p-value.
#' @param verbose If TRUE, prints progress and summary messages.
#'
#' @return
#' \item{permutation_samples_of_error}{A vector which records the error metric of the BART models with the covariates permuted (see details).}
#' \item{observed_error_estimate}{For regression, this is the Pseudo-Rsq on the original
#' training data set. For classification, this is the observed total misclassification error
#' on the original training data set.}
#' \item{pval}{The approximate p-value for this test (see details).
#' }
#'
#' @references
#' Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
#' with Bayesian Additive Regression Trees. Journal of Statistical
#' Software, 70(4), 1-40. \doi{10.18637/jss.v070.i04}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
#'
#' @examples
#' \dontrun{
#' ##regression example
#' 
#' ##generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' 
#' ##now test if X[, 1] affects Y nonparametrically under the BART model assumptions
#' cov_importance_test(bart_machine, covariates = c(1))
#' ## note the plot and the printed p-value
#' 
#' }
#' @export
cov_importance_test = function(bart_machine, covariates = NULL, num_permutation_samples = 100, plot = TRUE, verbose = TRUE){
  assert_class(bart_machine, "bartMachine")
  # covariates can be numeric (indices) or character (names) or NULL
  assert(
    check_numeric(covariates, null.ok = TRUE, min.len = 1, lower = 1),
    check_character(covariates, null.ok = TRUE, min.len = 1),
    combine = "or"
  )
  assert_int(num_permutation_samples, lower = 1)
  assert_flag(plot)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	#be able to handle regular expressions to find the covariates
	
	all_covariates = bart_machine$training_data_features_with_missing_features
	
	if (is.null(covariates)){
		title = "bartMachine omnibus test for covariate importance\n"
	} else if (length(covariates) <= 3){
		if (inherits(covariates[1], "numeric")){
			cov_names = paste(all_covariates[covariates], collapse = ", ")
		} else {
			cov_names = paste(covariates, collapse = ", ")
		}
		title = paste("bartMachine test for importance of covariate(s):", cov_names, "\n")
	} else {
		title = paste("bartMachine test for importance of", length(covariates), "covariates", "\n")
	}
	if (verbose){
		cat(title)
	}
	observed_error_estimate = ifelse(bart_machine$pred_type == "regression", bart_machine$PseudoRsq, bart_machine$misclassification_error)
		
	permutation_samples_of_error = array(NA, num_permutation_samples)
	for (nsim in 1 : num_permutation_samples){
		if (verbose){
			cat(".")
			if (nsim %% 50 == 0){
				cat("\n")
			}
		}	
		#omnibus F-like test - just permute y (same as permuting ALL the columns of X and it's faster)
		if (is.null(covariates)){
			bart_machine_samp = bart_machine_duplicate(bart_machine, y = sample(bart_machine$y), run_in_sample = TRUE, verbose = FALSE) #we have to turn verbose off otherwise there would be too many outputs
		#partial F-like test - permute the columns that we're interested in seeing if they matter
		} else {
			X_samp = bart_machine$X #copy original design matrix
			
			covariates_left_to_permute = c()
			for (cov in covariates){
				if (cov %in% colnames(X_samp)){
					X_samp[, cov] = sample(X_samp[, cov])
				} else {
					covariates_left_to_permute = c(covariates_left_to_permute, cov)
				}
			}

			bart_machine_samp = bart_machine_duplicate(bart_machine, X = X_samp, covariates_to_permute = covariates_left_to_permute, run_in_sample = TRUE, verbose = FALSE) #we have to turn verbose off otherwise there would be too many outputs
		}
		#record permutation result
		permutation_samples_of_error[nsim] = ifelse(bart_machine$pred_type == "regression", bart_machine_samp$PseudoRsq, bart_machine_samp$misclassification_error)	
	}
	if (verbose){
		cat("\n")
	}
  
	##compute p-value
	pval = ifelse(bart_machine$pred_type == "regression", sum(observed_error_estimate < permutation_samples_of_error), sum(observed_error_estimate > permutation_samples_of_error)) / (num_permutation_samples + 1)
	
	plot_obj = NULL
	if (plot){
		x_min = min(c(permutation_samples_of_error, 0.99 * observed_error_estimate), na.rm = TRUE)
		x_max = max(c(permutation_samples_of_error, 1.01 * observed_error_estimate), na.rm = TRUE)
		bins = max(1, floor(num_permutation_samples / 10))
		plot_df = data.frame(error = permutation_samples_of_error)
		plot_obj = ggplot2::ggplot(plot_df, ggplot2::aes(x = error)) +
			ggplot2::geom_histogram(bins = bins, fill = "gray70", color = "white") +
			ggplot2::geom_vline(xintercept = observed_error_estimate, color = "blue", linewidth = 1) +
			ggplot2::coord_cartesian(xlim = c(x_min, x_max)) +
			ggplot2::labs(
				title = paste(title, "Null Samples of", ifelse(bart_machine$pred_type == "regression", "Pseudo-R^2's", "Misclassification Errors")),
				x = paste("permutation samples\n pval =", round(pval, 3)),
				y = "count"
			) +
			ggplot2::theme_minimal()
		if (verbose){
			print(plot_obj)
		}
	}
	if (verbose){
		cat("p_val = ", pval, "\n")
	}
	result = list(
		permutation_samples_of_error = permutation_samples_of_error,
		observed_error_estimate = observed_error_estimate,
		pval = pval
	)
	if (!is.null(plot_obj)){
		attr(result, "plot") = plot_obj
	}
	invisible(result)
}

#' Test of Linearity
#'
#' @description
#' Test to investigate \eqn{H_0:} the functional relationship between the response and the
#' regressors is linear. We fit a linear model and then test if the residuals are a function
#' of the regressors using the
#' @param lin_mod A linear model you can pass in if you do not want to use the default which is \code{lm(y ~ X)}. Default is \code{NULL} which should be used if you pass in \code{X} and \code{y}.
#' @param X Data frame of predictors. Factors are automatically converted to dummies internally. Default is \code{NULL} which should be used if you pass in \code{lin_mode}.
#' @param y Vector of response variable. If \code{y} is \code{numeric} or \code{integer}, a BART model for regression is built. If \code{y} is a factor with two levels, a BART model for classification is built.
#'   Default is \code{NULL} which should be used if you pass in \code{lin_mode}.
#' @param num_permutation_samples This function relies on \code{\link{cov_importance_test}} (see documentation there for details).
#' @param plot This function relies on \code{\link{cov_importance_test}} (see documentation there for details).
#' @param verbose If TRUE, prints progress and summary messages.
#' @param ... Additional parameters to be passed to \code{bartMachine}, the model constructed on the residuals of the linear model.
#'
#' @return
#' \item{permutation_samples_of_error}{	This function relies on \code{\link{cov_importance_test}} (see documentation there for details).
#' }
#' \item{observed_error_estimate}{	This function relies on \code{\link{cov_importance_test}} (see documentation there for details).
#' }
#' \item{pval}{The approximate p-value for this test. See the documentation at \code{\link{cov_importance_test}}.
#' }
#'
#' @seealso
#' \code{\link{cov_importance_test}}
#'
#' @author
#' Adam Kapelner
#'
#' @examples
#' \dontrun{
#' ##regression example
#' 
#' ##generate Friedman data i.e. a nonlinear response model
#' set.seed(11)
#' n  = 200
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##now test if there is a nonlinear relationship between X1, ..., X5 and y.
#' linearity_test(X = X, y = y)
#' ## note the plot and the printed p-value.. should be approx 0
#' 
#' #generate a linear response model
#' y = 1 * X[ ,1] + 3 * X[,2] + 5 * X[,3] + 7 * X[ ,4] + 9 * X[,5] + rnorm(n)
#' linearity_test(X = X, y = y)
#' ## note the plot and the printed p-value.. should be > 0.05
#' 
#' }
#' @export
linearity_test = function(lin_mod = NULL, X = NULL, y = NULL, num_permutation_samples = 100, plot = TRUE, verbose = TRUE, ...){
  assert_class(lin_mod, "lm", null.ok = TRUE)
  assert_data_frame(X, null.ok = TRUE)
  if (!is.null(y)) assert_atomic_vector(y)
  assert_int(num_permutation_samples, lower = 1)
  assert_flag(plot)
  assert_flag(verbose)
  if (is.null(lin_mod) && (is.null(X) || is.null(y))) {
      stop("If lin_mod is not provided, both X and y must be provided.")
  }

	if (is.null(lin_mod)){
		lin_mod = lm(y ~ as.matrix(X))
	}
	y_hat = predict(lin_mod, X)
	bart_mod = bartMachine(X, y - y_hat, verbose = verbose, ...)
	cov_importance_test(bart_mod, num_permutation_samples = num_permutation_samples, plot = plot, verbose = verbose)	
}
