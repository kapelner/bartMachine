##give summary info about bart
#' Summarizes information about a \code{bartMachine} object.
#'
#' @description
#' Provides a quick summary of the BART model.
#'
#' @details
#' Gives the version number of the \code{bartMachine} package used to build this \code{additiveBartMachine} object and if the object
#' models either ``regression'' or ``classification.'' Gives the amount of training data and the dimension of feature space. Prints
#' the amount of time it took to build the model, how many processor cores were used to during its construction, as well as the
#' number of burn-in and posterior Gibbs samples were used.
#' 
#' If the model is for regression, it prints the estimate of \eqn{\sigma^2} before the model was constructed as well as after so
#' the user can inspect how much variance was explained.
#' 
#' If the model was built using the \code{run_in_sample = TRUE} parameter in \code{\link{build_bart_machine}} and is for regression, the summary L1,
#' L2, rmse, Pseudo-\eqn{R^2} are printed as well as the p-value for the tests of normality and zero-mean noise. If the model is for classification, a confusion matrix is printed.
#' @param object An object of class ``bartMachine''.
#' @param verbose If TRUE, prints summary output.
#' @param ... Parameters that are ignored.
#'
#' @return
#' None.
#'
#' @author
#' Adam Kapelner
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
#' ##print out details
#' summary(bart_machine)
#' 
#' ##Also, the default print works too
#' bart_machine
#' }
#' @export
summary.bartMachine = function(object, verbose = TRUE, ...){	
  assert_class(object, "bartMachine")
  assert_flag(verbose)

	if (verbose){
		cat(paste("bartMachine v", packageVersion("bartMachine"), ifelse(object$pred_type == "regression", " for regression", " for classification"), "\n\n", sep = ""))
		if (object$use_missing_data){
			cat("Missing data feature ON\n")
		}
		#first print out characteristics of the training data
		if (!is.null(object$interaction_constraints)){
			cat(paste0("number of specified covariate interactivity constraints = ", length(object$interaction_constraints), ".\ntraining data size: n = ", object$n, " and p = ", object$p, "\n"))
		} else {
			cat(paste("training data size: n =", object$n, "and p =", object$p, "\n"))
		}	
		
		##build time
		ttb = as.numeric(object$time_to_build, units = "secs")
		if (ttb > 60){
			ttb = as.numeric(object$time_to_build, units = "mins")
			cat(paste("built in", round(ttb, 2), "mins on", object$num_cores, ifelse(object$num_cores == 1, "core,", "cores,"), object$num_trees, "trees,", object$num_burn_in, "burn-in and", object$num_iterations_after_burn_in, "post. samples\n"))
		} else {
			cat(paste("built in", round(ttb, 1), "secs on", object$num_cores, ifelse(object$num_cores == 1, "core,", "cores,"), object$num_trees, "trees,", object$num_burn_in, "burn-in and", object$num_iterations_after_burn_in, "post. samples\n"))
		}
		
		if (object$pred_type == "regression"){
			sigsq_est = sigsq_est(object) ##call private function
			cat(paste("\nsigsq est for y beforehand:", round(object$sig_sq_est, 3), "\n"))
			cat(paste("avg sigsq estimate after burn-in:", round(sigsq_est, 5), "\n"))
			
			if (object$run_in_sample){
				cat("\nin-sample statistics:\n")
				cat(paste(" L1 =", round(object$L1_err_train, 2), "\n",
								"L2 =", round(object$L2_err_train, 2), "\n",
								"rmse =", round(object$rmse_train, 2), "\n"),
						"Pseudo-Rsq =", round(object$PseudoRsq, 4))
				
				es = object$residuals
				if (length(es) > 5000){
					normal_p_val = shapiro.test(sample(es, 5000))$p.value
				} else {
					normal_p_val = shapiro.test(es)$p.value
				}
				cat("\np-val for shapiro-wilk test of normality of residuals:", round(normal_p_val, 5), "\n")
				
				centered_p_val = t.test(es)$p.value
				cat("p-val for zero-mean noise:", round(centered_p_val, 5), "\n")	
			} else {
				cat("\nno in-sample information available (use option run_in_sample = TRUE next time)\n")
			}		
		} else if (object$pred_type == "classification"){
			if (object$run_in_sample){
				cat("\nconfusion matrix:\n\n")
				print(object$confusion_matrix)
			} else {
				cat("\nno in-sample information available (use option run_in_sample = TRUE next time)\n")
			}		
		}
		cat("\n")
	}
	invisible(object)
}

#alias for summary
#' Summarizes information about a \code{bartMachine} object.
#'
#' @description
#' This is an alias for the \code{\link{summary.bartMachine}} function. See description in that section.
#' @param x An object of class ``bartMachine''.
#' @param verbose If TRUE, prints summary output.
#' @param ... Parameters that are ignored.
#'
#' @return
#' None.
#'
#' @author
#' Adam Kapelner and Justin Bleich
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
#' ##print out details
#' print(bart_machine)
#' 
#' ##Also, the default print works too
#' bart_machine
#' }
#' @export
print.bartMachine = function(x, verbose = TRUE, ...){
  assert_class(x, "bartMachine")
  assert_flag(verbose)

	summary(x, verbose = verbose, ...)
	invisible(x)
}
