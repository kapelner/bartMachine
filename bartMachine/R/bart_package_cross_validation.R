##performs out-of-sample error estimation for a BART model
#' Estimate Out-of-sample Error with K-fold Cross validation
#'
#' @description
#' Builds a BART model using a specified set of arguments to \code{build_bart_machine} and estimates the out-of-sample performance by using k-fold cross validation.
#'
#' @details
#' For each fold, a new BART model is trained (using the same set of arguments) and its performance is evaluated on the holdout piece of that fold.
#' @param X Data frame of predictors. Factors are automatically converted to dummies internally.
#' @param y Vector of response variable. If \code{y} is \code{numeric} or \code{integer}, a BART model for regression is built. If \code{y} is a factor with two levels, a BART model for classification is built.
#' @param k_folds Number of folds to cross-validate over. This argument is ignored if \code{folds_vec} is non-null.
#' @param folds_vec An integer vector of indices specifying which fold each observation belongs to.
#' @param verbose Prints information about progress of the algorithm to the screen.
#' @param \dots Additional arguments to be passed to \code{build_bart_machine}.
#'
#' @return
#' For regression models, a list with the following components is returned:
#'   \item{y_hat}{Predictions for the observations computed on the fold for which the observation was omitted from the training set.}
#'   \item{L1_err}{Aggregate L1 error across the folds.}
#'   \item{L2_err}{Aggregate L1 error across the folds.}
#'   \item{rmse}{Aggregate RMSE across the folds.}
#'   \item{folds}{Vector of indices specifying which fold each observation belonged to.}
#' 
#' For classification models, a list with the following components is returned:
#' 
#'   \item{y_hat}{Class predictions for the observations computed on the fold for which the observation was omitted from the training set.}
#'     \item{p_hat}{Probability estimates for the observations computed on the fold for which the observation was omitted from the training set.}
#'   \item{confusion_matrix}{Aggregate confusion matrix across the folds.}
#'     \item{misclassification_error}{Total misclassification error across the folds.}
#'     \item{folds}{Vector of indices specifying which fold each observation belonged to.}
#'
#' @seealso
#' \code{\link{bartMachine}}
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
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' #evaluate default BART on 5 folds
#' k_fold_val = k_fold_cv(X, y)
#' print(k_fold_val$rmse)
#' }
#' @export
k_fold_cv = function(X, y, k_folds = 5, folds_vec = NULL, verbose = FALSE, ...){
  # Validate arguments
  assert_data_frame(X)
  assert_atomic_vector(y)
  assert_number(k_folds, lower = 2) # Inf is allowed as it is > 2, but wait assert_number(Inf) works.
  assert_integerish(folds_vec, null.ok = TRUE)
  assert_flag(verbose)

	#we cannot afford the time sink of serialization during the grid search, so shut it off manually
	args = list(...)
	args$serialize = FALSE

	
	if (!inherits(X, "data.frame")){
		stop("The training data X must be a data frame.")
	}
	if (!(class(y) %in% c("numeric", "integer", "factor"))){
		stop("Your response must be either numeric, an integer or a factor with two levels.\n")
	}	
    if (!is.null(folds_vec) & !inherits(folds_vec, "integer")){
	  stop("folds_vec must be an a vector of integers specifying the indexes of each folds.")  
    }
	
	y_levels = levels(y)
	if (inherits(y, "numeric") || inherits(y, "integer")){ #if y is numeric, then it's a regression problem
		pred_type = "regression"
	} else if (inherits(y, "factor") & length(y_levels) == 2){ #if y is a factor and and binary, then it's a classification problem
		pred_type = "classification"
	}
	
	n = nrow(X)
	Xpreprocess = pre_process_training_data(X)$data
	
	p = ncol(Xpreprocess)
	
	#set up k folds
    if (is.null(folds_vec)){ ##if folds were not pre-set:
	  	if (k_folds == Inf){ #leave-one-out
	  		k_folds = n
	  	}
	  	
	  	if (k_folds <= 1 || k_folds > n){
	  		stop("The number of folds must be at least 2 and less than or equal to n, use \"Inf\" for leave one out")
	  	}
		
	  	temp = rnorm(n)
		
	  	folds_vec = cut(temp, breaks = quantile(temp, seq(0, 1, length.out = k_folds + 1)), 
  	                include.lowest= T, labels = F)
    } else {
    	k_folds = length(unique(folds_vec)) ##otherwise we know the folds, so just get k
    }	

	if (pred_type == "regression"){
		L1_err = 0
		L2_err = 0
		yhat_cv = numeric(n) ##store cv
	} else {
    phat_cv = numeric(n)
    yhat_cv = factor(n, levels = y_levels)
		confusion_matrix = matrix(0, nrow = 3, ncol = 3)
		rownames(confusion_matrix) = c(paste("actual", y_levels), "use errors")
		colnames(confusion_matrix) = c(paste("predicted", y_levels), "model errors")
	}

	Xy = data.frame(Xpreprocess, y) ##set up data
	
	for (k in 1 : k_folds){
		if (verbose){
			cat(".")
		}
		
	    train_idx = which(folds_vec != k)
	    test_idx = setdiff(1 : n, train_idx)
		test_data_k = Xy[test_idx, ]
		training_data_k = Xy[train_idx, ]


		
   		#build bart object
		bart_machine_cv = do.call(build_bart_machine, c(list(
							X = training_data_k[, 1 : p, drop = FALSE], 
							y = training_data_k[, (p + 1)], 
							run_in_sample = FALSE, 
							verbose = verbose), args))
		predict_obj = bart_predict_for_test_data(
			bart_machine_cv,
			test_data_k[, 1 : p, drop = FALSE],
			test_data_k[, (p + 1)],
			verbose = verbose
		)
		
		#tabulate errors
		if (pred_type == "regression"){
			L1_err = L1_err + predict_obj$L1_err
			L2_err = L2_err + predict_obj$L2_err
      		yhat_cv[test_idx] = predict_obj$y_hat
		} else {
	        phat_cv[test_idx] = predict_obj$p_hat
	        yhat_cv[test_idx] = predict_obj$y_hat
			tab = table(factor(test_data_k$y, levels = y_levels), factor(predict_obj$y_hat, levels = y_levels))
			confusion_matrix[1 : 2, 1 : 2] = confusion_matrix[1 : 2, 1 : 2] + tab
		}
	}
	if (verbose){
		cat("\n")
	}
	if (pred_type == "regression"){
		list(y_hat = yhat_cv, L1_err = L1_err, L2_err = L2_err, rmse = sqrt(L2_err / n), PseudoRsq = 1 - L2_err / sum((y - mean(y))^2), folds = folds_vec)
	} else {		
		#calculate the rest of the confusion matrix and return it plus the errors
		confusion_matrix[3, 1] = round(confusion_matrix[2, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1]), 3)
		confusion_matrix[3, 2] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 2] + confusion_matrix[2, 2]), 3)
		confusion_matrix[1, 3] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 1] + confusion_matrix[1, 2]), 3)
		confusion_matrix[2, 3] = round(confusion_matrix[2, 1] / (confusion_matrix[2, 1] + confusion_matrix[2, 2]), 3)
		confusion_matrix[3, 3] = round((confusion_matrix[1, 2] + confusion_matrix[2, 1]) / sum(confusion_matrix[1 : 2, 1 : 2]), 3)
		list(y_hat = yhat_cv, phat = phat_cv, confusion_matrix = confusion_matrix, misclassification_error = confusion_matrix[3, 3], folds = folds_vec)
	}
	
}
