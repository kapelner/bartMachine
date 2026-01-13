#S3 predict method
#' Make a prediction on data using a BART object
#'
#' @description
#' Makes a prediction on new data given a fitted BART model for regression or classification.
#' @param object An object of class ``bartMachine''.
#' @param new_data A data frame where each row is an observation to predict. The column names
#'   should be the same as the column names of the training data.
#' @param type Only relevant if the bartMachine model is classification. The type can be ``prob'' which will
#'   return the estimate of \eqn{P(Y = 1)}(the ``positive'' class) or ``class'' which will return the best guess as to the
#'   class of the object, in the original label, based on if the probability estimate is greater
#'   than \code{prob_rule_class}. Default is ``prob.''
#' @param prob_rule_class The rule to determine when the class estimate is \eqn{Y = 1} (the ``positive'' class) based on the probability estimate. This
#'   defaults to what was originally specified in the \code{bart_machine} object.
#' @param verbose Prints out prediction-related messages. Currently in use only for probability predictions to let the user know which class
#'   is being predicted. Default is \code{TRUE}.
#' @param ... Parameters that are ignored.
#'
#' @return
#' If regression, a numeric vector of \code{y_hat}, the best guess as to the response. If classification and \code{type = ``prob''},
#' a numeric vector of \code{p_hat}, the best guess as to the probability of the response class being  the ''positive'' class. If classification and
#' \code{type = ''class''}, a character vector of the best guess of the response's class labels.
#'
#' @seealso
#' \code{\link{bart_predict_for_test_data}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @examples
#' #Regression example
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
#' ##make predictions on the training data
#' y_hat = predict(bart_machine, X)
#' 
#' #Classification example
#' data(iris)
#' iris2 = iris[51 : 150, ] #do not include the third type of flower for this example
#' iris2$Species = factor(iris2$Species)
#' bart_machine = bartMachine(iris2[ ,1:4], iris2$Species)
#' 
#' ##make probability predictions on the training data
#' p_hat = predict(bart_machine, X)
#' 
#' ##make class predictions on test data
#' y_hat_class = predict(bart_machine, X, type = "class")
#' 
#' ##make class predictions on test data conservatively for ''versicolor''
#' y_hat_class_conservative = predict(bart_machine, X, type = "class", prob_rule_class = 0.9)
#' }
#' @export
predict.bartMachine = function(object, new_data, type = "prob", prob_rule_class = NULL, verbose = TRUE, ...){
  # Validate arguments
  assert_class(object, "bartMachine")
  assert_data_frame(new_data)
  assert_choice(type, c("prob", "class"))
  assert_number(prob_rule_class, lower = 0, upper = 1, null.ok = TRUE)
  assert_flag(verbose)

	check_serialization(object) #ensure the Java object exists and fire an error if not
	
	if(!(type %in% c("prob", "class"))){
		stop("For classification, type must be either \"prob\" or \"class\". ")
	}
  
	if (object$pred_type == "regression"){	
		bart_machine_get_posterior_mean(object, new_data, verbose = verbose)$y_hat
	} else { ##classification
	    if (type == "prob"){
			if (isTRUE(verbose)){
				cat("predicting probabilities where \"", object$y_levels[1], "\" is considered the target level...\n", sep = "")
			}			
	    	bart_machine_get_posterior_mean(object, new_data, verbose = verbose)$y_hat
	    } else {
	    	labels = bart_machine_get_posterior_mean(object, new_data, verbose = verbose)$y_hat > ifelse(is.null(prob_rule_class), object$prob_rule_class, prob_rule_class)
	      	#return whatever the raw y_levels were
	      	labels_to_y_levels(object, labels)      
	    }
	}	
}

##private function
labels_to_y_levels = function(bart_machine, labels){
	factor(ifelse(labels == TRUE, bart_machine$y_levels[1], bart_machine$y_levels[2]), levels = bart_machine$y_levels)
}

.bartMachine_pred_cache <- new.env(parent = emptyenv())
.bartMachine_pred_cache$processed_data_array <- NULL

prepare_prediction_data = function(bart_machine, new_data){
	if (!"data.frame"%in%class(new_data)){		
		stop("\"new_data\" needs to be a data frame with the same column names as the training data.")
	}
	clean_data = new_data
	if (!bart_machine$use_missing_data && !bart_machine$replace_missing_data_with_x_j_bar){
		if (anyNA(clean_data)){
			nrow_before = nrow(clean_data)
			clean_data = na.omit(clean_data)
			if (nrow_before > nrow(clean_data)){
				warning(nrow_before - nrow(clean_data), " rows omitted due to missing data. Try using the missing data feature in \"build_bart_machine\" to be able to predict on all observations.", call. = FALSE)
			}
		}
	}
	
	if (nrow(clean_data) == 0){
		stop("No rows to predict.\n")
	}
	
	cache = .bartMachine_pred_cache
	if (!is.null(cache$java_bart_machine) &&
		identical(cache$java_bart_machine, bart_machine$java_bart_machine) &&
		!is.null(cache$raw_data) &&
		identical(cache$raw_data, clean_data)) {
		return(list(data = cache$processed_data, array = cache$processed_data_array))
	}
	
	if (identical(clean_data, bart_machine$X) &&
		(bart_machine$use_missing_data || bart_machine$replace_missing_data_with_x_j_bar || !anyNA(clean_data))) {
		processed_data = bart_machine$model_matrix_training_data[, 1 : bart_machine$p, drop = FALSE]
	} else {
		processed_data = pre_process_new_data(clean_data, bart_machine)
	}
	
	cache$java_bart_machine = bart_machine$java_bart_machine
	cache$raw_data = clean_data
	cache$processed_data = processed_data
	cache$processed_data_array = .jarray(processed_data, dispatch = TRUE)
	list(data = processed_data, array = cache$processed_data_array)
}

bart_machine_get_posterior_mean = function(bart_machine, new_data, verbose = TRUE){
  # Validate arguments
  assert_class(bart_machine, "bartMachine")
  assert_data_frame(new_data)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	prepared_data = prepare_prediction_data(bart_machine, new_data)
	new_data = prepared_data$data
		
	y_hat = .jcall(
		bart_machine$java_bart_machine,
		"[D",
		"getPosteriorMeanForPrediction",
		prepared_data$array,
		as.integer(bart_machine_num_cores()),
		simplify = TRUE
	)
	list(y_hat = y_hat, X = new_data)
}

##utility function for predicting when test outcomes are known
#' Predict for Test Data with Known Outcomes
#'
#' @description
#' Utility wrapper function for computing out-of-sample metrics for a BART model when the test set outcomes are known.
#' @param bart_machine An object of class ``bartMachine''.
#' @param Xtest Data frame for test data containing rows at which predictions are to be made. Colnames should match that of the training data.
#' @param ytest Actual outcomes for test data.
#' @param prob_rule_class Threshold for classification.
#' @param verbose If TRUE, prints prediction-related messages.
#'
#' @return
#' For regression models, a list with the following components is returned:
#' 
#'   \item{y_hat}{Predictions (as posterior means) for the test observations.}
#'   \item{L1_err}{L1 error for predictions.}
#'   \item{L2_err}{L2 error for predictions.}
#'   \item{rmse}{RMSE for predictions.}
#' 
#' For classification models, a list with the following components is returned:
#' 
#'   \item{y_hat}{Class predictions for the test observations.}
#'   \item{p_hat}{Probability estimates for the test observations.}
#'   \item{confusion_matrix}{A confusion matrix for the test observations.}
#' 
#' %% ...
#'
#' @seealso
#' \code{\link{predict}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @examples
#' \dontrun{
#' #generate Friedman data
#' set.seed(11)
#' n  = 250
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##split into train and test
#' train_X = X[1 : 200, ]
#' test_X = X[201 : 250, ]
#' train_y = y[1 : 200]
#' test_y = y[201 : 250]
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(train_X, train_y)
#' 
#' #explore performance on test data
#' oos_perf = bart_predict_for_test_data(bart_machine, test_X, test_y)
#' print(oos_perf$rmse)
#' }
#' @export
bart_predict_for_test_data = function(bart_machine, Xtest, ytest, prob_rule_class = NULL, verbose = TRUE){
  assert_class(bart_machine, "bartMachine")
  assert_data_frame(Xtest)
  assert_atomic_vector(ytest)
  assert_number(prob_rule_class, lower = 0, upper = 1, null.ok = TRUE)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (bart_machine$pred_type == "regression"){ #regression list
	  ytest_hat = predict(bart_machine, Xtest, verbose = verbose)
		n = nrow(Xtest)
		L2_err = sum((ytest - ytest_hat)^2)
		
		list(
				y_hat = ytest_hat,
				L1_err = sum(abs(ytest - ytest_hat)),
				L2_err = L2_err,
				rmse = sqrt(L2_err / n),
				e = ytest - ytest_hat
		)
	} else { ##classification list
	    if (!inherits(ytest, "factor")){
			stop("ytest must be a factor.")
		}
	    if (!all(levels(ytest) %in% bart_machine$y_levels)){
			stop("New factor level not seen in training introduced. Please remove.")
		}
		
	    ptest_hat = predict(bart_machine, Xtest, type = "prob", verbose = verbose)
	    ytest_labels = ptest_hat > ifelse(is.null(prob_rule_class), bart_machine$prob_rule_class, prob_rule_class)
	    ytest_hat = labels_to_y_levels(bart_machine, ytest_labels)
    
		confusion_matrix = as.data.frame(matrix(NA, nrow = 3, ncol = 3))
		rownames(confusion_matrix) = c(paste("actual", bart_machine$y_levels), "use errors")
		colnames(confusion_matrix) = c(paste("predicted", bart_machine$y_levels), "model errors")		
		confusion_matrix[1 : 2, 1 : 2] = as.integer(table(ytest, ytest_hat)) 
		confusion_matrix[3, 1] = round(confusion_matrix[2, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1]), 3)
		confusion_matrix[3, 2] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 2] + confusion_matrix[2, 2]), 3)
		confusion_matrix[1, 3] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 1] + confusion_matrix[1, 2]), 3)
		confusion_matrix[2, 3] = round(confusion_matrix[2, 1] / (confusion_matrix[2, 1] + confusion_matrix[2, 2]), 3)
		confusion_matrix[3, 3] = round((confusion_matrix[1, 2] + confusion_matrix[2, 1]) / sum(confusion_matrix[1 : 2, 1 : 2]), 3)
		
		list(y_hat = ytest_hat, p_hat = ptest_hat, confusion_matrix = confusion_matrix)
	}
}

##get full set of samples from posterior distribution of f(x)
#' Get Full Posterior Distribution
#'
#' @description
#' Generates draws from posterior distribution of \eqn{\hat{f}(x)} for a specified set of observations.
#' @param bart_machine An object of class ``bartMachine''.
#' @param new_data A data frame containing observations at which draws from posterior distribution of \eqn{\hat{f}(x)} are to be obtained.
#' @param verbose If TRUE, prints preprocessing-related messages.
#'
#' @return
#' Returns a list with the following components:
#' %%  If it is a LIST, use
#'   \item{y_hat}{Posterior mean estimates. For regression, the estimates have the same units as the response. For classification, the estimates are probabilities.}
#'   \item{new_data}{The data frame with rows at which the posterior draws are to be generated. Column names should match that of the training data.}
#'   \item{y_hat_posterior_samples}{The full set of posterior samples of size \code{num_iterations_after_burn_in} for each observation. For regression, the estimates have the same units as the response. For classification, the estimates are probabilities.}
#' %% ...
#'
#' @seealso
#' \code{\link{calc_credible_intervals}}, \code{\link{calc_prediction_intervals}}
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
#' #get posterior distribution
#' posterior = bart_machine_get_posterior(bart_machine, X)
#' print(posterior$y_hat)
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
#' bart_machine = bartMachine(iris2[ ,1 : 4], iris2$Species)
#' 
#' #get posterior distribution
#' posterior = bart_machine_get_posterior(bart_machine, iris2[ ,1 : 4])
#' print(posterior$y_hat)
#' }
#' @export
bart_machine_get_posterior = function(bart_machine, new_data, verbose = TRUE){
  # Validate arguments
  assert_class(bart_machine, "bartMachine")
  assert_data_frame(new_data)
  assert_flag(verbose)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	prepared_data = prepare_prediction_data(bart_machine, new_data)
	new_data = prepared_data$data
	
	y_hat_posterior_samples = 
		.jcall(
			bart_machine$java_bart_machine,
			"[[D",
			"getGibbsSamplesForPrediction",
			prepared_data$array,
			as.integer(bart_machine_num_cores()),
			simplify = TRUE
		)
	
	#to get y_hat.. just take straight mean of posterior samples, alternatively, we can let java do it if we want more bells and whistles
	y_hat = rowMeans(y_hat_posterior_samples)
	
	list(y_hat = y_hat, X = new_data, y_hat_posterior_samples = y_hat_posterior_samples)
}

##compute credible intervals
#' Calculate Credible Intervals
#'
#' @description
#' Generates credible intervals for \eqn{\hat{f}(x)} for a specified set of observations.
#'
#' @details
#' This interval is the appropriate quantiles  based on the confidence level, \code{ci_conf}, of the predictions
#' for each of the Gibbs samples post-burn in.
#' @param bart_machine An object of class ``bartMachine''.
#' @param new_data A data frame containing observations at which credible intervals for \eqn{\hat{f}(x)} are to be computed.
#' @param ci_conf Confidence level for the credible intervals. The default is 95\%.
#'
#' @return
#' Returns a matrix of the lower and upper bounds of the credible intervals for each observation in \code{new_data}.
#'
#' @seealso
#' \code{\link{calc_prediction_intervals}}, \code{\link{bart_machine_get_posterior}}
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
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' 
#' #get credible interval
#' cred_int = calc_credible_intervals(bart_machine, X)
#' print(head(cred_int))
#' }
#' @export
calc_credible_intervals = function(bart_machine, new_data, ci_conf = 0.95){
  assert_class(bart_machine, "bartMachine")
  assert_data_frame(new_data)
  assert_number(ci_conf, lower = 0, upper = 1)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	prepared_data = prepare_prediction_data(bart_machine, new_data)

	.jcall(
		bart_machine$java_bart_machine,
		"[[D",
		"getCredibleIntervalsForPrediction",
		prepared_data$array,
		as.double(ci_conf),
		as.integer(bart_machine_num_cores()),
		simplify = TRUE
	)
}

##compute prediction intervals
#' Calculate Prediction Intervals
#'
#' @description
#' Generates prediction intervals for \eqn{\hat{y}} for a specified set of observations.
#'
#' @details
#' Credible intervals (see \code{\link{calc_credible_intervals}}) are the appropriate quantiles of the prediction
#' for each of the Gibbs samples post-burn in. Prediction intervals also make use of the noise estimate at each Gibbs
#' sample and hence are wider. For each Gibbs sample, we record the \eqn{\hat{y}} estimate of the response and the
#' \eqn{\hat{\sigma^2}} estimate of the noise variance. We then sample \code{normal_samples_per_gibbs_sample} times
#' from a \eqn{N(\hat{y}, \hat{\sigma^2})} random variable to simulate many possible disturbances for that Gibbs sample.
#' Then, all \code{normal_samples_per_gibbs_sample} times the number of Gibbs sample post burn-in are collected and the
#' appropriate quantiles are taken based on the confidence level, \code{pi_conf}.
#' @param bart_machine An object of class ``bartMachine''.
#' @param new_data A data frame containing observations at which prediction intervals for \eqn{\hat{y}} are to be computed.
#' @param pi_conf Confidence level for the prediction intervals. The default is 95\%.
#' @param num_samples_per_data_point The number of samples taken from the predictive distribution. The default is 1000.
#'
#' @return
#' Returns a matrix of the lower and upper bounds of the prediction intervals for each observation in \code{new_data}.
#'
#' @references
#' Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
#' with Bayesian Additive Regression Trees. Journal of Statistical
#' Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
#'
#' @seealso
#' \code{\link{calc_credible_intervals}}, \code{\link{bart_machine_get_posterior}}
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
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' 
#' #get prediction interval
#' pred_int = calc_prediction_intervals(bart_machine, X)
#' print(head(pred_int))
#' }
#' @export
calc_prediction_intervals = function(bart_machine, new_data, pi_conf = 0.95, num_samples_per_data_point = 1000){
  assert_class(bart_machine, "bartMachine")
  assert_data_frame(new_data)
  assert_number(pi_conf, lower = 0, upper = 1)
  assert_int(num_samples_per_data_point, lower = 1)

	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (bart_machine$pred_type == "classification"){
		stop("Prediction intervals are not possible for classification.")
	}
  
	prepared_data = prepare_prediction_data(bart_machine, new_data)
	.jcall(
		bart_machine$java_bart_machine,
		"[[D",
		"getPredictionIntervalsForPrediction",
		prepared_data$array,
		as.double(pi_conf),
		as.integer(num_samples_per_data_point),
		as.integer(bart_machine_num_cores()),
		simplify = TRUE
	)
}
