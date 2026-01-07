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
predict.bartMachine = function(object, new_data, type = "prob", prob_rule_class = NULL, verbose = TRUE, ...){
	check_serialization(object) #ensure the Java object exists and fire an error if not
	
	if(!(type %in% c("prob", "class"))){
		stop("For classification, type must be either \"prob\" or \"class\". ")
	}
  
	if (object$pred_type == "regression"){	
		bart_machine_get_posterior(object, new_data)$y_hat
	} else { ##classification
	    if (type == "prob"){
			if (verbose == TRUE){
				cat("predicting probabilities where \"", object$y_levels[1], "\" is considered the target level...\n", sep = "")
			}			
	    	bart_machine_get_posterior(object, new_data)$y_hat
	    } else {
	    	labels = bart_machine_get_posterior(object, new_data)$y_hat > ifelse(is.null(prob_rule_class), object$prob_rule_class, prob_rule_class)
	      	#return whatever the raw y_levels were
	      	labels_to_y_levels(object, labels)      
	    }
	}	
}

##private function
labels_to_y_levels = function(bart_machine, labels){
	factor(ifelse(labels == TRUE, bart_machine$y_levels[1], bart_machine$y_levels[2]), levels = bart_machine$y_levels)
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
bart_predict_for_test_data = function(bart_machine, Xtest, ytest, prob_rule_class = NULL){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (bart_machine$pred_type == "regression"){ #regression list
	  ytest_hat = predict(bart_machine, Xtest)
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
		
	    ptest_hat = predict(bart_machine, Xtest, type = "prob")
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
bart_machine_get_posterior = function(bart_machine, new_data){	
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (!"data.frame"%in%class(new_data)){		
		stop("\"new_data\" needs to be a data frame with the same column names as the training data.")
	}
	if (!bart_machine$use_missing_data){
		nrow_before = nrow(new_data)
		new_data = na.omit(new_data)
		if (nrow_before > nrow(new_data)){
			cat(nrow_before - nrow(new_data), "rows omitted due to missing data. Try using the missing data feature in \"build_bart_machine\" to be able to predict on all observations.\n")
		}
	}
	
	if (nrow(new_data) == 0){
		stop("No rows to predict.\n")
	}
	#pull out data objects for convenience
	java_bart_machine = bart_machine$java_bart_machine
	num_iterations_after_burn_in = bart_machine$num_iterations_after_burn_in
	n = nrow(new_data)
	
	#check for errors in data
	#
	#now process and make dummies if necessary
	new_data = pre_process_new_data(new_data, bart_machine)
	
	#check for missing data if this feature was not turned on
	if (!bart_machine$use_missing_data){
		M = matrix(0, nrow = nrow(new_data), ncol = ncol(new_data))
		for (i in 1 : nrow(new_data)){
			for (j in 1 : ncol(new_data)){
				if (is.missing(new_data[i, j])){
					M[i, j] = 1
				}
			}
		}
		if (sum(M) > 0){
			warning("missing data found in test data and bartMachine was not built with missing data feature!\n")
		}		
	}
	
	y_hat_posterior_samples = 
		.jcall(bart_machine$java_bart_machine, "[[D", "getGibbsSamplesForPrediction", .jarray(new_data, dispatch = TRUE), as.integer(bart_machine_num_cores()), simplify = TRUE)
	
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
calc_credible_intervals = function(bart_machine, new_data, ci_conf = 0.95){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	#first convert the rows to the correct dummies etc
	new_data = pre_process_new_data(new_data, bart_machine)
	n_test = nrow(new_data)
	
	ci_lower_bd = array(NA, n_test)
	ci_upper_bd = array(NA, n_test)	
	
	y_hat_posterior_samples = ##get samples
		.jcall(bart_machine$java_bart_machine, "[[D", "getGibbsSamplesForPrediction",  .jarray(new_data, dispatch = TRUE), as.integer(bart_machine_num_cores()), simplify = TRUE)
	
	#to get y_hat.. just take straight mean of posterior samples, alternatively, we can let java do it if we want more bells and whistles
	y_hat = rowMeans(y_hat_posterior_samples)
	
	for (i in 1 : n_test){		
		ci_lower_bd[i] = quantile(sort(y_hat_posterior_samples[i, ]), (1 - ci_conf) / 2)
		ci_upper_bd[i] = quantile(sort(y_hat_posterior_samples[i, ]), (1 + ci_conf) / 2)
	}
	#put them together and return
	cbind(ci_lower_bd, ci_upper_bd)
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
calc_prediction_intervals = function(bart_machine, new_data, pi_conf = 0.95, num_samples_per_data_point = 1000){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (bart_machine$pred_type == "classification"){
		stop("Prediction intervals are not possible for classification.")
	}
  
	#first convert the rows to the correct dummies etc
	new_data = pre_process_new_data(new_data, bart_machine)
	n_test = nrow(new_data)
	
	pi_lower_bd = array(NA, n_test)
	pi_upper_bd = array(NA, n_test)	
	
	y_hat_posterior_samples = 
		.jcall(bart_machine$java_bart_machine, "[[D", "getGibbsSamplesForPrediction",  .jarray(new_data, dispatch = TRUE), as.integer(bart_machine_num_cores()), simplify = TRUE)
	sigsqs = .jcall(bart_machine$java_bart_machine, "[D", "getGibbsSamplesSigsqs")
	
	
	#for each row in new_data we have to get a B x n_G matrix of draws from the normal
	
	all_prediction_samples = matrix(NA, nrow = n_test, ncol = num_samples_per_data_point)
	for (i in 1 : n_test){		
		#get all the y_hats in the posterior for this datapoint
		y_hats = y_hat_posterior_samples[i, ]
		#make a sample of gibbs samples to pull from
		n_gs = sample(1 : bart_machine$num_iterations_after_burn_in, num_samples_per_data_point, replace = TRUE)
		#now make num_samples_per_data_point draws from y_hat
		for (k in 1 : num_samples_per_data_point){
			y_hat_draw = y_hats[n_gs[k]]
			sigsq_draw = sigsqs[n_gs[k]]
			all_prediction_samples[i, k] = rnorm(1, mean = y_hat_draw, sd = sqrt(sigsq_draw))	
		}
	}
	
	for (i in 1 : n_test){		
		pi_lower_bd[i] = quantile(c(all_prediction_samples[i, ]), (1 - pi_conf) / 2) #fun fact: the "c" function is overloaded to vectorize an array
		pi_upper_bd[i] = quantile(c(all_prediction_samples[i, ]), (1 + pi_conf) / 2)
	}
	#put them together and return
	list(interval = cbind(pi_lower_bd, pi_upper_bd), all_prediction_samples = all_prediction_samples)
}
