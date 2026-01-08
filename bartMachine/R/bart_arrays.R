
#' Create an array of BART models for the same data.
#'
#' @description
#' If BART creates models that are variable,
#' running many on the same dataset and averaging is a good strategy.
#' This function is a convenience method for this procedure.
#' @param bart_machine An object of class ``bartMachine''.
#' @param R The number of replicated BART models in the array.
#'
#' @return
#' A \code{bartMachineArr} object which is just a list of the \code{R} bartMachine models.
#'
#' @author
#' Adam Kapelner
#'
#' @examples
#' \dontrun{
#' #Regression example
#' #generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' bart_machine_arr = bartMachineArr(bart_machine)
#' 
#' #Classification example
#' data(iris)
#' iris2 = iris[51 : 150, ] #do not include the third type of flower for this example
#' iris2$Species = factor(iris2$Species)
#' bart_machine = bartMachine(iris2[ ,1:4], iris2$Species)
#' bart_machine_arr = bartMachineArr(bart_machine)
#' }
#' @export
bartMachineArr = function(bart_machine, R = 10){
  assert_class(bart_machine, "bartMachine")
  assert_count(R, positive = TRUE)

	arr = list()
	arr[[1]] = bart_machine
	for (i in 2 : R){
		arr[[i]] = bart_machine_duplicate(bart_machine)
	}
	class(arr) = "bartMarchineArr"
	arr
}

#' Make a prediction on data using a BART array object
#'
#' @description
#' Makes a prediction on new data given an array of fitted BART model for
#' regression or classification. If BART creates models that are variable,
#' running many and averaging is a good strategy. It is well known that the
#' Gibbs sampler gets locked into local modes at times. This is a way
#' to average over many chains.
#' @param object An object of class ``bartMachineArr''.
#' @param new_data A data frame where each row is an observation to predict. The column names
#'   should be the same as the column names of the training data.
#' @param ... Not supported. Note that parameters \code{type} and \code{prob_rule_class} for
#'   \code{\link{predict.bartMachine}} are not supported.
#'
#' @return
#' If regression, a numeric vector of \code{y_hat}, the best guess as to the response. If classification and \code{type = ``prob''},
#' a numeric vector of \code{p_hat}, the best guess as to the probability of the response class being  the ''positive'' class. If classification and
#' \code{type = ''class''}, a character vector of the best guess of the response's class labels.
#'
#' @seealso
#' \code{\link{predict.bartMachine}}
#'
#' @author
#' Adam Kapelner
#'
#' @examples
#' \dontrun{
#' #Regression example
#' #generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 5
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y)
#' bart_machine_arr = bartMachineArr(bart_machine)
#' 
#' ##make predictions on the training data
#' y_hat = predict(bart_machine_arr, X)
#' 
#' #Classification example
#' data(iris)
#' iris2 = iris[51 : 150, ] #do not include the third type of flower for this example
#' iris2$Species = factor(iris2$Species)
#' bart_machine = bartMachine(iris2[ ,1:4], iris2$Species)
#' bart_machine_arr = bartMachineArr(bart_machine)
#' 
#' ##make probability predictions on the training data
#' p_hat = predict_bartMachineArr(bart_machine_arr, iris2[ ,1:4])
#' }
#' @export
predict_bartMachineArr = function(object, new_data, ...){
  assert_class(object, "bartMarchineArr")
  assert_data_frame(new_data)

	R = length(object)
	n_star = nrow(new_data)
	predicts = matrix(NA, nrow = n_star, ncol = R)
	for (r in 1 : R){
		predicts[, r] = predict(object[[r]], new_data, ...)
	}
	rowMeans(predicts)
}
