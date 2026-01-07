#' Gets node predictions indices of the training data for new data.
#'
#' @description
#' This returns a binary tensor for all gibbs samples after burn-in for all trees and for all training observations.
#' @param bart_machine An object of class ``bartMachine''.
#' @param new_data Data that you wish to investigate the training sample weights. If \code{NULL}, the original training data is used.
#'
#' @return
#' Returns a binary tensor indicating whether the prediction node contained a training datum or not. For each observation in new data, the size of this tensor is number of gibbs sample after burn-in
#' times the number of trees times the number of training data observations. This the size of the full tensor is the number of observations in the new data times the three dimensional object just explained.
#'
#' @examples
#' \dontrun{
#' set.seed(11)
#' n = 50
#' X = data.frame(x1 = rnorm(n), x2 = runif(n))
#' y = X$x1 + rnorm(n)
#' bart_machine = bartMachine(X, y, flush_indices_to_save_RAM = FALSE)
#' idx = node_prediction_training_data_indices(bart_machine)
#' }
node_prediction_training_data_indices = function(bart_machine, new_data = NULL){
	if (bart_machine$flush_indices_to_save_RAM){
		stop("Node prediction training data indices cannot be computed if \"flush_indices_to_save_RAM\" was used to construct the BART model.")
	}
	
	if (is.null(new_data)){
		double_vec_null = .jcast(.jnull(), new.class = "[[D", check = FALSE, convert.array = FALSE)
		.jcall(bart_machine$java_bart_machine, "[[[[Z", "getNodePredictionTrainingIndicies", double_vec_null, simplify = TRUE)
	} else {
		.jcall(bart_machine$java_bart_machine, "[[[[Z", "getNodePredictionTrainingIndicies", .jarray(new_data, dispatch = TRUE), simplify = TRUE)
	}	
}

#' Gets Training Sample Projection / Weights
#'
#' @description
#' Returns the matrix H where yhat is approximately equal to H y where yhat is the predicted values for \code{new_data}. If \code{new_data} is unspecified, yhat will be the in-sample fits.
#' If BART was the same as OLS, H would be an orthogonal projection matrix. Here it is a projection matrix, but clearly non-orthogonal. Unfortunately, I cannot get
#' this function to work correctly because of three possible reasons (1) BART does not work by averaging tree predictions: it is a sum of trees model where each tree sees the residuals
#' via backfitting (2) the prediction in each node is a bayesian posterior draw which is close to ybar of the observations contained in the node if noise is gauged to be small and
#' (3) there are transformations of the original y variable. I believe I got close and I think I'm off by a constant multiple which is a function of the number of trees. I can
#' use regression to estimate the constant multiple and correct for it. Turn \code{regression_kludge} to \code{TRUE} for this. Note that the weights do not add up to one here.
#' The intuition is because due to the backfitting there is multiple counting. But I'm not entirely sure.
#' @param bart_machine An object of class ``bartMachine''.
#' @param new_data Data that you wish to investigate the training sample projection / weights. If \code{NULL}, the original training data is used.
#' @param regression_kludge See explanation in the description. Default is \code{FALSE}.
#'
#' @return
#' Returns a matrix of proportions with number of rows equal to the number of rows of \code{new_data} and number of columns equal to the number of rows of the original training data, n.
#'
#' @examples
#' \dontrun{
#' options(java.parameters = "-Xmx10g")
#' pacman::p_load(bartMachine, tidyverse)
#' 
#' seed = 1984
#' set.seed(seed)
#' n = 100
#' x = rnorm(n, 0, 1)
#' sigma = 0.1
#' y = x + rnorm(n, 0, sigma)
#' 
#' num_trees = 200
#' num_iterations_after_burn_in = 1000
#' bart_mod = bartMachine(data.frame(x = x), y,
#' 	flush_indices_to_save_RAM = FALSE,
#' 	num_trees = num_trees,
#' 	num_iterations_after_burn_in = num_iterations_after_burn_in,
#' 	seed = seed)
#' bart_mod
#' 
#' n_star = 100
#' x_star = rnorm(n_star)
#' y_star = as.numeric(x_star + rnorm(n_star, 0, sigma))
#' yhat_star_bart = predict(bart_mod, data.frame(x = x_star))
#' 
#' Hstar = get_projection_weights(bart_mod, data.frame(x = x_star))
#' rowSums(Hstar)
#' yhat_star_projection = as.numeric(Hstar %*% y)
#' 
#' ggplot(data.frame(
#' 	yhat_star = yhat_star_bart,
#' 	yhat_star_projection = yhat_star_projection,
#' 	y_star = y_star)) +
#'   geom_point(aes(x = yhat_star_bart, y = yhat_star_projection), col = "green") +
#'   geom_abline(slope = 1, intercept = 0)
#' 
#' Hstar = get_projection_weights(bart_mod, data.frame(x = x_star), regression_kludge = TRUE)
#' rowSums(Hstar)
#' yhat_star_projection = as.numeric(Hstar %*% y)
#' 
#' ggplot(data.frame(
#' 	yhat_star = yhat_star_bart,
#' 	yhat_star_projection = yhat_star_projection,
#' 	y_star = y_star)) +
#'   geom_point(aes(x = yhat_star_bart, y = yhat_star_projection), col = "green") +
#'   geom_abline(slope = 1, intercept = 0)
#' 
#' }
get_projection_weights = function(bart_machine, new_data = NULL, regression_kludge = FALSE){
	if (bart_machine$flush_indices_to_save_RAM){
		stop("Node prediction training data indices cannot be computed if \"flush_indices_to_save_RAM\" was used to construct the BART model.")
	}
	if (regression_kludge){
		if (is.null(new_data)){
			new_data = bart_machine$X
		}
		yhats = predict(bart_machine, new_data)
	}
	if (is.null(new_data)){
		double_vec_null = .jcast(.jnull(), new.class = "[[D", check = FALSE, convert.array = FALSE)
		weights = .jcall(bart_machine$java_bart_machine, "[[D", "getProjectionWeights", double_vec_null, simplify = TRUE)
	} else {
		weights = .jcall(bart_machine$java_bart_machine, "[[D", "getProjectionWeights", .jarray(as.matrix(new_data), dispatch = TRUE), simplify = TRUE)
	}	
	if (regression_kludge){
		yhat_star_projection = as.numeric(weights %*% bart_machine$y)
		weights * as.numeric(coef(lm(yhats ~ yhat_star_projection))[2]) #scale it back
	} else {
		weights
	}
}

BAD_FLAG_INT = -2147483647
BAD_FLAG_DOUBLE = -1.7976931348623157e+308
#' Gets Raw Node data
#'
#' @description
#' Returns a list object that contains all the information for all trees in a given Gibbs sample. Daughter nodes are nested
#' in the list structure recursively.
#' @param bart_machine An object of class ``bartMachine''.
#' @param g The gibbs sample number. It must be a natural number between 1 and the number of iterations after burn in. Default is 1.
#'
#' @return
#' Returns a list object that contains all the information for all trees in a given Gibbs sample.
#'
#' @examples
#' \dontrun{
#' options(java.parameters = "-Xmx10g")
#' pacman::p_load(bartMachine)
#' 
#' seed = 1984
#' set.seed(seed)
#' n = 100
#' x = rnorm(n, 0, 1)
#' sigma = 0.1
#' y = x + rnorm(n, 0, sigma)
#' 
#' num_trees = 200
#' num_iterations_after_burn_in = 1000
#' bart_mod = bartMachine(data.frame(x = x), y,
#' 	flush_indices_to_save_RAM = FALSE,
#' 	num_trees = num_trees,
#' 	num_iterations_after_burn_in = num_iterations_after_burn_in,
#' 	seed = seed)
#' 
#' raw_node_data = extract_raw_node_data(bart_mod)
#' 
#' }
extract_raw_node_data = function(bart_machine, g = 1){
	if (g < 1 | g > bart_machine$num_iterations_after_burn_in){
		stop("g is the gibbs sample number i.e. it must be a natural number between 1 and the number of iterations after burn in")
	}
	raw_data_java = .jcall(bart_machine$java_bart_machine, "[LbartMachine/bartMachineTreeNode;", "extractRawNodeInformation", as.integer(g - 1), simplify = TRUE)
	raw_data = list()
	for (m in 1 : bart_machine$num_trees){
		raw_data[[m]] = extract_node_data(raw_data_java[[m]])	
	}
	raw_data
}

extract_node_data = function(node_java){
	node_data = list()
	node_data$java_obj = node_java
	node_data$left_java_obj = node_java$left
	node_data$right_java_obj = node_java$right
	node_data$depth = node_java$depth
	node_data$isLeaf = node_java$isLeaf

	node_data$sendMissingDataRight = node_java$sendMissingDataRight

	node_data$n_eta = node_java$n_eta
	node_data$string_id = node_java$stringID()
	node_data$is_stump = node_java$isStump()
	node_data$string_location = node_java$stringLocation()
	
	if (node_java$splitAttributeM == BAD_FLAG_INT){
		node_data$splitAttributeM = NA
	} else {
		node_data$splitAttributeM = node_java$splitAttributeM
	}
	
	if (node_java$splitValue == BAD_FLAG_DOUBLE){
		node_data$splitValue = NA
	} else {
		node_data$splitValue = node_java$splitValue
	}
	
	if (node_java$y_pred == BAD_FLAG_DOUBLE){
		node_data$y_pred = NA
	} else {
		node_data$y_pred = node_java$y_pred
	}
	
	if (node_java$y_avg == BAD_FLAG_DOUBLE){
		node_data$y_avg = NA
	} else {
		node_data$y_avg = node_java$y_avg
	}
	
	if (node_java$posterior_var == BAD_FLAG_DOUBLE){
		node_data$posterior_var = NA
	} else {
		node_data$posterior_var = node_java$posterior_var
	}
	
	if (node_java$posterior_mean == BAD_FLAG_DOUBLE){
		node_data$posterior_mean = NA
	} else {
		node_data$posterior_mean = node_java$posterior_mean
	}
	
	if (!is.jnull(node_java$parent)){
		node_data$parent_java_obj = node_java$parent
	} else {
		node_data$parent_java_obj = NA	
	}
	
	if (!is.jnull(node_java$left)){
		node_data$left = extract_node_data(node_java$left)
	} else {
		node_data$left = NA
	}
	if (!is.jnull(node_java$right)){
		node_data$right = extract_node_data(node_java$right)
	} else {
		node_data$right = NA
	}
	node_data
}
		
		
