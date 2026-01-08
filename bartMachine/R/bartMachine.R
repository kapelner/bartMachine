#' Build a BART Model
#'
#' @description
#' Builds a BART model for regression or classification.
#' @param X Data frame of predictors. Factors are automatically converted to dummies internally.
#' @param y Vector of response variable. If \code{y} is \code{numeric} or \code{integer}, a BART model for regression is built. If \code{y} is a factor with two levels, a BART model for classification is built.
#' @param Xy A data frame of predictors and the response. The response column must be named ``y''.
#' @param num_trees The number of trees to be grown in the sum-of-trees model.
#' @param num_burn_in Number of MCMC samples to be discarded as ``burn-in''.
#' @param num_iterations_after_burn_in Number of MCMC samples to draw from the posterior distribution of \eqn{\hat{f}(x)}.
#' @param alpha Base hyperparameter in tree prior for whether a node is nonterminal or not.
#' @param beta Power hyperparameter in tree prior for whether a node is nonterminal or not.
#' @param k For regression, \code{k} determines the prior probability that \eqn{E(Y|X)} is contained in the interval \eqn{(y_{min}, y_{max})}, based on a normal distribution. For example, when \eqn{k=2}, the prior probability is 95\%. For classification, \code{k} determines the prior probability that \eqn{E(Y|X)} is between \eqn{(-3,3)}. Note that a larger value of \code{k} results in more shrinkage and a more conservative fit.
#' @param q Quantile of the prior on the error variance at which the data-based estimate is placed. Note that the larger the value of \code{q}, the more aggressive the fit as you are placing more prior weight on values lower than the data-based estimate. Not used for classification.
#' @param nu Degrees of freedom for the inverse \eqn{\chi^2} prior. Not used for classification.
#' @param prob_rule_class Threshold for classification. Any observation with a conditional probability greater than \code{prob_class_rule} is assigned the ``positive'' outcome. Note that the first level of the response is treated as the ``positive'' outcome and the second is treated as the ``negative'' outcome.
#' @param mh_prob_steps Vector of prior probabilities for proposing changes to the tree structures: (GROW, PRUNE, CHANGE)
#' @param debug_log If TRUE, additional information about the model construction are printed to a file in the working directory.
#' @param run_in_sample If TRUE, in-sample statistics such as \eqn{\hat{f}(x)}, Pseudo-\eqn{R^2}, and RMSE are computed. Setting this to FALSE when not needed can decrease computation time.
#' @param s_sq_y If ``mse'', a data-based estimated of the error variance is computed as the MSE from ordinary least squares regression. If ``var''., the data-based estimate is computed as the variance of the response. Not used in classification.
#' @param sig_sq_est Pass in an estimate of the maximum sig_sq of the model. This is useful to cache somewhere and then pass in during cross-validation since the default method of estimation is a linear model. In large dimensions, linear model estimation is slow.
#' @param print_tree_illustrations For every Gibbs iteration, print out an illustration of the trees side-by-side. This is excruciatingly SLOW!
#' @param cov_prior_vec Vector assigning relative weights to how often a particular variable should be proposed as a candidate for a split. The vector is internally normalized so that the weights sum to 1. Note that the length of this vector must equal the length of the design matrix after dummification and augmentation of indicators of missingness (if used). To see what the dummified matrix looks like, use \code{\link{dummify_data}}. See Bleich et al. (2013) for more details on when this feature is most appropriate.
#' @param interaction_constraints A list of vectors indicating where the vectors are sets of elements allowed to interact with one another. The elements in each
#'   vector correspond to features in the data frame \code{X} specified by either the column number as a numeric value or the column
#'   name as a string e.g. \code{list(c(1, 2), c("nox", "rm"))}. The elements of the vectors can be reused among components for any
#'   level of interaction complexity you wish. Default is \code{NULL} which corresponds to the vanilla modeling procedure where
#'   all interactions are legal. For a pure generalized added model, use \code{as.list(seq(1 : p))} where \code{p}
#'   is the number of columns in the design matrix \code{X}.
#' @param use_missing_data If TRUE, the missing data feature is used to automatically handle missing data without imputation. See Kapelner and Bleich (2013) for details.
#' @param covariates_to_permute Private argument for \code{\link{cov_importance_test}}. Not needed by user.
#' @param num_rand_samps_in_library Before building a BART model, samples from the Standard Normal and \eqn{\chi^2(\nu)} are drawn to be used in the MCMC steps. This parameter determines the number of samples to be taken.
#' @param use_missing_data_dummies_as_covars If TRUE, additional indicator variables for whether or not an observation in a particular column is missing are included. See Kapelner and Bleich (2013) for details.
#' @param replace_missing_data_with_x_j_bar If TRUE ,missing entries in \code{X} are imputed with average value or modal category.
#' @param impute_missingness_with_rf_impute If TRUE, missing entries are filled in using the rf.impute() function from the \code{randomForest} library.
#' @param impute_missingness_with_x_j_bar_for_lm If TRUE, when computing the data-based estimate of \eqn{\sigma^2}, missing entries are imputed with average value or modal category.
#' @param mem_cache_for_speed Speed enhancement that caches the predictors and the split values that are available at each node for selecting new rules. If the number
#'   of predictors is large, the memory requirements become large. We recommend keeping this on (default) and turning it off if you experience out-of-memory errors.
#' @param flush_indices_to_save_RAM Setting this flag to \code{TRUE} saves memory with the downside that you cannot use the functions \code{node_prediction_training_data_indices} nor \code{get_projection_weights}.
#' @param serialize Setting this option to \code{TRUE} will allow serialization of bartMachine objects which allows for persistence between
#'   R sessions if the object is saved and reloaded. Note that serialized objects can take up a large amount of memory.
#'   Thus, the default is \code{FALSE}.
#' @param seed Optional: sets the seed in both R and Java. Default is \code{NULL} which does not set the seed in R nor Java.
#'   Setting the seed enforces deterministic behavior only in the case when one core is used (the default before
#'   \code{set_bart_machine_num_cores() was invoked}.
#' @param verbose Prints information about progress of the algorithm to the screen.
#'
#' @return
#' Returns an object of class ``bartMachine''. The ``bartMachine'' object contains a list of the following components:
#' 
#'   \item{java_bart_machine}{A pointer to the BART Java object.}
#'   \item{train_data_features}{The names of the variables used in the training data.}
#'   \item{training_data_features_with_missing_features.}{The names of the variables used in the training data. If \code{use_missing_data_dummies_as_covars = TRUE}, this also includes dummies for any predictors that contain at least one missing entry (named ``M_<feature>'').}
#'   \item{y}{The values of the response for the training data.}
#'   \item{y_levels}{The levels of the response (for classification only).}
#'   \item{pred_type}{Whether the model was build for regression of classification.}
#'   \item{model_matrix_training_data}{The training data with factors converted to dummies.}
#'   \item{num_cores}{The number of cores used to build the BART model.}
#'   \item{sig_sq_est}{The data-based estimate of \eqn{\sigma^2} used to create the prior on the error variance for the BART model.}
#'   \item{time_to_build}{Total time to build the BART model.}
#'   \item{y_hat_train}{The posterior means of \eqn{\hat{f}(x)} for each observation. Only returned if \code{run_in_sample = TRUE}.}
#'   \item{residuals}{The model residuals given by \code{y} - \code{y_hat_train}. Only returned if \code{run_in_sample = TRUE}.}
#'   \item{L1_err_train}{L1 error on the training set. Only returned if \code{run_in_sample = TRUE}.}
#'   \item{L2_err_train}{L2 error on the training set. Only returned if \code{run_in_sample = TRUE}.}
#'   \item{PseudoRsq}{Calculated as 1 - SSE / SST where SSE is the sum of square errors in the training data and SST is the sample variance of the response times \eqn{n-1}. Only returned if \code{run_in_sample = TRUE}.}
#'   \item{rmse_train}{Root mean square error on the training set. Only returned if \code{run_in_sample = TRUE}.}
#' 
#' Additionally, the parameters passed to the function \code{bartMachine} are also components of the list.
#'
#' @references
#' Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
#' with Bayesian Additive Regression Trees. Journal of Statistical
#' Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
#' 
#' HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive Regressive Trees.
#' The Annals of Applied Statistics, 4(1): 266--298, 2010.
#' 
#' A Kapelner and J Bleich. Prediction with Missing Data via Bayesian Additive Regression
#' Trees. Canadian Journal of Statistics, 43(2): 224-239, 2015
#' 
#' J Bleich, A Kapelner, ST Jensen, and EI George. Variable Selection Inference for Bayesian
#' Additive Regression Trees. ArXiv e-prints, 2013.
#'
#' @seealso
#' \code{\link{bartMachineCV}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' This function is parallelized by the number of cores set by \code{\link{set_bart_machine_num_cores}}. Each core will create an
#' independent MCMC chain of size \cr
#' \code{num_burn_in} \eqn{+} \code{num_iterations_after_burn_in / bart_machine_num_cores}.
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
#' summary(bart_machine)
#' 
#' ##Build another BART regression model
#' bart_machine = bartMachine(X,y, num_trees = 200, num_burn_in = 500,
#' num_iterations_after_burn_in = 1000)
#' 
#' ##Classification example
#' 
#' #get data and only use 2 factors
#' data(iris)
#' iris2 = iris[51:150,]
#' iris2$Species = factor(iris2$Species)
#' 
#' #build BART classification model
#' bart_machine = build_bart_machine(iris2[ ,1:4], iris2$Species)
#' 
#' ##get estimated probabilities
#' phat = bart_machine$p_hat_train
#' ##look at in-sample confusion matrix
#' bart_machine$confusion_matrix
#' }
#'
#' @aliases build_bart_machine
#' @export
bartMachine = function(
	X = NULL, 
	y = NULL, 
	Xy = NULL, 
    num_trees = 50, #found many times to not get better after this value... so let it be the default, it's faster too 
    num_burn_in = 250, 
    num_iterations_after_burn_in = 1000, 
    alpha = 0.95,
    beta = 2,
    k = 2,
    q = 0.9,
    nu = 3.0,
    prob_rule_class = 0.5,
    mh_prob_steps = c(2.5, 2.5, 4) / 9, #only the first two matter
    debug_log = FALSE,
    run_in_sample = TRUE,
    s_sq_y = "mse", # "mse" or "var"
	sig_sq_est = NULL,
    print_tree_illustrations = FALSE, #POWER USERS ONLY
    cov_prior_vec = NULL,
	interaction_constraints = NULL,
    use_missing_data = FALSE,
    covariates_to_permute = NULL, #PRIVATE
    num_rand_samps_in_library = 10000, #give the user the option to make a bigger library of random samples of normals and inv-gammas
    use_missing_data_dummies_as_covars = FALSE,
    replace_missing_data_with_x_j_bar = FALSE,
    impute_missingness_with_rf_impute = FALSE,
    impute_missingness_with_x_j_bar_for_lm = TRUE,
    mem_cache_for_speed = TRUE,
	flush_indices_to_save_RAM = TRUE,
	serialize = FALSE,
	seed = NULL,
    verbose = TRUE
    	){
  # Validate arguments
  assert_data_frame(X, null.ok = TRUE)
  if (!is.null(y)) assert_atomic_vector(y)
  assert_data_frame(Xy, null.ok = TRUE)
  assert_int(num_trees, lower = 1)
  assert_int(num_burn_in, lower = 0)
  assert_int(num_iterations_after_burn_in, lower = 1)
  assert_number(alpha, lower = 0, upper = 1)
  assert_number(beta, lower = 0)
  assert_number(k, lower = .Machine$double.eps)
  assert_number(q, lower = .Machine$double.eps, upper = 1 - .Machine$double.eps)
  assert_number(nu, lower = .Machine$double.eps)
  assert_number(prob_rule_class, lower = 0, upper = 1)
  assert_numeric(mh_prob_steps, len = 3, lower = .Machine$double.eps)
  assert_flag(debug_log)
  assert_flag(run_in_sample)
  assert_choice(s_sq_y, c("mse", "var"))
  assert_number(sig_sq_est, lower = .Machine$double.eps, null.ok = TRUE)
  assert_flag(print_tree_illustrations)
  assert_numeric(cov_prior_vec, lower = .Machine$double.eps, null.ok = TRUE)
  assert_list(interaction_constraints, null.ok = TRUE)
  assert_flag(use_missing_data)
  assert_int(num_rand_samps_in_library, lower = 1)
  assert_flag(use_missing_data_dummies_as_covars)
  assert_flag(replace_missing_data_with_x_j_bar)
  assert_flag(impute_missingness_with_rf_impute)
  assert_flag(impute_missingness_with_x_j_bar_for_lm)
  assert_flag(mem_cache_for_speed)
  assert_flag(flush_indices_to_save_RAM)
  assert_flag(serialize)
  assert_int(seed, null.ok = TRUE)
  assert_flag(verbose)

    	build_bart_machine(    
			X = X, 
			y = y, 
			Xy = Xy, 
			num_trees = num_trees,
			num_burn_in = num_burn_in, 
			num_iterations_after_burn_in = num_iterations_after_burn_in, 
			alpha = alpha,
			beta = beta,
			k = k,
			q = q,
			nu = nu,
			prob_rule_class = prob_rule_class,
			mh_prob_steps = mh_prob_steps,
			debug_log = debug_log,
			run_in_sample = run_in_sample,
			s_sq_y = s_sq_y,
			sig_sq_est = sig_sq_est,
			print_tree_illustrations = print_tree_illustrations, 
			cov_prior_vec = cov_prior_vec,
			interaction_constraints = interaction_constraints,
			use_missing_data = use_missing_data,
			covariates_to_permute = covariates_to_permute, 
			num_rand_samps_in_library = num_rand_samps_in_library, #give the user the option to make a bigger library of random samples of normals and inv-gammas
			use_missing_data_dummies_as_covars = use_missing_data_dummies_as_covars,
			replace_missing_data_with_x_j_bar = replace_missing_data_with_x_j_bar,
			impute_missingness_with_rf_impute = impute_missingness_with_rf_impute,
			impute_missingness_with_x_j_bar_for_lm = impute_missingness_with_x_j_bar_for_lm,
			mem_cache_for_speed = mem_cache_for_speed,
			flush_indices_to_save_RAM = flush_indices_to_save_RAM,
			serialize = serialize,
			seed = seed,
			verbose = verbose			
	   )
}


#' Build BART-CV
#'
#' @description
#' Builds a BART-CV model by cross-validating over a grid of hyperparameter choices.
#' @param X Data frame of predictors. Factors are automatically converted to dummies internally.
#' @param y Vector of response variable. If \code{y} is \code{numeric} or \code{integer}, a BART model for regression is built. If \code{y} is a factor with two levels, a BART model for classification is built.
#' @param Xy A data frame of predictors and the response. The response column must be named ``y''.
#' @param num_tree_cvs Vector of sizes for the sum-of-trees models to cross-validate over.
#' @param k_cvs Vector of choices for the hyperparameter \code{k} to cross-validate over.
#' @param nu_q_cvs Only for regression. List of vectors containing (\code{nu}, \code{q}) ordered pair choices to cross-validate over. If \code{NULL}, then it defaults to the three values \code{list(c(3, 0.9), c(3, 0.99), c(10, 0.75))}.
#' @param k_folds Number of folds for cross-validation
#' @param folds_vec An integer vector of indices specifying which fold each observation belongs to.
#' @param verbose Prints information about progress of the algorithm to the screen.
#' @param \dots Additional arguments to be passed to \code{bartMachine}.
#'
#' @return
#' Returns an object of class ``bartMachine'' with the set of hyperparameters chosen via cross-validation. We also return a matrix ``cv_stats''
#' which contains the out-of-sample RMSE for each hyperparameter set tried and ``folds'' which gives the fold in which each observation fell across the k-folds.
#'
#' @references
#' Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
#' with Bayesian Additive Regression Trees. Journal of Statistical
#' Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
#'
#' @seealso
#' \code{\link{bartMachine}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' This function may require significant run-time.
#' This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}} via calling \code{\link{bartMachine}}.
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
#' bart_machine_cv = bartMachineCV(X, y)
#' 
#' #information about cross-validated model
#' summary(bart_machine_cv)
#' }
#'
#' @aliases build_bart_machine_cv
#' @export
bartMachineCV = function(X = NULL, y = NULL, Xy = NULL, 
   num_tree_cvs = c(50, 200),
   k_cvs = c(2, 3, 5),
   nu_q_cvs = NULL,
   k_folds = 5, 
   folds_vec = NULL,    
   verbose = FALSE, ...){
  # Validate arguments
  assert_data_frame(X, null.ok = TRUE)
  if (!is.null(y)) assert_atomic_vector(y)
  assert_data_frame(Xy, null.ok = TRUE)
  assert_integerish(num_tree_cvs, lower = 1, min.len = 1)
  assert_numeric(k_cvs, lower = 0, min.len = 1)
  assert_list(nu_q_cvs, null.ok = TRUE)
  assert_count(k_folds, positive = TRUE)
  assert_integerish(folds_vec, null.ok = TRUE)
  assert_flag(verbose)

  	build_bart_machine_cv(X, y, Xy, 
	   num_tree_cvs,
	   k_cvs,
	   nu_q_cvs,
	   k_folds, 
	   folds_vec, 
	   verbose = verbose,
	   ...)
}
