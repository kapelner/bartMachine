\name{bartMachine}
\alias{bartMachine}
\alias{build_bart_machine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Build a BART Model}
\description{
Builds a BART model for regression or classification.
}
\usage{
bartMachine(X = NULL, y = NULL, Xy = NULL, 
num_trees = 50, 
num_burn_in = 250, 
num_iterations_after_burn_in = 1000, 
alpha = 0.95, beta = 2, k = 2, q = 0.9, nu = 3, 
prob_rule_class = 0.5, 
mh_prob_steps = c(2.5, 2.5, 4)/9,
debug_log = FALSE, 
run_in_sample = TRUE,  
s_sq_y = "mse",
cov_prior_vec = NULL, 
use_missing_data = FALSE, 
covariates_to_permute = NULL,
num_rand_samps_in_library = 10000, 
use_missing_data_dummies_as_covars = FALSE, 
replace_missing_data_with_x_j_bar = FALSE,
impute_missingness_with_rf_impute = FALSE,
impute_missingness_with_x_j_bar_for_lm = TRUE, 
mem_cache_for_speed = TRUE,
serialize = FALSE,
verbose = TRUE)

build_bart_machine(X = NULL, y = NULL, Xy = NULL, 
num_trees = 50, 
num_burn_in = 250, 
num_iterations_after_burn_in = 1000, 
alpha = 0.95, beta = 2, k = 2, q = 0.9, nu = 3, 
prob_rule_class = 0.5, 
mh_prob_steps = c(2.5, 2.5, 4)/9,
debug_log = FALSE, 
run_in_sample = TRUE,  
s_sq_y = "mse",
cov_prior_vec = NULL, 
use_missing_data = FALSE, 
covariates_to_permute = NULL,
num_rand_samps_in_library = 10000, 
use_missing_data_dummies_as_covars = FALSE, 
replace_missing_data_with_x_j_bar = FALSE,
impute_missingness_with_rf_impute = FALSE,
impute_missingness_with_x_j_bar_for_lm = TRUE, 
mem_cache_for_speed = TRUE,
serialize = FALSE,
verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data frame of predictors. Factors are automatically converted to dummies interally. 
}
  \item{y}{
Vector of response variable. If \code{y} is \code{numeric} or \code{integer}, a BART model for regression is built. If \code{y} is a factor with two levels, a BART model for classification is built.
}
  \item{Xy}{
A data frame of predictors and the response. The response column must be named ``y''. 
}
  \item{num_trees}{
The number of trees to be grown in the sum-of-trees model.
}
  \item{num_burn_in}{
Number of MCMC samples to be discarded as ``burn-in''.
}
  \item{num_iterations_after_burn_in}{
Number of MCMC samples to draw from the posterior distribution of \eqn{\hat{f}(x)}. 
}
  \item{alpha}{
Base hyperparameter in tree prior for whether a node is nonterminal or not.
}
  \item{beta}{
Power hyperparameter in tree prior for whether a node is nonterminal or not.
}
  \item{k}{
For regression, \code{k} determines the prior probability that \eqn{E(Y|X)} is contained in the interval \eqn{(y_{min}, y_{max})}, based on a normal distribution. For example, when \eqn{k=2}, the prior probability is 95\%. For classification, \code{k} determines the prior probability that \eqn{E(Y|X)} is between \eqn{(-3,3)}. Note that a larger value of \code{k} results in more shrinkage and a more conservative fit. 
}
  \item{q}{
Quantile of the prior on the error variance at which the data-based estimate is placed. Note that the larger the value of \code{q}, the more aggressive the fit as you are placing more prior weight on values lower than the data-based estimate. Not used for classification.
}
  \item{nu}{
Degrees of freedom for the inverse \eqn{\chi^2} prior. Not used for classification.
}
  \item{prob_rule_class}{
Threshold for classification. Any observation with a conditional probability greater than \code{prob_class_rule} is assigned the ``positive'' outcome. Note that the first level of the response is treated as the ``negative'' outcome and the second is treated as the ``positive'' outcome.  
}
  \item{mh_prob_steps}{
Vector of prior probabilities for proposing changes to the tree structures: (GROW, PRUNE, CHANGE)
}
  \item{debug_log}{
If TRUE, additional information about the model construction are printed to a file in the working directory.
}
  \item{run_in_sample}{
If TRUE, in-sample statistics such as \eqn{\hat{f}(x)}, Pseudo-\eqn{R^2}, and RMSE are computed. Setting this to FALSE when not needed can decrease computation time. 
}
  \item{s_sq_y}{
If ``mse'', a data-based estimated of the error variance is computed as the MSE from ordinary least squares regression. If ``var''., the data-based estimate is computed as the variance of the response. Not used in classification. 
}
  \item{cov_prior_vec}{
Vector assigning relative weights to how often a particular variable should be proposed as a candidate for a split. The vector is internally normalized so that the weights sum to 1. Note that the length of this vector must equal the length of the design matrix after dummification and augmentation of indicators of missingness (if used). To see what the dummified matrix looks like, use \code{\link{dummify_data}}. See Bleich et al. (2013) for more details on when this feature is most appropriate. 
}
  \item{use_missing_data}{
If TRUE, the missing data feature is used to automatically handle missing data without imputation. See Kapelner and Bleich (2013) for details. 
}
  \item{covariates_to_permute}{
Private argument for \code{\link{cov_importance_test}}. Not needed by user. 
}
  \item{num_rand_samps_in_library}{
Before building a BART model, samples from the Standard Normal and \eqn{\chi^2(\nu)} are drawn to be used in the MCMC steps. This parameter determines the number of samples to be taken.  
}
  \item{use_missing_data_dummies_as_covars}{
If TRUE, additional indicator variables for whether or not an observation in a particular column is missing are included. See Kapelner and Bleich (2013) for details.
}
  \item{replace_missing_data_with_x_j_bar}{
  If TRUE ,missing entries in \code{X} are imputed with average value or modal category.

}
  \item{impute_missingness_with_rf_impute}{
If TRUE, missing entries are filled in using the rf.impute() function from the \code{randomForest} library. 
}
  \item{impute_missingness_with_x_j_bar_for_lm}{
If TRUE, when computing the data-based estimate of \eqn{\sigma^2}, missing entries are imputed with average value or modal category.
}
  \item{mem_cache_for_speed}{
Speed enhancement that caches the predictors and the split values that are available at each node for selecting new rules. If the number
of predictors is large, the memory requirements become large. We recommend keeping this on (default) and turning it off if you experience out-of-memory errors.  
}
  \item{serialize}{
Setting this option to \code{TRUE} will allow serialization of bartMachine objects which allows for persistence between
R sessions if the object is saved and reloaded. Note that serialized objects can take up a large amount of memory. 
Thus, the default is \code{FALSE}.  
}
  \item{verbose}{
Prints information about progress of the algorithm to the screen. 
}
}

\value{
Returns an object of class ``bartMachine''. The ``bartMachine'' object contains a list of the following components:

  \item{java_bart_machine}{A pointer to the BART Java object.}
  \item{train_data_features}{The names of the variables used in the training data.}
  \item{training_data_features_with_missing_features.}{The names of the variables used in the training data. If \code{use_missing_data_dummies_as_covars = TRUE}, this also includes dummies for any predictors that contain at least one missing entry (named ``M_<feature>'').}
  \item{y}{The values of the response for the training data.}
  \item{y_levels}{The levels of the response (for classification only).}
  \item{pred_type}{Whether the model was build for regression of classification.}
  \item{model_matrix_training_data}{The training data with factors converted to dummies.}
  \item{num_cores}{The number of cores used to build the BART model.}
  \item{sig_sq_est}{The data-based estimate of \eqn{\sigma^2} used to create the prior on the error variance for the BART model.}
  \item{time_to_build}{Total time to build the BART model.}
  \item{y_hat_train}{The posterior means of \eqn{\hat{f}(x)} for each observation. Only returned if \code{run_in_sample = TRUE}.}
  \item{residuals}{The model residuals given by \code{y} - \code{y_hat_train}. Only returned if \code{run_in_sample = TRUE}.}
  \item{L1_err_train}{L1 error on the training set. Only returned if \code{run_in_sample = TRUE}.}
  \item{L2_err_train}{L2 error on the training set. Only returned if \code{run_in_sample = TRUE}.}
  \item{PseudoRsq}{Calculated as 1 - SSE / SST where SSE is the sum of square errors in the training data and SST is the sample variance of the response times \eqn{n-1}. Only returned if \code{run_in_sample = TRUE}.}
  \item{rmse_train}{Root mean square error on the training set. Only returned if \code{run_in_sample = TRUE}.}
        
Additionally, the parameters passed to the function \code{bartMachine} are also components of the list. 
}
\references{
HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive Regressive Trees.
The Annals of Applied Statistics, 4(1): 266--298, 2010.

A Kapelner and J Bleich. Prediction with Missing Data via Bayesian Additive Regression
Trees. ArXiv e-prints, 2013.

J Bleich, A Kapelner, ST Jensen, and EI George. Variable Selection Inference for Bayesian
Additive Regression Trees. ArXiv e-prints, 2013.
}
\author{
Adam Kapelner and Justin Bleich
}
\note{
This function is parallelized by the number of cores set by \code{\link{set_bart_machine_num_cores}}. Each core will create an 
independent MCMC chain of size \cr
\code{num_burn_in} \eqn{+} \code{num_iterations_after_burn_in / bart_machine_num_cores}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bartMachineCV}}
}
\examples{
##regression example

##generate Friedman data
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y)
summary(bart_machine)

\dontrun{
##Build another BART regression model
bart_machine = bartMachine(X,y, num_trees = 200, num_burn_in = 500,
num_iterations_after_burn_in = 1000)

##Classification example

#get data and only use 2 factors
data(iris)
iris2 = iris[51:150,]
iris2$Species = factor(iris2$Species)

#build BART classification model
bart_machine = build_bart_machine(iris2[ ,1:4], iris2$Species)

##get estimated probabilities
phat = bart_machine$p_hat_train
##look at in-sample confusion matrix
bart_machine$confusion_matrix
}




}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line