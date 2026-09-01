# Build a BART Model

Builds a BART model for regression or classification.

## Usage

``` r
bartMachine(
  X = NULL,
  y = NULL,
  Xy = NULL,
  num_trees = 50,
  num_burn_in = 250,
  num_iterations_after_burn_in = 1000,
  alpha = 0.95,
  beta = 2,
  k = 2,
  q = 0.9,
  nu = 3,
  prob_rule_class = 0.5,
  mh_prob_steps = c(2.5, 2.5, 4)/9,
  debug_log = FALSE,
  run_in_sample = TRUE,
  s_sq_y = "mse",
  sig_sq_est = NULL,
  print_tree_illustrations = FALSE,
  cov_prior_vec = NULL,
  interaction_constraints = NULL,
  use_missing_data = FALSE,
  covariates_to_permute = NULL,
  num_rand_samps_in_library = 10000,
  use_missing_data_dummies_as_covars = FALSE,
  replace_missing_data_with_x_j_bar = FALSE,
  impute_missingness_with_rf_impute = FALSE,
  impute_missingness_with_x_j_bar_for_lm = TRUE,
  mem_cache_for_speed = TRUE,
  flush_indices_to_save_RAM = TRUE,
  serialize = FALSE,
  seed = NULL,
  use_xoshiro = FALSE,
  verbose = TRUE
)

build_bart_machine(
  X = NULL,
  y = NULL,
  Xy = NULL,
  num_trees = 50,
  num_burn_in = 250,
  num_iterations_after_burn_in = 1000,
  alpha = 0.95,
  beta = 2,
  k = 2,
  q = 0.9,
  nu = 3,
  prob_rule_class = 0.5,
  mh_prob_steps = c(2.5, 2.5, 4)/9,
  debug_log = FALSE,
  run_in_sample = TRUE,
  s_sq_y = "mse",
  sig_sq_est = NULL,
  print_tree_illustrations = FALSE,
  cov_prior_vec = NULL,
  interaction_constraints = NULL,
  use_missing_data = FALSE,
  covariates_to_permute = NULL,
  num_rand_samps_in_library = 10000,
  use_missing_data_dummies_as_covars = FALSE,
  replace_missing_data_with_x_j_bar = FALSE,
  impute_missingness_with_rf_impute = FALSE,
  impute_missingness_with_x_j_bar_for_lm = TRUE,
  mem_cache_for_speed = TRUE,
  flush_indices_to_save_RAM = TRUE,
  serialize = FALSE,
  seed = NULL,
  use_xoshiro = FALSE,
  verbose = TRUE
)
```

## Arguments

- X:

  Data frame of predictors. Factors are automatically converted to
  dummies internally.

- y:

  Vector of response variable. If `y` is `numeric` or `integer`, a BART
  model for regression is built. If `y` is a factor with two levels, a
  BART model for classification is built.

- Xy:

  A data frame of predictors and the response. The response column must
  be named “y”.

- num_trees:

  The number of trees to be grown in the sum-of-trees model.

- num_burn_in:

  Number of MCMC samples to be discarded as “burn-in”.

- num_iterations_after_burn_in:

  Number of MCMC samples to draw from the posterior distribution of
  \\\hat{f}(x)\\.

- alpha:

  Base hyperparameter in tree prior for whether a node is nonterminal or
  not.

- beta:

  Power hyperparameter in tree prior for whether a node is nonterminal
  or not.

- k:

  For regression, `k` determines the prior probability that \\E(Y\|X)\\
  is contained in the interval \\(y\_{min}, y\_{max})\\, based on a
  normal distribution. For example, when \\k=2\\, the prior probability
  is 95%. For classification, `k` determines the prior probability that
  \\E(Y\|X)\\ is between \\(-3,3)\\. Note that a larger value of `k`
  results in more shrinkage and a more conservative fit.

- q:

  Quantile of the prior on the error variance at which the data-based
  estimate is placed. Note that the larger the value of `q`, the more
  aggressive the fit as you are placing more prior weight on values
  lower than the data-based estimate. Not used for classification.

- nu:

  Degrees of freedom for the inverse \\\chi^2\\ prior. Not used for
  classification.

- prob_rule_class:

  Threshold for classification. Any observation with a conditional
  probability greater than `prob_class_rule` is assigned the “positive”
  outcome. Note that the first level of the response is treated as the
  “positive” outcome and the second is treated as the “negative”
  outcome.

- mh_prob_steps:

  Vector of prior probabilities for proposing changes to the tree
  structures: (GROW, PRUNE, CHANGE)

- debug_log:

  If TRUE, additional information about the model construction are
  printed to a file in the working directory.

- run_in_sample:

  If TRUE, in-sample statistics such as \\\hat{f}(x)\\, Pseudo-\\R^2\\,
  and RMSE are computed. Setting this to FALSE when not needed can
  decrease computation time.

- s_sq_y:

  If “mse”, a data-based estimated of the error variance is computed as
  the MSE from ordinary least squares regression. If “var”., the
  data-based estimate is computed as the variance of the response. Not
  used in classification.

- sig_sq_est:

  Pass in an estimate of the maximum sig_sq of the model. This is useful
  to cache somewhere and then pass in during cross-validation since the
  default method of estimation is a linear model. In large dimensions,
  linear model estimation is slow.

- print_tree_illustrations:

  For every Gibbs iteration, print out an illustration of the trees
  side-by-side. This is excruciatingly SLOW!

- cov_prior_vec:

  Vector assigning relative weights to how often a particular variable
  should be proposed as a candidate for a split. The vector is
  internally normalized so that the weights sum to 1. Note that the
  length of this vector must equal the length of the design matrix after
  dummification and augmentation of indicators of missingness (if used).
  To see what the dummified matrix looks like, use
  [`dummify_data`](https://kapelner.github.io/bartMachine/reference/dummify_data.md).
  See Bleich et al. (2013) for more details on when this feature is most
  appropriate.

- interaction_constraints:

  A list of vectors indicating where the vectors are sets of elements
  allowed to interact with one another. The elements in each vector
  correspond to features in the data frame `X` specified by either the
  column number as a numeric value or the column name as a string e.g.
  `list(c(1, 2), c("nox", "rm"))`. The elements of the vectors can be
  reused among components for any level of interaction complexity you
  wish. Default is `NULL` which corresponds to the vanilla modeling
  procedure where all interactions are legal. For a pure generalized
  added model, use `as.list(seq(1 : p))` where `p` is the number of
  columns in the design matrix `X`.

- use_missing_data:

  If TRUE, the missing data feature is used to automatically handle
  missing data without imputation. See Kapelner and Bleich (2013) for
  details.

- covariates_to_permute:

  Private argument for
  [`cov_importance_test`](https://kapelner.github.io/bartMachine/reference/cov_importance_test.md).
  Not needed by user.

- num_rand_samps_in_library:

  Before building a BART model, samples from the Standard Normal and
  \\\chi^2(\nu)\\ are drawn to be used in the MCMC steps. This parameter
  determines the number of samples to be taken.

- use_missing_data_dummies_as_covars:

  If TRUE, additional indicator variables for whether or not an
  observation in a particular column is missing are included. See
  Kapelner and Bleich (2013) for details.

- replace_missing_data_with_x_j_bar:

  If TRUE ,missing entries in `X` are imputed with average value or
  modal category.

- impute_missingness_with_rf_impute:

  If TRUE, missing entries are filled in using the rf.impute() function
  from the `randomForest` library.

- impute_missingness_with_x_j_bar_for_lm:

  If TRUE, when computing the data-based estimate of \\\sigma^2\\,
  missing entries are imputed with average value or modal category.

- mem_cache_for_speed:

  Speed enhancement that caches the predictors and the split values that
  are available at each node for selecting new rules. If the number of
  predictors is large, the memory requirements become large. We
  recommend keeping this on (default) and turning it off if you
  experience out-of-memory errors.

- flush_indices_to_save_RAM:

  Setting this flag to `TRUE` saves memory with the downside that you
  cannot use the functions `node_prediction_training_data_indices` nor
  `get_projection_weights`.

- serialize:

  Setting this option to `TRUE` will allow serialization of bartMachine
  objects which allows for persistence between R sessions if the object
  is saved and reloaded. Note that serialized objects can take up a
  large amount of memory. Thus, the default is `FALSE`.

- seed:

  Optional: sets the seed in both R and Java. Default is `NULL` which
  does not set the seed in R nor Java. Setting the seed enforces
  deterministic behavior only in the case when one core is used (the
  default before `set_bart_machine_num_cores() was invoked`.

- use_xoshiro:

  if TRUE, use the Xoshiro256PlusPlus random number generator; if FALSE,
  use the legacy MersenneTwister random number generator (default is
  FALSE)

- verbose:

  Prints information about progress of the algorithm to the screen.

## Value

Returns an object of class “bartMachine”. The “bartMachine” object
contains a list of the following components:

- java_bart_machine:

  A pointer to the BART Java object.

- train_data_features:

  The names of the variables used in the training data.

- training_data_features_with_missing_features.:

  The names of the variables used in the training data. If
  `use_missing_data_dummies_as_covars = TRUE`, this also includes
  dummies for any predictors that contain at least one missing entry
  (named “M\_\<feature\>”).

- y:

  The values of the response for the training data.

- y_levels:

  The levels of the response (for classification only).

- pred_type:

  Whether the model was build for regression of classification.

- model_matrix_training_data:

  The training data with factors converted to dummies.

- num_cores:

  The number of cores used to build the BART model.

- sig_sq_est:

  The data-based estimate of \\\sigma^2\\ used to create the prior on
  the error variance for the BART model.

- time_to_build:

  Total time to build the BART model.

- y_hat_train:

  The posterior means of \\\hat{f}(x)\\ for each observation. Only
  returned if `run_in_sample = TRUE`.

- residuals:

  The model residuals given by `y` - `y_hat_train`. Only returned if
  `run_in_sample = TRUE`.

- L1_err_train:

  L1 error on the training set. Only returned if `run_in_sample = TRUE`.

- L2_err_train:

  L2 error on the training set. Only returned if `run_in_sample = TRUE`.

- PseudoRsq:

  Calculated as 1 - SSE / SST where SSE is the sum of square errors in
  the training data and SST is the sample variance of the response times
  \\n-1\\. Only returned if `run_in_sample = TRUE`.

- rmse_train:

  Root mean square error on the training set. Only returned if
  `run_in_sample = TRUE`.

Additionally, the parameters passed to the function `bartMachine` are
also components of the list.

## Note

This function is parallelized by the number of cores set by
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).
Each core will create an independent MCMC chain of size  
`num_burn_in` \\+\\
`num_iterations_after_burn_in / bart_machine_num_cores`.

## References

Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning with
Bayesian Additive Regression Trees. Journal of Statistical Software,
70(4), 1-40.
[doi:10.18637/jss.v070.i04](https://doi.org/10.18637/jss.v070.i04)

HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive
Regressive Trees. The Annals of Applied Statistics, 4(1): 266–298, 2010.

A Kapelner and J Bleich. Prediction with Missing Data via Bayesian
Additive Regression Trees. Canadian Journal of Statistics, 43(2):
224-239, 2015

J Bleich, A Kapelner, ST Jensen, and EI George. Variable Selection
Inference for Bayesian Additive Regression Trees. ArXiv e-prints, 2013.

## See also

[`bartMachineCV`](https://kapelner.github.io/bartMachine/reference/bartMachineCV.md)

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
