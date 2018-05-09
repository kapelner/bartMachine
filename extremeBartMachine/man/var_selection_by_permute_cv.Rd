\name{var_selection_by_permute_cv}
\alias{var_selection_by_permute_cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform Variable Selection Using Cross-validation Procedure
}
\description{
Performs variable selection by cross-validating over the three threshold-based procedures outlined in Bleich et al. (2013) and selecting the single procedure that returns the lowest cross-validation RMSE. 
}
\usage{
var_selection_by_permute_cv(bart_machine, k_folds = 5, 
num_reps_for_avg = 5, num_permute_samples = 100, 
num_trees_for_permute = 20, alpha = 0.05, num_trees_pred_cv = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{k_folds}{
Number of folds to be used in cross-validation.
}
  \item{num_reps_for_avg}{
Number of replicates to over over to for the BART model's variable inclusion proportions.
}
  \item{num_permute_samples}{
Number of permutations of the response to be made to generate the ``null'' permutation distribution.
}
  \item{num_trees_for_permute}{
Number of trees to use in the variable selection procedure. As with \cr \code{\link{investigate_var_importance}}, a small number of trees should be used to force variables to compete for entry into the model. Note that this number is used to estimate both the ``true'' and ``null'' variable inclusion proportions.
}
  \item{alpha}{
Cut-off level for the thresholds.
}
  \item{num_trees_pred_cv}{
Number of trees to use for prediction on the hold-out portion of each fold. Once variables have been selected using the training portion of each fold, a new model is built using only those variables with \code{num_trees_pred_cv} trees in the sum-of-trees model. Forecasts for the holdout sample are made using this model. A larger number of trees is recommended to exploit the full forecasting power of BART. 
}
}
\details{
See Bleich et al. (2013) for a complete description of the procedures outlined above as well as the corresponding vignette for a brief summary with examples. 
}
\value{
Returns a list with the following components:

  \item{best_method}{The name of the best variable selection procedure, as chosen via cross-validation.}
  \item{important_vars_cv}{The variables chosen by the \code{best_method} above.}
}
\references{
J Bleich, A Kapelner, ST Jensen, and EI George. Variable Selection Inference for Bayesian
Additive Regression Trees. ArXiv e-prints, 2013.

Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
with Bayesian Additive Regression Trees. Journal of Statistical
Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
}
\author{
Adam Kapelner and Justin Bleich
}
\note{
This function can have substantial run-time. 
This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{var_selection_by_permute}}, \code{\link{investigate_var_importance}}
}
\examples{
\dontrun{
#generate Friedman data
set.seed(11)
n  = 150 
p = 100 ##95 useless predictors 
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model (not actually used in variable selection)
bart_machine = bartMachine(X, y)

#variable selection via cross-validation
var_sel_cv = var_selection_by_permute_cv(bart_machine, k_folds = 3)
print(var_sel_cv$best_method)
print(var_sel_cv$important_vars_cv)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
