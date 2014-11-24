\name{k_fold_cv}
\alias{k_fold_cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate Out-of-sample Error with K-fold Cross validation
}
\description{
Builds a BART model using a specified set of arguments to \code{build_bart_machine} and estimates the out-of-sample performance by using k-fold cross validation.
}
\usage{
k_fold_cv(X, y, k_folds = 5, folds_vec = NULL, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data frame of predictors. Factors are automatically converted to dummies interally. 
}
  \item{y}{
Vector of response variable. If \code{y} is \code{numeric} or \code{integer}, a BART model for regression is built. If \code{y} is a factor with two levels, a BART model for classification is built.
}
  \item{k_folds}{
Number of folds to cross-validate over. This argument is ignored if \code{folds_vec} is non-null.
}
  \item{k_folds}{
An integer vector of indices specifying which fold each observation belongs to. 
}
  \item{verbose}{
Prints information about progress of the algorithm to the screen. 
}
  \item{\dots}{
Additional arguments to be passed to \code{build_bart_machine}.
}
}
\details{
For each fold, a new BART model is trained (using the same set of arguments) and its performance is evaluated on the holdout piece of that fold.
}
\value{
For regression models, a list with the following components is returned:
  \item{y_hat}{Predictions for the observations computed on the fold for which the observation was omitted from the training set.}
  \item{L1_err}{Aggregate L1 error across the folds.}
  \item{L2_err}{Aggregate L1 error across the folds.}
  \item{rmse}{Aggregate RMSE across the folds.}
  \item{folds}{Vector of indices specifying which fold each observation belonged to.}

For classification models, a list with the following components is returned: 

  \item{y_hat}{Class predictions for the observations computed on the fold for which the observation was omitted from the training set.}
    \item{p_hat}{Probability estimates for the observations computed on the fold for which the observation was omitted from the training set.}
  \item{confusion_matrix}{Aggregate confusion matrix across the folds.}
    \item{misclassification_error}{Total misclassification error across the folds.}
    \item{folds}{Vector of indices specifying which fold each observation belonged to.}
}

\author{
Adam Kapelner and Justin Bleich
}
\note{
This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bartMachine}}
}
\examples{
\dontrun{
#generate Friedman data
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

#evaluate default BART on 5 folds
k_fold_val = k_fold_cv(X, y)
print(k_fold_val$rmse)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
