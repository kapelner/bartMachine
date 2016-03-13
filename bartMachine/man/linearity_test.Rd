\name{linearity_test}
\alias{linearity_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test of Linearity
}
\description{
Test to investigate \eqn{H_0:} the functional relationship between the response and the
regressors is linear. We fit a linear model and then test if the residuals are a function
of the regressors using the 
}
\usage{
linearity_test(lin_mod = NULL, X = NULL, y = NULL, 
	num_permutation_samples = 100, plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lin_mod}{
A linear model you can pass in if you do not want to use the default which is \code{lm(y ~ X)}. Default is \code{NULL} which should be used if you pass in \code{X} and \code{y}. 
}
  \item{X}{
Data frame of predictors. Factors are automatically converted to dummies internally. Default is \code{NULL} which should be used if you pass in \code{lin_mode}.
}
  \item{y}{
Vector of response variable. If \code{y} is \code{numeric} or \code{integer}, a BART model for regression is built. If \code{y} is a factor with two levels, a BART model for classification is built.
Default is \code{NULL} which should be used if you pass in \code{lin_mode}.
}
  \item{num_permutation_samples}{
	This function relies on \code{\link{cov_importance_test}} (see documentation there for details).
}
  \item{plot}{
	This function relies on \code{\link{cov_importance_test}} (see documentation there for details).
}
  \item{...}{
	Additional parameters to be passed to \code{bartMachine}, the model constructed on the residuals of the linear model.
}
}
\value{
\item{permutation_samples_of_error}{	This function relies on \code{\link{cov_importance_test}} (see documentation there for details).
}
\item{observed_error_estimate}{	This function relies on \code{\link{cov_importance_test}} (see documentation there for details).
}
\item{pval}{The approximate p-value for this test. See the documentation at \code{\link{cov_importance_test}}.
}
}
\references{
A Kapelner and J Bleich. bartMachine: Machine Learning with Bayesian Additive Regression Trees, arXiv preprints, 2013
}
\author{
Adam Kapelner
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{cov_importance_test}}
}

\examples{
\dontrun{
##regression example

##generate Friedman data i.e. a nonlinear response model
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##now test if there is a nonlinear relationship between X1, ..., X5 and y.
linearity_test(X = X, y = y)
## note the plot and the printed p-value.. should be approx 0

#generate a linear response model
y = 1 * X[ ,1] + 3 * X[,2] + 5 * X[,3] + 7 * X[ ,4] + 9 * X[,5] + rnorm(n)
linearity_test(X = X, y = y)
## note the plot and the printed p-value.. should be > 0.05

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
