\name{cov_importance_test}
\alias{cov_importance_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Importance Test for Covariate(s) of Interest
}
\description{
This function tests the null hypothesis \eqn{H_0}: These covariates of interest
do not affect the response under the assumptions of the BART 
model.
}
\usage{
cov_importance_test(bart_machine, covariates = NULL, 
num_permutation_samples = 100, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
  An object of class ``bart_machine''.
}
  \item{covariates}{
	A vector of names of covariates of interest to be tested for having an effect on the response. A value of NULL
	indicates an omnibus test for all covariates having an effect on the response. If the name of a covariate is a factor,
	the entire factor will be permuted. We do not recommend entering the names of factor covariate dummies.
}
  \item{num_permutation_samples}{
	The number of times to permute the covariates of interest and create a corresponding new BART model (see details).
}
  \item{plot}{
	If \code{TRUE}, this produces a histogram of the Pseudo-Rsq's / total misclassifcation error rates from
	the \code{num_permutations} BART models created with the \code{covariates} permuted. The plot also illustrates
	the observed Pseudo-Rsq's / total misclassifcation error rate from the original training data and indicates
	the test's p-value.
}
}
\details{
To test the importance of a covariate or a set of covariates of interest on the response, this function generates 
\code{num_permutations} BART models with the covariate(s) of interest permuted (differently each time). 
On each run, a measure of fit is recorded. For regression, the metric is Pseudo-Rsq; for classification, it is
total misclassification error.\cr A 
p-value can then be generated as follows. For regression, the p-value is the number of 
permutation-sampled Pseudo-Rsq's greater than the observed Pseudo-Rsq divided by 
\code{num_permutations + 1}. For classification, the p-value is the number of permutation-sampled 
total misclassification errors less than the observed total misclassification error divided by \code{num_permutations + 1}.
}
\value{
\item{permutation_samples_of_error}{A vector which records the error metric of the BART models with the covariates permuted (see details).}
\item{observed_error_estimate}{For regression, this is the Pseudo-Rsq on the original
training data set. For classification, this is the observed total misclassification error
on the original training data set.}
\item{pval}{The approximate p-value for this test (see details). 
}
}
\references{
A Kapelner and J Bleich. bartMachine: A Powerful Tool for Machine Learning in R, arXiv preprints, 2013
}
\author{
Adam Kapelner and Justin Bleich
}
\note{
This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
##regression example

##generate Friedman data
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = build_bart_machine(X, y)

##now test if X[, 1] affects Y nonparametrically under the BART model assumptions
cov_importance_test(bart_machine, covariates = c(1))
## note the plot and the printed p-value

##destroy BART model
destroy_bart_machine(bart_machine)

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
