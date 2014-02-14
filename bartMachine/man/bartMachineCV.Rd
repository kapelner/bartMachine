\name{bartMachineCV}
\alias{bartMachineCV}
\alias{build_bart_machine_cv}

\title{
Build BART-CV
}
\description{
Builds a BART-CV model by cross-validating over a grid of hyperparameter choices.
}
\usage{
bartMachineCV(X = NULL, y = NULL, Xy = NULL, 
num_tree_cvs = c(50, 200), k_cvs = c(2, 3, 5), 
nu_q_cvs = list(c(3, 0.9), c(3, 0.99), c(10, 0.75)), k_folds = 5, verbose = FALSE, ...)

build_bart_machine_cv(X = NULL, y = NULL, Xy = NULL, 
num_tree_cvs = c(50, 200), k_cvs = c(2, 3, 5), 
nu_q_cvs = list(c(3, 0.9), c(3, 0.99), c(10, 0.75)), k_folds = 5, verbose = FALSE, ...)
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
  \item{num_tree_cvs}{
Vector of sizes for the sum-of-trees models to cross-validate over.
}
  \item{k_cvs}{
Vector of choices for the hyperparameter \code{k} to cross-validate over.
}
  \item{nu_q_cvs}{
List of vectors containing (\code{nu}, \code{q}) ordered pair choices to cross-validate over.
}
  \item{k_folds}{
Number of folds for cross-validation
}
  \item{verbose}{
Prints information about progress of the algorithm to the screen. 
}
  \item{\dots}{
Additional arguments to be passed to \code{bartMachine}.
}
}

\value{
Returns an object of class ``bartMachine'' with the set of hyperparameters chosen via cross-validation. We also return a matrix ``cv_stats''
which contains the out-of-sample RMSE for each hyperparameter set tried.
}
\references{
HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive Regressive Trees.
The Annals of Applied Statistics, 4(1): 266--298, 2010.
}
\author{
Adam Kapelner and Justin Bleich
}
\note{
This function may require significant run-time.
This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}} via calling \code{\link{bartMachine}}.
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

##build BART regression model
bart_machine_cv = bartMachineCV(X, y)

#information about cross-validated model
summary(bart_machine_cv)

#destroy BART model
destroy_bart_machine(bart_machine_cv)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
