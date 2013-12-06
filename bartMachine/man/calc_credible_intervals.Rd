\name{calc_credible_intervals}
\alias{calc_credible_intervals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Credible Intervals
}
\description{
Generates credible intervals for \eqn{\hat{f}(x)} for a specified set of observations.
}
\usage{
calc_credible_intervals(bart_machine, new_data, 
ci_conf = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{new_data}{
A data frame containing observations at which credible intervals for \eqn{\hat{f}(x)} are to be computed.
}
  \item{ci_conf}{
Confidence level for the credible intervals. The default is 95\%.
}
}
\details{
This interval is the appropriate quantiles  based on the confidence level, \code{ci_conf}, of the predictions 
for each of the Gibbs samples post-burn in.
}
\value{
Returns a matrix of the lower and upper bounds of the credible intervals for each observation in \code{new_data}.
}

\author{
Adam Kapelner and Justin Bleich
}
\note{
This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{calc_prediction_intervals}}, \code{\link{bart_machine_get_posterior}}
}
\examples{
#generate Friedman data
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = build_bart_machine(X, y)

#get credible interval
cred_int = calc_credible_intervals(bart_machine, X)
print(head(cred_int))

#destroy BART model
destroy_bart_machine(bart_machine)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
