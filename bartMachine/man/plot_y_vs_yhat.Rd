\name{plot_y_vs_yhat}
\alias{plot_y_vs_yhat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the fitted Versus Actual Response
}
\description{
Generates a plot actual versus fitted values and corresponding credible intervals or prediction intervals for the fitted values.
}
\usage{
plot_y_vs_yhat(bart_machine, Xtest = NULL, ytest = NULL, 
credible_intervals = FALSE, prediction_intervals = FALSE, 
interval_confidence_level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
  An object of class ``bartMachine''. 
}
  \item{Xtest}{
Optional argument for test data. If included, BART computes fitted values at the rows of \code{Xtest}. Else, the fitted values from the training data are used.
}
  \item{ytest}{
Optional argument for test data. Vector of observed values corresponding to the rows of \code{Xtest} to be plotted against the predictions for the rows of \code{Xtest}.
}
  \item{credible_intervals}{
If TRUE, Bayesian credible intervals are computed using the quantiles of the posterior distribution of \eqn{\hat{f}(x)}. See \code{\link{calc_credible_intervals}} for details.
}
  \item{prediction_intervals}{
If TRUE, Bayesian predictive intervals are computed using the a draw of from \eqn{\hat{f}(x)}. See \code{\link{calc_prediction_intervals}} for details.
}
  \item{interval_confidence_level}{
Desired level of confidence for credible or prediction intervals. 
}
}
\details{

}
\value{
None.
}
\references{

}
\author{
Adam Kapelner and Justin Bleich
}
\note{
This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bart_machine_get_posterior}}, \code{\link{calc_credible_intervals}}, \code{\link{calc_prediction_intervals}}
}

\examples{
\dontrun{
#generate linear data
set.seed(11)
n  = 500 
p = 3
X = data.frame(matrix(runif(n * p), ncol = p))
y = 3*X[ ,1] + 2*X[ ,2] +X[ ,3] + rnorm(n)

##build BART regression model
bart_machine = build_bart_machine(X, y)

##generate plot
plot_y_vs_yhat(bart_machine)

#generate plot with prediction bands
plot_y_vs_yhat(bart_machine, prediction_intervals = TRUE)

#destroy BART model
destroy_bart_machine(bart_machine)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
