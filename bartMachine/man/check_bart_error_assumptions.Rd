\name{check_bart_error_assumptions}
\alias{check_bart_error_assumptions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check BART Error Assumptions
}
\description{
Diagnostic tools to assess whether the errors of the BART model for regression are normally distributed and homoskedastic, as assumed by the model. This function generates a normal quantile plot of the residuals with a Shapiro-Wilks p-value as well as a residual plot.
}
\usage{
check_bart_error_assumptions(bart_machine, hetero_plot = "yhats")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}

  \item{hetero_plot}{
If ``yhats'', the residuals are plotted against the fitted values of the response. If ``ys'', the residuals are plotted against the actual values of the response.
}
}
\details{
 
}
\value{
None.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Adam Kapelner and Justin Bleich
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot_convergence_diagnostics}}
}
\examples{
#generate Friedman data
set.seed(11)
n  = 300 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = build_bart_machine(X, y)

#check error diagnostics
check_bart_error_assumptions(bart_machine)

#destroy BART model
destroy_bart_machine(bart_machine)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
