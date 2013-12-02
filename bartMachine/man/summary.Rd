\name{summary.bartMachine}
\alias{summary.bartMachine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizes information about a \code{bartMachine} object.
}
\description{
Provides a quick summary of the BART model.
}
\usage{
\method{summary}{bartMachine}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	An object of class ``bartMachine''.
}
  \item{...}{
	Parameters that are ignored.
}
}
\details{
Gives the version number of the \code{bartMachine} package used to build this \code{bartMachine} object and if the object
models either ``regression'' or ``classification.'' Gives the amount of training data and the dimension of feature space. Prints 
the amount of time it took to build the model, how many processor cores were used to during its construction, as well as the 
number of burn-in and posterior Gibbs samples were used. 

If the model is for regression, it prints the estimate of \eqn{\sigma^2} before the model was constructed as well as after so 
the user can inspect how much variance was explained.

If the model was built using the \code{run_in_sample = TRUE} parameter in \code{\link{build_bart_machine}} and is for regression, the summary L1,
L2, rmse, Pseudo-\eqn{R^2} are printed as well as the p-value for the tests of normality and zero-mean noise. If the model is for classification, a confusion matrix is printed.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#Regression example

#generate Friedman data
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = build_bart_machine(X, y)

##print out details
summary(bart_machine)

##Also, the default print works too
bart_machine

##destroy BART model
destroy_bart_machine(bart_machine)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
