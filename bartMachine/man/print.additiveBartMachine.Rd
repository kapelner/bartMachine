\name{print.additiveBartMachine}
\alias{print.additiveBartMachine}
\title{
Summarizes information about a \code{additiveBartMachine} object.
}
\description{
This is an alias for the \code{\link{summary.additiveBartMachine}} function. See description in that section.
}
\usage{
\method{print}{additiveBartMachine}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	An object of class ``additiveBartMachine''.
}
  \item{...}{
	Parameters that are ignored.
}
}

\value{
None.
}

\author{
Adam Kapelner
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
additive_bart_machine = additiveBartMachine(list(3, 4, 5), X = X, y = y)

##print out details
print(additive_bart_machine)

##Also, the default print works too
additive_bart_machine
}
