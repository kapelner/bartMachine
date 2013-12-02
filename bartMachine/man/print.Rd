\name{print.bartMachine}
\alias{print.bartMachine}
\title{
Summarizes information about a \code{bartMachine} object.
}
\description{
This is an alias for the \code{\link{summary}} function. See description in that section.
}
\usage{
\method{print}{bartMachine}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	An object of class ``bartMachine''.
}
  \item{...}{
	Parameters that are ignored.
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
print(bart_machine)

##Also, the default print works too
bart_machine

##destroy BART model
destroy_bart_machine(bart_machine)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
