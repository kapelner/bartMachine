\name{destroy_bart_machine}
\alias{destroy_bart_machine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Destroy BART Model in Java
}
\description{
Destroys a BART model by setting all Java pointers to null.
}
\usage{
destroy_bart_machine(bart_machine)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
%%     ~~Describe \code{bart_machine} here~~
}
}
\details{
Removing a ``bart_machine'' object from \code{R} does not free heap space from Java. Since BART objects can consume a large amount of RAM, it is important to remove these objects by calling this function if they are no longer needed or many BART objects are being created.
}
\value{
None.
}

\author{
Adam Kapelner and Justin Bleich
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##Generate Friedman Data
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model and destroy it 
bart_machine = bartMachine(X, y)

##should be called when object is no longer needed 
##and before potentially removing the object from R
destroy_bart_machine(bart_machine) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
