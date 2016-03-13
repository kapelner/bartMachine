\name{bartMachineArr}
\alias{bartMachineArr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an array of BART models for the same data.
}
\description{
If BART creates models that are variable,
running many on the same dataset and averaging is a good strategy. 
This function is a convenience method for this procedure.
}
\usage{
bartMachineArr(bart_machine, R = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
	 An object of class ``bartMachine''.
}
  \item{R}{
	The number of replicated BART models in the array.
}
}

\value{
A \code{bartMachineArr} object which is just a list of the \code{R} bartMachine models.
}

\author{
Adam Kapelner
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#Regression example
\dontrun{
#generate Friedman data
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y)
bart_machine_arr = bartMachineArr(bart_machine)

#Classification example
data(iris)
iris2 = iris[51 : 150, ] #do not include the third type of flower for this example
iris2$Species = factor(iris2$Species)  
bart_machine = bartMachine(iris2[ ,1:4], iris2$Species)
bart_machine_arr = bartMachineArr(bart_machine)
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
