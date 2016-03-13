\name{predict.bartMachineArr}
\alias{predict.bartMachineArr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a prediction on data using a BART array object
}
\description{
Makes a prediction on new data given an array of fitted BART model for 
regression or classification. If BART creates models that are variable,
running many and averaging is a good strategy. It is well known that the
Gibbs sampler gets locked into local modes at times. This is a way
to average over many chains.
}
\usage{
predict(object, new_data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	 An object of class ``bartMachineArr''.
}
  \item{new_data}{
	A data frame where each row is an observation to predict. The column names
	should be the same as the column names of the training data.
}
  \item{...}{
	Not supported. Note that parameters \code{type} and \code{prob_rule_class} for 
	\code{\link{predict.bartMachine}} are not supported.
}
}

\value{
If regression, a numeric vector of \code{y_hat}, the best guess as to the response. If classification and \code{type = ``prob''}, 
a numeric vector of \code{p_hat}, the best guess as to the probability of the response class being  the ''positive'' class. If classification and 
\code{type = ''class''}, a character vector of the best guess of the response's class labels. 
}

\author{
Adam Kapelner
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{predict.bartMachine}}
}
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

##make predictions on the training data
y_hat = predict(bart_machine_arr, X)

#Classification example
data(iris)
iris2 = iris[51 : 150, ] #do not include the third type of flower for this example
iris2$Species = factor(iris2$Species)  
bart_machine = bartMachine(iris2[ ,1:4], iris2$Species)
bart_machine_arr = bartMachineArr(bart_machine)

##make probability predictions on the training data
p_hat = predict(bart_machine_arr, iris2[ ,1:4])
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
