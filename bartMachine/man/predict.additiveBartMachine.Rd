\name{predict.additiveBartMachine}
\alias{predict.additiveBartMachine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a prediction on data using a Additive BART object
}
\description{
Makes a prediction on new data given a fitted Additive BART model for regression or classification.
}
\usage{
\method{predict}{additiveBartMachine}(object, new_data, type = "prob", prob_rule_class = NULL, verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	 An object of class ``additiveBartMachine''.
}
  \item{new_data}{
	A data frame where each row is an observation to predict. The column names
	should be the same as the column names of the training data.
}
  \item{type}{
	 Only relevant if the bartMachine model is classification. The type can be ``prob'' which will
	 return the estimate of \eqn{P(Y = 1)}(the ``positive'' class) or ``class'' which will return the best guess as to the
	 class of the object, in the original label, based on if the probability estimate is greater 
	 than \code{prob_rule_class}. Default is ``prob.''
}
  \item{prob_rule_class}{
	The rule to determine when the class estimate is \eqn{Y = 1} (the ``positive'' class) based on the probability estimate. This
	defaults to what was originally specified in the \code{bart_machine} object. 
}
  \item{verbose}{
	Prints out prediction-related messages. Currently in use only for probability predictions to let the user know which class
	is being predicted. Default is \code{TRUE}.
}
  \item{...}{
	Parameters that are ignored.
}
}

\value{
If regression, a numeric vector of \code{y_hat}, the best guess as to the response. If classification and \code{type = ``prob''}, 
a numeric vector of \code{p_hat}, the best guess as to the probability of the response class being  the ''positive'' class. If classification and 
\code{type = ''class''}, a character vector of the best guess of the response's class labels. 
}

\author{
Adam Kapelner and Justin Bleich
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bart_predict_for_test_data}}
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
additive_bart_machine = additiveBartMachine(list(3, 4, 5), X = X, y = y)

##make predictions on the training data
y_hat = predict(additive_bart_machine, X)
}
}
