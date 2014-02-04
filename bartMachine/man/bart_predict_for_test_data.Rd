\name{bart_predict_for_test_data}
\alias{bart_predict_for_test_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict for Test Data with Known Outcomes
}
\description{
Utility wrapper function for computing out-of-sample metrics for a BART model when the test set outcomes are known.
}
\usage{
bart_predict_for_test_data(bart_machine, Xtest, ytest)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{Xtest}{
Data frame for test data containing rows at which predictions are to be made. Colnames should match that of the training data.
}
  \item{ytest}{
Actual outcomes for test data.
}
}

\value{
For regression models, a list with the following components is returned:

  \item{y_hat}{Predictions (as posterior means) for the test observations.}
  \item{L1_err}{L1 error for predictions.}
  \item{L2_err}{L2 error for predictions.}
  \item{rmse}{RMSE for predictions.}

For classification models, a list with the following components is returned: 

  \item{y_hat}{Class predictions for the test observations.}
  \item{confusion_matrix}{A confusion matrix for the test observations.}

%% ...
}
\author{
Adam Kapelner and Justin Bleich
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{predict}}
}
\examples{
#generate Friedman data
set.seed(11)
n  = 250 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##split into train and test
train_X = X[1 : 200, ]
test_X = X[201 : 250, ]
train_y = y[1 : 200]
test_y = y[201 : 250]

##build BART regression model
bart_machine = bartMachine(train_X, train_y)

#explore performance on test data
oos_perf = bart_predict_for_test_data(bart_machine, test_X, test_y)
print(oos_perf$rmse)

#destroy BART model
destroy_bart_machine(bart_machine)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
