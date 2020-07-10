\name{get_sample_weights}
\alias{get_sample_weights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets Training Sample Weights
}
\description{
Returns the proportion of time each training datum was found in a prediction node averaged over all trees and gibbs samples after burn-in.
}
\usage{
get_sample_weights(bart_machine, new_data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{new_data}{
Data that you wish to investigate the training sample weights. If \code{NULL}, the original training data is used.
}
}

\value{
Returns a matrix of proportions with number of rows equal to the number of rows of \code{new_data} and number of columns equal to the number of rows of the original training data, n.
}