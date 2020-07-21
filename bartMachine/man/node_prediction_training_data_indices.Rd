\name{node_prediction_training_data_indices}
\alias{node_prediction_training_data_indices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets node predictions indices of the training data for new data.
}
\description{
This returns a binary tensor for all gibbs samples after burn-in for all trees and for all training observations.
}
\usage{
node_prediction_training_data_indices(bart_machine, new_data = NULL)
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
Returns a binary tensor indicating whether the prediction node contained a training datum or not. For each observation in new data, the size of this tensor is number of gibbs sample after burn-in
times the number of trees times the number of training data observations. This the size of the full tensor is the number of observations in the new data times the three dimensional object just explained.
}