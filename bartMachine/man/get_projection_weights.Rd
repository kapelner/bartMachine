\name{get_projection_weights}
\alias{get_projection_weights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets Training Sample Projection / Weights
}
\description{
Returns the matrix H where yhat is approximately equal to H y where yhat is the predicted values for \code{new_data}. If \code{new_data} is unspecified, yhat will be the in-sample fits.
If BART was the same as OLS, H would be an orthogonal projection matrix. Here it is a projection matrix, but clearly non-orthogonal. Unfortunately, I cannot get
this function to work correctly because of three possible reasons (1) BART does not work by averaging tree predictions: it is a sum of trees model where each tree sees the residuals
via backfitting (2) the prediction in each node is a bayesian posterior draw which is close to ybar of the observations contained in the node if noise is gauged to be small and 
(3) there are transformations of the original y variable. I believe I got close and I think I'm off by a constant multiple which is a function of the number of trees. I can
use regression to estimate the constant multiple and correct for it. Turn \code{regression_kludge} to \code{TRUE} for this. Note that the weights do not add up to one here.
The intuition is because due to the backfitting there is multiple counting. But I'm not entirely sure.
}
\usage{
get_projection_weights(bart_machine, new_data = NULL, regression_kludge = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{new_data}{
Data that you wish to investigate the training sample projection / weights. If \code{NULL}, the original training data is used.
}
  \item{regression_kludge}{
See explanation in the description. Default is \code{FALSE}.
}
}

\value{
Returns a matrix of proportions with number of rows equal to the number of rows of \code{new_data} and number of columns equal to the number of rows of the original training data, n.
}

\examples{
\dontrun{
options(java.parameters = "-Xmx10g")
pacman::p_load(bartMachine, tidyverse)

seed = 1984
set.seed(seed)
n = 100
x = rnorm(n, 0, 1)
sigma = 0.1
y = x + rnorm(n, 0, sigma)

num_trees = 200
num_iterations_after_burn_in = 1000
bart_mod = bartMachine(data.frame(x = x), y, 
	flush_indices_to_save_RAM = FALSE, 
	num_trees = num_trees, 
	num_iterations_after_burn_in = num_iterations_after_burn_in, 
	seed = seed)
bart_mod

n_star = 100
x_star = rnorm(n_star)
y_star = as.numeric(x_star + rnorm(n_star, 0, sigma))
yhat_star_bart = predict(bart_mod, data.frame(x = x_star))

Hstar = get_projection_weights(bart_mod, data.frame(x = x_star))
rowSums(Hstar)
yhat_star_projection = as.numeric(Hstar %*% y)

ggplot(data.frame(
	yhat_star = yhat_star_bart, 
	yhat_star_projection = yhat_star_projection, 
	y_star = y_star)) +
  geom_point(aes(x = yhat_star_bart, y = yhat_star_projection), col = "green") + 
  geom_abline(slope = 1, intercept = 0)

Hstar = get_projection_weights(bart_mod, data.frame(x = x_star), regression_kludge = TRUE)
rowSums(Hstar)
yhat_star_projection = as.numeric(Hstar %*% y)

ggplot(data.frame(
	yhat_star = yhat_star_bart, 
	yhat_star_projection = yhat_star_projection, 
	y_star = y_star)) +
  geom_point(aes(x = yhat_star_bart, y = yhat_star_projection), col = "green") + 
  geom_abline(slope = 1, intercept = 0)

}
}
