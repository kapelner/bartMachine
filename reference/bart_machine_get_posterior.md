# Get Full Posterior Distribution

Generates draws from posterior distribution of \\\hat{f}(x)\\ for a
specified set of observations.

## Usage

``` r
bart_machine_get_posterior(bart_machine, new_data, verbose = TRUE)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- new_data:

  A data frame containing observations at which draws from posterior
  distribution of \\\hat{f}(x)\\ are to be obtained.

- verbose:

  If TRUE, prints preprocessing-related messages.

## Value

Returns a list with the following components:

- y_hat:

  Posterior mean estimates. For regression, the estimates have the same
  units as the response. For classification, the estimates are
  probabilities.

- new_data:

  The data frame with rows at which the posterior draws are to be
  generated. Column names should match that of the training data.

- y_hat_posterior_samples:

  The full set of posterior samples of size
  `num_iterations_after_burn_in` for each observation. For regression,
  the estimates have the same units as the response. For classification,
  the estimates are probabilities.

## Note

This function is parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).

## See also

[`calc_credible_intervals`](https://kapelner.github.io/bartMachine/reference/calc_credible_intervals.md),
[`calc_prediction_intervals`](https://kapelner.github.io/bartMachine/reference/calc_prediction_intervals.md)

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
#Regression example

#generate Friedman data
set.seed(11)
n  = 200
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y)

#get posterior distribution
posterior = bart_machine_get_posterior(bart_machine, X)
print(posterior$y_hat)


#Classification example

#get data and only use 2 factors
data(iris)
iris2 = iris[51:150,]
iris2$Species = factor(iris2$Species)

#build BART classification model
bart_machine = bartMachine(iris2[ ,1 : 4], iris2$Species)

#get posterior distribution
posterior = bart_machine_get_posterior(bart_machine, iris2[ ,1 : 4])
print(posterior$y_hat)
} # }
```
