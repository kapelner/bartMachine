# Plot the fitted Versus Actual Response

Generates a plot actual versus fitted values and corresponding credible
intervals or prediction intervals for the fitted values.

## Usage

``` r
plot_y_vs_yhat(
  bart_machine,
  Xtest = NULL,
  ytest = NULL,
  credible_intervals = FALSE,
  prediction_intervals = FALSE,
  interval_confidence_level = 0.95,
  verbose = TRUE
)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- Xtest:

  Optional argument for test data. If included, BART computes fitted
  values at the rows of `Xtest`. Else, the fitted values from the
  training data are used.

- ytest:

  Optional argument for test data. Vector of observed values
  corresponding to the rows of `Xtest` to be plotted against the
  predictions for the rows of `Xtest`.

- credible_intervals:

  If TRUE, Bayesian credible intervals are computed using the quantiles
  of the posterior distribution of \\\hat{f}(x)\\. See
  [`calc_credible_intervals`](https://kapelner.github.io/bartMachine/reference/calc_credible_intervals.md)
  for details.

- prediction_intervals:

  If TRUE, Bayesian predictive intervals are computed using the a draw
  of from \\\hat{f}(x)\\. See
  [`calc_prediction_intervals`](https://kapelner.github.io/bartMachine/reference/calc_prediction_intervals.md)
  for details.

- interval_confidence_level:

  Desired level of confidence for credible or prediction intervals.

- verbose:

  If TRUE, prints plots to the active device.

## Value

None.

## Note

This function is parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).

## See also

[`bart_machine_get_posterior`](https://kapelner.github.io/bartMachine/reference/bart_machine_get_posterior.md),
[`calc_credible_intervals`](https://kapelner.github.io/bartMachine/reference/calc_credible_intervals.md),
[`calc_prediction_intervals`](https://kapelner.github.io/bartMachine/reference/calc_prediction_intervals.md)

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
#generate linear data
set.seed(11)
n  = 500
p = 3
X = data.frame(matrix(runif(n * p), ncol = p))
y = 3*X[ ,1] + 2*X[ ,2] +X[ ,3] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y)

##generate plot
plot_y_vs_yhat(bart_machine)

#generate plot with prediction bands
plot_y_vs_yhat(bart_machine, prediction_intervals = TRUE)
} # }
```
