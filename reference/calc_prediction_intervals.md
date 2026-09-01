# Calculate Prediction Intervals

Generates prediction intervals for \\\hat{y}\\ for a specified set of
observations.

## Usage

``` r
calc_prediction_intervals(
  bart_machine,
  new_data,
  pi_conf = 0.95,
  num_samples_per_data_point = 1000
)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- new_data:

  A data frame containing observations at which prediction intervals for
  \\\hat{y}\\ are to be computed.

- pi_conf:

  Confidence level for the prediction intervals. The default is 95%.

- num_samples_per_data_point:

  The number of samples taken from the predictive distribution. The
  default is 1000.

## Value

Returns a matrix of the lower and upper bounds of the prediction
intervals for each observation in `new_data`.

## Details

Credible intervals (see
[`calc_credible_intervals`](https://kapelner.github.io/bartMachine/reference/calc_credible_intervals.md))
are the appropriate quantiles of the prediction for each of the Gibbs
samples post-burn in. Prediction intervals also make use of the noise
estimate at each Gibbs sample and hence are wider. For each Gibbs
sample, we record the \\\hat{y}\\ estimate of the response and the
\\\hat{\sigma^2}\\ estimate of the noise variance. We then sample
`normal_samples_per_gibbs_sample` times from a \\N(\hat{y},
\hat{\sigma^2})\\ random variable to simulate many possible disturbances
for that Gibbs sample. Then, all `normal_samples_per_gibbs_sample` times
the number of Gibbs sample post burn-in are collected and the
appropriate quantiles are taken based on the confidence level,
`pi_conf`.

## Note

This function is parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).

## References

Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning with
Bayesian Additive Regression Trees. Journal of Statistical Software,
70(4), 1-40.
[doi:10.18637/jss.v070.i04](https://doi.org/10.18637/jss.v070.i04)

## See also

[`calc_credible_intervals`](https://kapelner.github.io/bartMachine/reference/calc_credible_intervals.md),
[`bart_machine_get_posterior`](https://kapelner.github.io/bartMachine/reference/bart_machine_get_posterior.md)

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
#generate Friedman data
set.seed(11)
n  = 200
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y)

#get prediction interval
pred_int = calc_prediction_intervals(bart_machine, X)
print(head(pred_int))
} # }
```
