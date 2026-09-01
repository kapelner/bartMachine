# Calculate Credible Intervals

Generates credible intervals for \\\hat{f}(x)\\ for a specified set of
observations.

## Usage

``` r
calc_credible_intervals(bart_machine, new_data, ci_conf = 0.95)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- new_data:

  A data frame containing observations at which credible intervals for
  \\\hat{f}(x)\\ are to be computed.

- ci_conf:

  Confidence level for the credible intervals. The default is 95%.

## Value

Returns a matrix of the lower and upper bounds of the credible intervals
for each observation in `new_data`.

## Details

This interval is the appropriate quantiles based on the confidence
level, `ci_conf`, of the predictions for each of the Gibbs samples
post-burn in.

## Note

This function is parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).

## See also

[`calc_prediction_intervals`](https://kapelner.github.io/bartMachine/reference/calc_prediction_intervals.md),
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

#get credible interval
cred_int = calc_credible_intervals(bart_machine, X)
print(head(cred_int))
} # }
```
