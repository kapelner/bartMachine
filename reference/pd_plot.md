# Partial Dependence Plot

Creates a partial dependence plot for a BART model for regression or
classification.

## Usage

``` r
pd_plot(
  bart_machine,
  j,
  levs = c(0.05, seq(from = 0.1, to = 0.9, by = 0.1), 0.95),
  lower_ci = 0.025,
  upper_ci = 0.975,
  prop_data = 1,
  verbose = TRUE
)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- j:

  The number or name of the column in the design matrix for which the
  partial dependence plot is to be created.

- levs:

  Quantiles at which the partial dependence function should be
  evaluated. Linear extrapolation is performed between these points.

- lower_ci:

  Lower limit for credible interval

- upper_ci:

  Upper limit for credible interval

- prop_data:

  The proportion of the training data to use. Default is 1. Use a lower
  proportion for speedier pd_plots. The closer to 1, the more resolution
  the PD plot will have; the closer to 0, the lower but faster.

- verbose:

  If TRUE, prints progress messages and plots to the active device.

## Value

Invisibly, returns a list with the following components:

- x_j_quants:

  Quantiles at which the partial dependence function is evaluated

- bart_avg_predictions_by_quantile_by_gibbs:

  All samples of \\\hat{f}(x)\\

- bart_avg_predictions_by_quantile:

  Posterior means for \\\hat{f}(x)\\ at `x_j_quants`

- bart_avg_predictions_lower:

  Lower bound of the desired confidence of the credible interval of
  \\\hat{f}(x)\\

- bart_avg_predictions_upper:

  Upper bound of the desired confidence of the credible interval of
  \\\hat{f}(x)\\

- prop_data:

  The proportion of the training data to use as specified when this
  function was executed

## Details

For regression models, the units on the y-axis are the same as the units
of the response. For classification models, the units on the y-axis are
probits.

## Note

This function is parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).

## References

Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning with
Bayesian Additive Regression Trees. Journal of Statistical Software,
70(4), 1-40.
[doi:10.18637/jss.v070.i04](https://doi.org/10.18637/jss.v070.i04)

HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive
Regressive Trees. The Annals of Applied Statistics, 4(1): 266–298, 2010.

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

#partial dependence plot for quadratic term
pd_plot(bart_machine, "X3")


#Classification example

#get data and only use 2 factors
data(iris)
iris2 = iris[51:150,]
iris2$Species = factor(iris2$Species)

#build BART classification model
bart_machine = bartMachine(iris2[ ,1:4], iris2$Species)

#partial dependence plot
pd_plot(bart_machine, "Petal.Width")
} # }
```
