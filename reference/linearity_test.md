# Test of Linearity

Test to investigate \\H_0:\\ the functional relationship between the
response and the regressors is linear. We fit a linear model and then
test if the residuals are a function of the regressors using the

## Usage

``` r
linearity_test(
  lin_mod = NULL,
  X = NULL,
  y = NULL,
  num_permutation_samples = 100,
  plot = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- lin_mod:

  A linear model you can pass in if you do not want to use the default
  which is `lm(y ~ X)`. Default is `NULL` which should be used if you
  pass in `X` and `y`.

- X:

  Data frame of predictors. Factors are automatically converted to
  dummies internally. Default is `NULL` which should be used if you pass
  in `lin_mode`.

- y:

  Vector of response variable. If `y` is `numeric` or `integer`, a BART
  model for regression is built. If `y` is a factor with two levels, a
  BART model for classification is built. Default is `NULL` which should
  be used if you pass in `lin_mode`.

- num_permutation_samples:

  This function relies on
  [`cov_importance_test`](https://kapelner.github.io/bartMachine/reference/cov_importance_test.md)
  (see documentation there for details).

- plot:

  This function relies on
  [`cov_importance_test`](https://kapelner.github.io/bartMachine/reference/cov_importance_test.md)
  (see documentation there for details).

- verbose:

  If TRUE, prints progress and summary messages.

- ...:

  Additional parameters to be passed to `bartMachine`, the model
  constructed on the residuals of the linear model.

## Value

- permutation_samples_of_error:

  This function relies on
  [`cov_importance_test`](https://kapelner.github.io/bartMachine/reference/cov_importance_test.md)
  (see documentation there for details).

- observed_error_estimate:

  This function relies on
  [`cov_importance_test`](https://kapelner.github.io/bartMachine/reference/cov_importance_test.md)
  (see documentation there for details).

- pval:

  The approximate p-value for this test. See the documentation at
  [`cov_importance_test`](https://kapelner.github.io/bartMachine/reference/cov_importance_test.md).

## See also

[`cov_importance_test`](https://kapelner.github.io/bartMachine/reference/cov_importance_test.md)

## Author

Adam Kapelner

## Examples

``` r
if (FALSE) { # \dontrun{
##regression example

##generate Friedman data i.e. a nonlinear response model
set.seed(11)
n  = 200
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##now test if there is a nonlinear relationship between X1, ..., X5 and y.
linearity_test(X = X, y = y)
## note the plot and the printed p-value.. should be approx 0

#generate a linear response model
y = 1 * X[ ,1] + 3 * X[,2] + 5 * X[,3] + 7 * X[ ,4] + 9 * X[,5] + rnorm(n)
linearity_test(X = X, y = y)
## note the plot and the printed p-value.. should be > 0.05

} # }
```
