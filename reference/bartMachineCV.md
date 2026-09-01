# Build BART-CV

Builds a BART-CV model by cross-validating over a grid of hyperparameter
choices.

## Usage

``` r
bartMachineCV(
  X = NULL,
  y = NULL,
  Xy = NULL,
  num_tree_cvs = c(50, 200),
  k_cvs = c(2, 3, 5),
  nu_q_cvs = NULL,
  k_folds = 5,
  folds_vec = NULL,
  use_xoshiro = FALSE,
  verbose = FALSE,
  ...
)

build_bart_machine_cv(
  X = NULL,
  y = NULL,
  Xy = NULL,
  num_tree_cvs = c(50, 200),
  k_cvs = c(2, 3, 5),
  nu_q_cvs = NULL,
  k_folds = 5,
  folds_vec = NULL,
  use_xoshiro = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- X:

  Data frame of predictors. Factors are automatically converted to
  dummies internally.

- y:

  Vector of response variable. If `y` is `numeric` or `integer`, a BART
  model for regression is built. If `y` is a factor with two levels, a
  BART model for classification is built.

- Xy:

  A data frame of predictors and the response. The response column must
  be named “y”.

- num_tree_cvs:

  Vector of sizes for the sum-of-trees models to cross-validate over.

- k_cvs:

  Vector of choices for the hyperparameter `k` to cross-validate over.

- nu_q_cvs:

  Only for regression. List of vectors containing (`nu`, `q`) ordered
  pair choices to cross-validate over. If `NULL`, then it defaults to
  the three values `list(c(3, 0.9), c(3, 0.99), c(10, 0.75))`.

- k_folds:

  Number of folds for cross-validation

- folds_vec:

  An integer vector of indices specifying which fold each observation
  belongs to.

- use_xoshiro:

  if TRUE, use the Xoshiro256PlusPlus random number generator; if FALSE,
  use the legacy MersenneTwister random number generator (default is
  FALSE)

- verbose:

  Prints information about progress of the algorithm to the screen.

- ...:

  Additional arguments to be passed to `bartMachine`.

## Value

Returns an object of class “bartMachine” with the set of hyperparameters
chosen via cross-validation. We also return a matrix “cv_stats” which
contains the out-of-sample RMSE for each hyperparameter set tried and
“folds” which gives the fold in which each observation fell across the
k-folds.

## Note

This function may require significant run-time. This function is
parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md)
via calling
[`bartMachine`](https://kapelner.github.io/bartMachine/reference/bartMachine.md).

## References

Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning with
Bayesian Additive Regression Trees. Journal of Statistical Software,
70(4), 1-40.
[doi:10.18637/jss.v070.i04](https://doi.org/10.18637/jss.v070.i04)

## See also

[`bartMachine`](https://kapelner.github.io/bartMachine/reference/bartMachine.md)

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
bart_machine_cv = bartMachineCV(X, y)

#information about cross-validated model
summary(bart_machine_cv)
} # }
```
