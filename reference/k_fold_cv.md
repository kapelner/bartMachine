# Estimate Out-of-sample Error with K-fold Cross validation

Builds a BART model using a specified set of arguments to
`build_bart_machine` and estimates the out-of-sample performance by
using k-fold cross validation.

## Usage

``` r
k_fold_cv(X, y, k_folds = 5, folds_vec = NULL, verbose = FALSE, ...)
```

## Arguments

- X:

  Data frame of predictors. Factors are automatically converted to
  dummies internally.

- y:

  Vector of response variable. If `y` is `numeric` or `integer`, a BART
  model for regression is built. If `y` is a factor with two levels, a
  BART model for classification is built.

- k_folds:

  Number of folds to cross-validate over. This argument is ignored if
  `folds_vec` is non-null.

- folds_vec:

  An integer vector of indices specifying which fold each observation
  belongs to.

- verbose:

  Prints information about progress of the algorithm to the screen.

- ...:

  Additional arguments to be passed to `build_bart_machine`.

## Value

For regression models, a list with the following components is returned:

- y_hat:

  Predictions for the observations computed on the fold for which the
  observation was omitted from the training set.

- L1_err:

  Aggregate L1 error across the folds.

- L2_err:

  Aggregate L1 error across the folds.

- rmse:

  Aggregate RMSE across the folds.

- folds:

  Vector of indices specifying which fold each observation belonged to.

For classification models, a list with the following components is
returned:

- y_hat:

  Class predictions for the observations computed on the fold for which
  the observation was omitted from the training set.

- p_hat:

  Probability estimates for the observations computed on the fold for
  which the observation was omitted from the training set.

- confusion_matrix:

  Aggregate confusion matrix across the folds.

- misclassification_error:

  Total misclassification error across the folds.

- folds:

  Vector of indices specifying which fold each observation belonged to.

## Details

For each fold, a new BART model is trained (using the same set of
arguments) and its performance is evaluated on the holdout piece of that
fold.

## Note

This function is parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).

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

#evaluate default BART on 5 folds
k_fold_val = k_fold_cv(X, y)
print(k_fold_val$rmse)
} # }
```
