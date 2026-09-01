# Importance Test for Covariate(s) of Interest

This function tests the null hypothesis \\H_0\\: These covariates of
interest do not affect the response under the assumptions of the BART
model.

## Usage

``` r
cov_importance_test(
  bart_machine,
  covariates = NULL,
  num_permutation_samples = 100,
  plot = TRUE,
  verbose = TRUE
)
```

## Arguments

- bart_machine:

  An object of class “bart_machine”.

- covariates:

  A vector of names of covariates of interest to be tested for having an
  effect on the response. A value of NULL indicates an omnibus test for
  all covariates having an effect on the response. If the name of a
  covariate is a factor, the entire factor will be permuted. We do not
  recommend entering the names of factor covariate dummies.

- num_permutation_samples:

  The number of times to permute the covariates of interest and create a
  corresponding new BART model (see details).

- plot:

  If `TRUE`, this produces a histogram of the Pseudo-Rsq's / total
  misclassification error rates from the `num_permutations` BART models
  created with the `covariates` permuted. The plot also illustrates the
  observed Pseudo-Rsq's / total misclassification error rate from the
  original training data and indicates the test's p-value.

- verbose:

  If TRUE, prints progress and summary messages.

## Value

- permutation_samples_of_error:

  A vector which records the error metric of the BART models with the
  covariates permuted (see details).

- observed_error_estimate:

  For regression, this is the Pseudo-Rsq on the original training data
  set. For classification, this is the observed total misclassification
  error on the original training data set.

- pval:

  The approximate p-value for this test (see details).

## Details

To test the importance of a covariate or a set of covariates of interest
on the response, this function generates `num_permutations` BART models
with the covariate(s) of interest permuted (differently each time). On
each run, a measure of fit is recorded. For regression, the metric is
Pseudo-Rsq; for classification, it is total misclassification error.  
A p-value can then be generated as follows. For regression, the p-value
is the number of permutation-sampled Pseudo-Rsq's greater than the
observed Pseudo-Rsq divided by `num_permutations + 1`. For
classification, the p-value is the number of permutation-sampled total
misclassification errors less than the observed total misclassification
error divided by `num_permutations + 1`.

## Note

This function is parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).

## References

Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning with
Bayesian Additive Regression Trees. Journal of Statistical Software,
70(4), 1-40.
[doi:10.18637/jss.v070.i04](https://doi.org/10.18637/jss.v070.i04)

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
##regression example

##generate Friedman data
set.seed(11)
n  = 200
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y)

##now test if X[, 1] affects Y nonparametrically under the BART model assumptions
cov_importance_test(bart_machine, covariates = c(1))
## note the plot and the printed p-value

} # }
```
