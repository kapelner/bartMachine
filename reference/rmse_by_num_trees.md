# Assess the Out-of-sample RMSE by Number of Trees

Assess out-of-sample RMSE of a BART model for varying numbers of trees
in the sum-of-trees model.

## Usage

``` r
rmse_by_num_trees(
  bart_machine,
  tree_list = c(5, seq(10, 50, 10), 100, 150, 200),
  in_sample = FALSE,
  plot = TRUE,
  holdout_pctg = 0.3,
  num_replicates = 4,
  verbose = TRUE,
  ...
)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- tree_list:

  List of sizes for the sum-of-trees models.

- in_sample:

  If TRUE, the RMSE is computed on in-sample data rather than an
  out-of-sample holdout.

- plot:

  If TRUE, a plot of the RMSE by the number of trees in the ensemble is
  created.

- holdout_pctg:

  Percentage of the data to be treated as an out-of-sample holdout.

- num_replicates:

  Number of replicates to average the results over. Each replicate uses
  a randomly sampled holdout of the data, (which could have overlap).

- verbose:

  If TRUE, prints progress messages and plots to the active device.

- ...:

  Other arguments to be passed to the plot function.

## Value

Invisibly, returns the out-of-sample average RMSEs for each tree size.

## Note

Since using a large number of trees can substantially increase
computation time, this plot can help assess whether a smaller ensemble
size is sufficient to obtain desirable predictive performance. This
function is parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
#generate Friedman data
set.seed(11)
n  = 200
p = 10
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y, num_trees = 20)

#explore RMSE by number of trees
rmse_by_num_trees(bart_machine)
} # }
```
