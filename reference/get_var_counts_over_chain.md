# Get the Variable Inclusion Counts

Computes the variable inclusion counts for a BART model.

## Usage

``` r
get_var_counts_over_chain(bart_machine, type = "splits")
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- type:

  If “splits”, then the number of times each variable is chosen for a
  splitting rule is computed. If “trees”, then the number of times each
  variable appears in a tree is computed.

## Value

Returns a matrix of counts of each predictor across all trees by Gibbs
sample. Thus, the dimension is `num_iterations_after_burn_in` by `p`
(where `p` is the number of predictors after dummifying factors and
adding missingness dummies if specified by
`use_missing_data_dummies_as_covars`).

## See also

[`get_var_props_over_chain`](https://kapelner.github.io/bartMachine/reference/get_var_props_over_chain.md)

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

#get variable inclusion counts
var_counts = get_var_counts_over_chain(bart_machine)
print(var_counts)
} # }
```
