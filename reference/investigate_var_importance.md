# Explore Variable Inclusion Proportions in BART Model

Explore the variable inclusion proportions for a BART model to learn
about the relative influence of the different covariates. This function
includes an option to generate a plot of the variable inclusion
proportions.

## Usage

``` r
investigate_var_importance(
  bart_machine,
  type = "splits",
  plot = TRUE,
  num_replicates_for_avg = 5,
  num_trees_bottleneck = 20,
  num_var_plot = Inf,
  bottom_margin = 10,
  verbose = TRUE
)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- type:

  If “splits”, then the proportion of times each variable is chosen for
  a splitting rule is computed. If “trees”, then the proportion of times
  each variable appears in a tree is computed.

- plot:

  If TRUE, a plot of the variable inclusion proportions is generated.

- num_replicates_for_avg:

  The number of replicates of BART to be used to generate variable
  inclusion proportions. Averaging across multiple BART models improves
  stability of the estimates. See Bleich et al. (2013) for more details.

- num_trees_bottleneck:

  Number of trees to be used in the sum-of-trees for computing the
  variable inclusion proportions. A small number of trees should be used
  to force the variables to compete for entry into the model. Chipman et
  al. (2010) recommend 20. See this reference for more details.

- num_var_plot:

  Number of variables to be shown on the plot. If “Inf”, all variables
  are plotted.

- bottom_margin:

  A display parameter that adjusts the bottom margin of the graph if
  labels are clipped. The scale of this parameter is the same as set
  with `par(mar = c(....))` in R. Higher values allow for more space if
  the covariate names are long. Note that making this parameter too
  large will prevent plotting and the plot function in R will throw an
  error.

- verbose:

  If TRUE, prints progress messages and plots to the active device.

## Value

Invisibly, returns a list with the following components:

- avg_var_props:

  The average variable inclusion proportions for each variable  
  (across `num_replicates_for_avg`)

- sd_var_props:

  The standard deviation of the variable inclusion proportions for each
  variable (across `num_replicates_for_avg`)

## Details

In the plot, the red bars correspond to the standard error of the
variable inclusion proportion estimates.

## Note

This function is parallelized by the number of cores set in
[`set_bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/set_bart_machine_num_cores.md).

## References

Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning with
Bayesian Additive Regression Trees. Journal of Statistical Software,
70(4), 1-40.
[doi:10.18637/jss.v070.i04](https://doi.org/10.18637/jss.v070.i04)

J Bleich, A Kapelner, ST Jensen, and EI George. Variable Selection
Inference for Bayesian Additive Regression Trees. ArXiv e-prints, 2013.

HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive
Regressive Trees. The Annals of Applied Statistics, 4(1): 266–298, 2010.

## See also

[`interaction_investigator`](https://kapelner.github.io/bartMachine/reference/interaction_investigator.md)

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

#investigate variable inclusion proportions
investigate_var_importance(bart_machine)
} # }
```
