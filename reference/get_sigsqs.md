# Get Posterior Error Variance Estimates

Returns the posterior estimates of the error variance from the Gibbs
samples with an option to create a histogram of the posterior estimates
of the error variance with a credible interval overlaid.

## Usage

``` r
get_sigsqs(
  bart_machine,
  after_burn_in = TRUE,
  plot_hist = FALSE,
  plot_CI = 0.95,
  plot_sigma = F,
  verbose = TRUE
)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- after_burn_in:

  If TRUE, only the \\\sigma^2\\ draws after the burn-in period are
  returned.

- plot_hist:

  If TRUE, a histogram of the posterior \\\sigma^2\\ draws is generated.

- plot_CI:

  Confidence level for credible interval on histogram.

- plot_sigma:

  If TRUE, plots \\\sigma\\ instead of \\\sigma^2\\.

- verbose:

  If TRUE, prints plots to the active device.

## Value

Returns a vector of posterior \\\sigma^2\\ draws (with or without the
burn-in samples).

## See also

`get_sigsqs`

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
#generate Friedman data
set.seed(11)
n  = 300
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y)

#get posterior sigma^2's after burn-in and plot
sigsqs = get_sigsqs(bart_machine, plot_hist = TRUE)
} # }
```
