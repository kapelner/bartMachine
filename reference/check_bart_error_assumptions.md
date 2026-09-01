# Check BART Error Assumptions

Diagnostic tools to assess whether the errors of the BART model for
regression are normally distributed and homoskedastic, as assumed by the
model. This function generates a normal quantile plot of the residuals
with a Shapiro-Wilks p-value as well as a residual plot.

## Usage

``` r
check_bart_error_assumptions(
  bart_machine,
  hetero_plot = "yhats",
  verbose = TRUE
)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- hetero_plot:

  If “yhats”, the residuals are plotted against the fitted values of the
  response. If “ys”, the residuals are plotted against the actual values
  of the response.

- verbose:

  If TRUE, prints plots to the active device.

## Value

None.

## See also

[`plot_convergence_diagnostics`](https://kapelner.github.io/bartMachine/reference/plot_convergence_diagnostics.md)

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

#check error diagnostics
check_bart_error_assumptions(bart_machine)
} # }
```
