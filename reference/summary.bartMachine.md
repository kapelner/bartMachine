# Summarizes information about a `bartMachine` object.

Provides a quick summary of the BART model.

## Usage

``` r
# S3 method for class 'bartMachine'
summary(object, verbose = TRUE, ...)
```

## Arguments

- object:

  An object of class “bartMachine”.

- verbose:

  If TRUE, prints summary output.

- ...:

  Parameters that are ignored.

## Value

None.

## Details

Gives the version number of the `bartMachine` package used to build this
`additiveBartMachine` object and if the object models either
“regression” or “classification.” Gives the amount of training data and
the dimension of feature space. Prints the amount of time it took to
build the model, how many processor cores were used to during its
construction, as well as the number of burn-in and posterior Gibbs
samples were used.

If the model is for regression, it prints the estimate of \\\sigma^2\\
before the model was constructed as well as after so the user can
inspect how much variance was explained.

If the model was built using the `run_in_sample = TRUE` parameter in
[`build_bart_machine`](https://kapelner.github.io/bartMachine/reference/bartMachine.md)
and is for regression, the summary L1, L2, rmse, Pseudo-\\R^2\\ are
printed as well as the p-value for the tests of normality and zero-mean
noise. If the model is for classification, a confusion matrix is
printed.

## Author

Adam Kapelner

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

##print out details
summary(bart_machine)

##Also, the default print works too
bart_machine
} # }
```
