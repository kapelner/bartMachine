# Summarizes information about a `bartMachine` object.

This is an alias for the
[`summary.bartMachine`](https://kapelner.github.io/bartMachine/reference/summary.bartMachine.md)
function. See description in that section.

## Usage

``` r
# S3 method for class 'bartMachine'
print(x, verbose = TRUE, ...)
```

## Arguments

- x:

  An object of class “bartMachine”.

- verbose:

  If TRUE, prints summary output.

- ...:

  Parameters that are ignored.

## Value

None.

## Author

Adam Kapelner and Justin Bleich

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
print(bart_machine)

##Also, the default print works too
bart_machine
} # }
```
