# Dummify Design Matrix

Create a data frame with factors converted to dummies.

## Usage

``` r
dummify_data(data)
```

## Arguments

- data:

  Data frame to be dummified.

## Value

Returns a data frame with factors converted to dummy indicator
variables.

## Details

The column names of the dummy variables are given by the
“FactorName_LevelName” and are augmented to the end of the design
matrix. See the example below.

## Note

BART handles dummification internally. This function is provided as a
utility function.

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
#generate data
set.seed(11)
x1 = rnorm(20)
x2 = as.factor(ifelse(x1 > 0, "A", "B"))
x3 = runif(20)
X = data.frame(x1,x2,x3)
#dummify data
X_dummified = dummify_data(X)
print(X_dummified)
} # }
```
