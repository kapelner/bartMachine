# Make a prediction on data using a BART object

Makes a prediction on new data given a fitted BART model for regression
or classification.

## Usage

``` r
# S3 method for class 'bartMachine'
predict(
  object,
  new_data,
  type = "prob",
  prob_rule_class = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  An object of class “bartMachine”.

- new_data:

  A data frame where each row is an observation to predict. The column
  names should be the same as the column names of the training data.

- type:

  Only relevant if the bartMachine model is classification. The type can
  be “prob” which will return the estimate of \\P(Y = 1)\\(the
  “positive” class) or “class” which will return the best guess as to
  the class of the object, in the original label, based on if the
  probability estimate is greater than `prob_rule_class`. Default is
  “prob.”

- prob_rule_class:

  The rule to determine when the class estimate is \\Y = 1\\ (the
  “positive” class) based on the probability estimate. This defaults to
  what was originally specified in the `bart_machine` object.

- verbose:

  Prints out prediction-related messages. Currently in use only for
  probability predictions to let the user know which class is being
  predicted. Default is `TRUE`.

- ...:

  Parameters that are ignored.

## Value

If regression, a numeric vector of `y_hat`, the best guess as to the
response. If classification and ``` type = ``prob'' ```, a numeric
vector of `p_hat`, the best guess as to the probability of the response
class being the ”positive” class. If classification and
`type = ''class''`, a character vector of the best guess of the
response's class labels.

## See also

[`bart_predict_for_test_data`](https://kapelner.github.io/bartMachine/reference/bart_predict_for_test_data.md)

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
#Regression example
if (FALSE) { # \dontrun{
#generate Friedman data
set.seed(11)
n  = 200
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y)

##make predictions on the training data
y_hat = predict(bart_machine, X)

#Classification example
data(iris)
iris2 = iris[51 : 150, ] #do not include the third type of flower for this example
iris2$Species = factor(iris2$Species)
bart_machine = bartMachine(iris2[ ,1:4], iris2$Species)

##make probability predictions on the training data
p_hat = predict(bart_machine, X)

##make class predictions on test data
y_hat_class = predict(bart_machine, X, type = "class")

##make class predictions on test data conservatively for ''versicolor''
y_hat_class_conservative = predict(bart_machine, X, type = "class", prob_rule_class = 0.9)
} # }
```
