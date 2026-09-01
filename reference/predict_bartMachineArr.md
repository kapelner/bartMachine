# Make a prediction on data using a BART array object

Makes a prediction on new data given an array of fitted BART model for
regression or classification. If BART creates models that are variable,
running many and averaging is a good strategy. It is well known that the
Gibbs sampler gets locked into local modes at times. This is a way to
average over many chains.

## Usage

``` r
predict_bartMachineArr(object, new_data, ...)
```

## Arguments

- object:

  An object of class “bartMachineArr”.

- new_data:

  A data frame where each row is an observation to predict. The column
  names should be the same as the column names of the training data.

- ...:

  Not supported. Note that parameters `type` and `prob_rule_class` for
  [`predict.bartMachine`](https://kapelner.github.io/bartMachine/reference/predict.bartMachine.md)
  are not supported.

## Value

If regression, a numeric vector of `y_hat`, the best guess as to the
response. If classification and ``` type = ``prob'' ```, a numeric
vector of `p_hat`, the best guess as to the probability of the response
class being the ”positive” class. If classification and
`type = ''class''`, a character vector of the best guess of the
response's class labels.

## See also

[`predict.bartMachine`](https://kapelner.github.io/bartMachine/reference/predict.bartMachine.md)

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
bart_machine_arr = bartMachineArr(bart_machine)

##make predictions on the training data
y_hat = predict(bart_machine_arr, X)

#Classification example
data(iris)
iris2 = iris[51 : 150, ] #do not include the third type of flower for this example
iris2$Species = factor(iris2$Species)
bart_machine = bartMachine(iris2[ ,1:4], iris2$Species)
bart_machine_arr = bartMachineArr(bart_machine)

##make probability predictions on the training data
p_hat = predict_bartMachineArr(bart_machine_arr, iris2[ ,1:4])
} # }
```
