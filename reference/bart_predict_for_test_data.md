# Predict for Test Data with Known Outcomes

Utility wrapper function for computing out-of-sample metrics for a BART
model when the test set outcomes are known.

## Usage

``` r
bart_predict_for_test_data(
  bart_machine,
  Xtest,
  ytest,
  prob_rule_class = NULL,
  verbose = TRUE
)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- Xtest:

  Data frame for test data containing rows at which predictions are to
  be made. Colnames should match that of the training data.

- ytest:

  Actual outcomes for test data.

- prob_rule_class:

  Threshold for classification.

- verbose:

  If TRUE, prints prediction-related messages.

## Value

For regression models, a list with the following components is returned:

- y_hat:

  Predictions (as posterior means) for the test observations.

- L1_err:

  L1 error for predictions.

- L2_err:

  L2 error for predictions.

- rmse:

  RMSE for predictions.

For classification models, a list with the following components is
returned:

- y_hat:

  Class predictions for the test observations.

- p_hat:

  Probability estimates for the test observations.

- confusion_matrix:

  A confusion matrix for the test observations.

## See also

[`predict`](https://rdrr.io/r/stats/predict.html)

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
#generate Friedman data
set.seed(11)
n  = 250
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##split into train and test
train_X = X[1 : 200, ]
test_X = X[201 : 250, ]
train_y = y[1 : 200]
test_y = y[201 : 250]

##build BART regression model
bart_machine = bartMachine(train_X, train_y)

#explore performance on test data
oos_perf = bart_predict_for_test_data(bart_machine, test_X, test_y)
print(oos_perf$rmse)
} # }
```
