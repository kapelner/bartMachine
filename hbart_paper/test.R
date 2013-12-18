n = 1000

gamma = 7
beta = 100

X = runif(n)
sigsq = exp(X * gamma)
y = beta * X + rnorm(n, 0, sqrt(sigsq))
plot(X, y)

write.csv(cbind(X, y), "r_hbart.csv")

library(bartMachine)
set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)

bart_machine = build_bart_machine(Xy = cbind(X, y))
bart_machine
#plot(bart_machine$y_hat, y)
#plot(X, bart_machine$y_hat)

#check_bart_error_assumptions(bart_machine)

hbart_machine = build_bart_machine(Xy = cbind(X, y), use_heteroskedastic_linear_model = TRUE)
hbart_machine
#plot_y_vs_yhat(hbart_machine)

plot(X, bart_machine$y_hat)
points(X, hbart_machine$y_hat, col = "green")

#oos testing
Xtest = runif(n)
sigsq_test = exp(Xtest * gamma)
ytest = beta * Xtest + rnorm(n, 0, sqrt(sigsq_test))
Xtest = data.frame(Xtest)
colnames(Xtest) = c("X")

bart_oosrmse = bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse
hbart_oosrmse = bart_predict_for_test_data(hbart_machine, Xtest, ytest)$rmse
bart_oosrmse
hbart_oosrmse
#how much better does it do?
(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100

#### BHD
library(MASS)
data(Boston)
X = Boston
y = X$medv
X$medv = NULL

Xtrain = X[1 : (nrow(X) / 2), ]
ytrain = y[1 : (nrow(X) / 2)]
Xtest = X[(nrow(X) / 2 + 1) : nrow(X), ]
ytest = y[(nrow(X) / 2 + 1) : nrow(X)]


bart_machine = build_bart_machine(Xtrain, ytrain)
bart_machine

hbart_machine = build_bart_machine(Xtrain, ytrain, use_heteroskedastic_linear_model = TRUE)
hbart_machine

bart_oosrmse = bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse
hbart_oosrmse = bart_predict_for_test_data(hbart_machine, Xtest, ytest)$rmse
bart_oosrmse
hbart_oosrmse
#how much better does it do?
(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100
