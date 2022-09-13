options(java.parameters = c("-Xmx20000m"))
library(bartMachine)
set_bart_machine_num_cores(10)


cov_dgp = function(n, p){
  data.frame(matrix(runif(n * p), ncol = p))
}

response_function = function(X, sigma = 0.3){
  # 10 * sin(pi * X[ ,1] * X[,2]) + 20 * (X[,3] -.5)^2 + 10 * X[, 4] + 5 * X[, 5] + rnorm(nrow(X), 0, 0.01) #Friedman
  # X[ ,1] * X[,2] + X[ ,3] * X[,4] + X[ ,5] + rnorm(nrow(X), 0, sigma)
  X[ ,1] + X[,2] + X[ ,3] + X[,4] + X[ ,5] + rnorm(nrow(X), 0, sigma)
}

SEED = 1984
set.seed(SEED)
ntrain = 500
p = 5
Xtrain = cov_dgp(ntrain, p)
ytrain = response_function(Xtrain)
?bartMachine

gam = bartMachine(Xtrain, ytrain, interaction_constraints = as.list(seq(1 : p)))
additive_bart_machine = bartMachine(Xtrain, ytrain, interaction_constraints = list(c(1, 2), c(3, 4), 5))
bart_machine = bartMachine(Xtrain, ytrain)

summary(gam)
summary(additive_bart_machine)
summary(bart_machine)


####now oos
ntest = 1000
Xtest = cov_dgp(ntest, p)

y_hat_test_gam = predict(gam, Xtest)
y_hat_test_additive = predict(additive_bart_machine, Xtest)
y_hat_test = predict(bart_machine, Xtest)

ytest = response_function(Xtest)
sqrt(sum((y_hat_test_gam - ytest)^2) / ntest)
sqrt(sum((y_hat_test_additive - ytest)^2) / ntest)
sqrt(sum((y_hat_test - ytest)^2) / ntest)

