##Generate Friedman data
library(bartMachine)

set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(2500)

gen_friedman_data = function(n, p, sigma){
  if (p < 5){
	  stop("p must be greater than or equal to 5")
  }
  X = matrix(runif(n * p ), nrow = n, ncol = p)
  y = 10 * sin(pi * X[, 1] *X[, 2]) + 20 *(X[, 3] - .5)^2 + 10 * X[, 4] + 5 * X[, 5] + rnorm(n, 0, sigma)
  data.frame(y, X)
}

##### section 4.11

#make training data
fr_data = gen_friedman_data(500, 100, 1)
y = fr_data$y
X = fr_data[, 2 : 101]

#make test data
fr_data = gen_friedman_data(500, 100, 1)
Xtest = fr_data[, 2 : 101]
ytest = fr_data$y

#build uninformed and informed models
bart_machine = bartMachine(X, y)

prior = c(rep(5, times = 5), rep(1, times = 95)) 
bart_machine_informed = bartMachine(X, y, cov_prior_vec = prior)

bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse
bart_predict_for_test_data(bart_machine_informed, Xtest, ytest)$rmse

##### section 4.12

fr_data = gen_friedman_data(500, 10, 1)
y = fr_data$y
X = fr_data[, 2 : 11]

bart_machine = bartMachine(X, y)

# Figure 11
interaction_investigator(bart_machine, num_replicates_for_avg = 25, num_var_plot = 10)
