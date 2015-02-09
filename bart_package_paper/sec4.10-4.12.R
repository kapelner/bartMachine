options(java.parameters = "-Xmx2500m")
library(bartMachine)
set_bart_machine_num_cores(4)


gen_friedman_data = function(n, p, sigma){
  if (p < 5){
	  stop("p must be greater than or equal to 5")
  }
  X = matrix(runif(n * p), nrow = n, ncol = p)
  y = 10 * sin(pi * X[, 1] *X[, 2]) + 20 *(X[, 3] - .5)^2 + 10 * X[, 4] + 5 * X[, 5] + rnorm(n, 0, sigma)
  data.frame(y, X)
}

##### section 4.10

#set up prior
p = 5
p0 = 95
prior = c(rep(5, times = p), rep(1, times = p0)) 

#make training and test data
ntrain = 500
sigma = 1
fr_data = gen_friedman_data(ntrain, p + p0, sigma)
y = fr_data$y
X = fr_data[, 2 : (p + p0 + 1)]
ntest = 500
fr_data = gen_friedman_data(ntest, p + p0, sigma)
Xtest = fr_data[, 2 : (p + p0 + 1)]
ytest = fr_data$y

#build uninformed and informed models
bart_machine = bartMachine(X, y)
bart_machine_informed = bartMachine(X, y, cov_prior_vec = prior)
#test out of sample
bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse
bart_predict_for_test_data(bart_machine_informed, Xtest, ytest)$rmse

##### section 4.11

fr_data = gen_friedman_data(500, 10, 1)
y = fr_data$y
X = fr_data[, 2 : 11]

bart_machine = bartMachine(X, y)

# Figure 10
interaction_investigator(bart_machine, num_replicates_for_avg = 25, num_var_plot = 10)

##### section 4.12

#bartMachine models can be saved and can persist across R sessions
bart_machine = bartMachine(X, y, serialize = TRUE)
save.image("bart_demo.RData")
q("no")
R
options(java.parameters = "-Xmx7000m")
library(bartMachine)
load("bart_demo.RData")
predict(bart_machine, X)

#Demonstrate that serialiation can be very expensive
options(java.parameters = "-Xmx7000m")
library(bartMachine)
fr_data = gen_friedman_data(4000, 1000, 1)
y = fr_data$y
X = fr_data[, 2 : 1001]
bart_machine = bartMachine(X, y, serialize = TRUE, num_iterations_after_burn_in = 4000, num_trees = 100, run_in_sample = FALSE, mem_cache_for_speed = FALSE)
save.image("bart_demo.RData")
q("no")

#demonstrate you cannot save a bartMachine model in an RData file
#without using the serialize option
options(java.parameters = "-Xmx6000m")
library(bartMachine)
bart_machine = bartMachine(X, y)
save.image("bart_demo.RData")
q("no")
R
options(java.parameters = "-Xmx6000m")
library(bartMachine)
load("bart_demo.RData")
predict(bart_machine, X)
