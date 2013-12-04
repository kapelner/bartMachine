library(bartMachine)

x = matrix(1 : 1000, ncol = 10)
y = 2 : 101

bart_machine = build_bart_machine(as.data.frame(x), y)
plot_y_vs_yhat(bart_machine)

#get some data
library(MASS)
data(Boston)
X = Boston
y = X$medv
X$medv = NULL

Xtrain = X[1 : (nrow(X) / 2), ]
ytrain = y[1 : (nrow(X) / 2)]
Xtest = X[(nrow(X) / 2 + 1) : nrow(X), ]
ytest = y[(nrow(X) / 2 + 1) : nrow(X)]

set_bart_machine_num_cores(4)
bart_machine = build_bart_machine(Xtrain, ytrain,
		num_trees = 200,
		num_burn_in = 300,
		num_iterations_after_burn_in = 1000,
		use_missing_data = TRUE,
		debug_log = TRUE,
		verbose = TRUE)
bart_machine

plot_y_vs_yhat(bart_machine)