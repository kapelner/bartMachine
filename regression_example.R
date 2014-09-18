library(bartMachine)
bart_machine = build_bart_machine(as.data.frame(1 : 100), 1 + rnorm(100))
save.image("test_bart_machine.RData")
#close R, open R
library(bartMachine)
load("test_bart_machine.RData")
#> bart_machine$java_bart_machine
#[1] "Java-Object<null>"
## doesn't work

library(bartMachine)
bart_machine = build_bart_machine(as.data.frame(1 : 100), 1 + rnorm(100))
serialized = .jserialize(bart_machine$java_bart_machine)
save.image("test_bart_machine.RData")
#close R, open R
library(bartMachine)
load("test_bart_machine.RData")
bart_machine$java_bart_machine = .junserialize(serialized)
bart_machine$java_bart_machine


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