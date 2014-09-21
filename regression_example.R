

## try using built-in jcache
R
options(java.parameters = "-Xmx1500m")
library(bartMachine)
n = 5000
x = 1 : n; y = x + rnorm(n)
bart_machine = build_bart_machine(as.data.frame(x), y, serialize = TRUE)
save.image("test_bart_machine.RData")
q("no")
#close R, open R
R
options(java.parameters = "-Xmx1500m")
library(bartMachine)
load("test_bart_machine.RData")
predict(bart_machine, as.data.frame(x[1:100]))


##how big is this stuff?
data_names = names(bart_machine)
sizes = matrix(NA, ncol = 1, nrow = length(data_names))
rownames(sizes) = data_names
for (i in 1 : length(data_names)){
	sizes[i, ] = object.size(bart_machine[[data_names[i]]]) / 1e6
}
t(t(sizes[order(-sizes), ]))
q("no")

####ensure no more memory leak
options(java.parameters = "-Xmx1000m")
library(bartMachine)
x = 1 : 100; y = x + rnorm(100)
for (i in 1 : 10000){
	bart_machine = build_bart_machine(as.data.frame(x), y)
}

## If it helps, this may

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

yhat = predict(bart_machine, Xtest)