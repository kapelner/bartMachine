

library(bartMachine)
#print(.jclassPath())
#cl = .jclassLoader()
#cl$verbose = T

x = matrix(1 : 1000, ncol = 10)
y = 2 : 101

bart_machine = build_bart_machine(as.data.frame(x), y)
plot_y_vs_yhat(bart_machine)

#get some data
library(MASS)
data(Boston)
X = Boston
#X = cbind(X, rnorm(nrow(X)))
y = X$medv
X$medv = NULL
#X$chas = as.character(X$chas)
#X$rad = as.factor(X$rad)

#split it into test and training
Xtrain = X[1 : (nrow(X) / 2), ]
ytrain = y[1 : (nrow(X) / 2)]
Xtest = X[(nrow(X) / 2 + 1) : nrow(X), ]
ytest = y[(nrow(X) / 2 + 1) : nrow(X)]
#build the BART machine
#bart_machines = list()
#for (i in 1 : 5000){
#graphics.off()
#windows()

dim(Xtrain)
head(Xtrain)
#Xtrain[3, 5] = NA

set_bart_machine_num_cores(4)
bart_machine = build_bart_machine(Xtrain, ytrain,
		num_trees = 200,
		num_burn_in = 300,
		num_iterations_after_burn_in = 1000,
		use_missing_data = TRUE,
		debug_log = TRUE,
		verbose = TRUE)
bart_machine
head(bart_machine$model_matrix_training_data)
plot_y_vs_yhat(bart_machine)
plot_y_vs_yhat(bart_machine, ppis = TRUE)
plot_y_vs_yhat(bart_machine, X = Xtest, y = ytest, ppis = TRUE)


mod = lm(y ~ ., X)
summary(mod)









library(randomForest)
rf = randomForest(x = Xtrain, y = ytrain)
#find R^2
yhat = predict(rf, Xtrain)
plot(ytrain, yhat)
abline(a = 0, b = 1)
sse = sum((ytrain - yhat)^2)
sst = sum((ytrain - mean(ytrain))^2)
Pseudo_Rsq_rf = 1 - sse / sst
Pseudo_Rsq_rf


var_selection_by_permute_response_three_methods(bart_machine, num_permute_samples = 10)


bart_machine = build_bart_machine_cv(Xtrain, ytrain,
		num_burn_in = 300,
		num_iterations_after_burn_in = 2000,
		debug_log = TRUE)


	
	cat(paste("built bart machine #", i, "\n"))
#}
summary(bart_machine)
bart_machine$training_data_features

plot_y_vs_yhat(bart_machine)
plot_y_vs_yhat(bart_machine, ppis = TRUE)
plot_y_vs_yhat(bart_machine, X = Xtest, y = ytest, ppis = TRUE)

cov_importance_test(bart_machine, num_trees = 20, plot = TRUE)
cov_importance_test(bart_machine, num_trees = 20, plot = TRUE, covariates = c(14))
cov_importance_test(bart_machine, num_trees = 20, plot = TRUE, covariates = c(7))
cov_importance_test(bart_machine, num_trees = 20, plot = TRUE, covariates = c(7, 14))

#convenience to predict on the test data automatically computing SSE, etc
predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
predict_obj$rmse


library(BayesTree)
rob = bart(x.train = Xtrain,y.train=ytrain,x.test=Xtest,ndpost=2000,nskip=500)
sqrt(sum((rob$yhat.test.mean - ytest)^2) / length(ytest))
mean(rob$sigma)


interaction_investigator(bart_machine, num_replicates_for_avg = 5, num_var_plot = 20, num_trees_bottleneck = 20)
investigate_var_importance(bart_machine)
investigate_var_importance(bart_machine, type = "trees")

plot_y_vs_yhat(bart_machine, ppis=T)

plot_tree_num_nodes(bart_machine)
plot_tree_depths(bart_machine)
plot_mh_acceptance_reject(bart_machine)
plot_convergence_diagnostics(bart_machine)


plot_sigsqs_convergence_diagnostics(bart_machine)
hist_sigsqs(bart_machine)
check_bart_error_assumptions(bart_machine)



pd_plot(bart_machine, 7)


rmses = array(NA, 20)
for (k in 2 : 20){
	rmse = k_fold_cv(X, k_folds = k, 
		num_trees = 200,
		num_burn_in = 250, 
		num_iterations_after_burn_in = 1000)$rmse
	rmses[k] = rmse
}
plot(1:20, rmses, type="l")
k = 1:20
summary(lm(rmses ~ k))


#get PPIs for test data
ppi_obj = calc_credible_intervals(bart_machine, Xtest)



#now test the variable importance
##generate the Friedman data
n = 100
p = 100
X = as.data.frame(matrix(runif(n * p, 0 , 1), ncol = p))
error = rnorm(n, 0, 1)

y = 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)^2 + 10 * X[, 4] + 5 * X[, 5] + error
#
#Xy = data.frame(X, y)
#summary(lm(y ~ ., Xy))
#Xy = read.csv("datasets/r_friedman_hd.csv")
#Xtrain = Xy[1 : 80, 1 : 100]
#ytrain = Xy[1 : 80, 101]
#Xtest = Xy[81 : 100, 1 : 100]
#ytest = Xy[81 : 100, 101]

Xtrain = X[1 : 100,]
ytrain = y[1 : 100]
#Xtest = X[1001 : 2000, ]
#ytest = y[1001 : 2000]

set_bart_machine_num_cores(4)
bart_machine = build_bart_machine(Xtrain, ytrain,
	num_trees = 200,
	num_burn_in = 250, 
	num_iterations_after_burn_in = 300)

var_selection_by_permute_response_three_methods(bart_machine, num_permute_samples = 100, num_var_plot = 20)


for (i in 1 : 50){
	t(sapply(.jcall(bart_machine$java_bart_machine, "[[I", "getCountsForAllAttribute", "splits"), .jevalArray))
	cat(".")
}
plot_y_vs_yhat(bart_machine, ppis=T)
plot_y_vs_yhat(bart_machine, X = Xtest, y = ytest, ppis = TRUE)

summary(bart_machine)
var_importance_by_shuffling(bart_machine, num_var_plot = 20)
var_importance_by_dropping_variable(bart_machine, num_var_plot = 20)
hist_sigsqs(bart_machine)
windows()
plot_sigsqs_convergence_diagnostics(bart_machine)
check_bart_error_assumptions(bart_machine)
windows()
plot_y_vs_yhat(bart_machine, ppis = T)
rmse_by_num_trees(bart_machine)
rmse_by_num_trees(bart_machine, in_sample = TRUE)

windows()
investigate_var_importance(bart_machine, num_replicates_for_avg = 5,num_var_plot = 15, type = "trees")
windows()
interaction_investigator(bart_machine, num_replicates_for_avg = 5, num_var_plot = 15)

library(BayesTree)
rob = bart(x.train = Xy[, 1:100], y.train = Xy[ ,101], ndpost = 1000, nskip = 1000, sigest = sd(Xy[,101]), ntree = 200)
counts = apply(rob$varcount,2,sum)
rob$sigest
mean(rob$sigma)
counts/sum(counts)
names(counts)=colnames(Xy)[1:100]
sort(counts, dec = T)
interaction_investigator(bart_machine, num_replicates_for_avg = 5, num_var_plot=20)




#X = read.csv("datasets/r_boston.csv")
Xtrain = X[1 : (nrow(X) / 2), ]
Xtest = X[(nrow(X) / 2 + 1) : nrow(X), ]
bart_machine = build_bart_machine(X, 
		num_trees = 200,
		num_burn_in = 2000, 
		num_iterations_after_burn_in = 500)
summary(bart_machine)
plot_y_vs_yhat(bart_machine)

pred_obj = bart_predict_for_test_data(bart_machine_cv, Xtest)
pred_obj$rmse




library(randomForest)
rf = randomForest(x = Xtrain[, 1 : 13], y = Xtrain$y, importance = TRUE)
importance(rf, type=1)
preds = predict(rf,newdata = Xtest[, 1 : 13])
sqrt(sum((preds - Xtest$y)^2) / length(preds))




source("r_scripts/create_simulated_models.R")

Xy = simulate_data_from_simulation_name("bivariate_linear")
#Xy = read.csv("datasets/r_just_noise.csv")
#Xy=Xy[1:500,]

bart_machine = build_bart_machine(Xy[, 1 : (ncol(Xy) - 1)], Xy$y,
		num_trees = 200,
		num_burn_in = 1000, 
		num_iterations_after_burn_in = 500,
		s_sq_y = "mse")
summary(bart_machine)
hist_sigsqs(bart_machine)
plot_sigsqs_convergence_diagnostics(bart_machine)
rmse_by_num_trees(bart_machine)

bart_machine = build_bart_machine(Xy[, 1 : (ncol(Xy) - 1)], rnorm(nrow(Xy)),
		num_trees = 200,
		num_burn_in = 1000, 
		num_iterations_after_burn_in = 500,
		s_sq_y = "mse")

library(BayesTree)
rob = bart(x.train = Xy[, 1:100], y.train = Xy[ , 101], ndpost = 500, nskip = 1000, ntree = 20, sigest = sd(Xy[,101]))
counts = apply(rob$varcount,2,sum)

sort_props= sort(counts/sum(counts),dec = T)
sort_props
barplot(sort_props)
rob$sigest
mean(rob$sigma)




#PROBIT BART
Xy = read.csv("datasets/c_breastcancer.csv", header = FALSE)
X = Xy[, 1 : (ncol(Xy) - 1)]
y = as.factor(Xy[, ncol(Xy)])

set_bart_machine_num_cores(4)
bart_machine = build_bart_machine(X, y,
		num_trees = 200,
		num_burn_in = 1000, 
		num_iterations_after_burn_in = 500)
bart_machine
