library(bartMachine)
library(BayesTree)
library(randomForest)

#set bartMachine computing parameters
set_bart_machine_num_cores(4)
set_bart_machine_memory(2500) #WARNING: a 64-bit machine with ample RAM is required for this setting

#load up data
data(benchmark_datasets)
datalist_names = c("boston", "triazine", "ozone", "baseball", "wine.red", "ankara", "wine.white", "pole", "compactiv")
##need to reorder boston because all others have response last
boston = boston[, c(2 : 14, 1)]
		
# put the datasets into a hash.
datalist = list()
for (i in 1:length(datalist_names)){
	 data = get(datalist_names[[i]])
	 for (j in 1 : ncol(data)){
		 data[, j] = as.numeric(data[, j])
	 }
	 # Truncate the datasets at n = 1000 to speed up simulations
	 if (nrow(data) > 1000){
		 data = data[1 : 1000, ]
	 }
	 datalist[[i]] = data
	 names(datalist)[i] = datalist_names[i]
}

#how many times we repeat the experiment
NREP = 20
#how many folds to use when estimating out-of-sample RMSE
KFOLDS = 10

#set up objects for simulation
oos_rmse_results = array(NA, c(length(datalist_names), 3, NREP))
rownames(oos_rmse_results) = datalist_names
colnames(oos_rmse_results) = c("bartMachine", "BayesTree", "RF")


k_fold_cv_bayes_tree = function(X, y, k_folds = 5, ...){	
	n = nrow(X)	
	p = ncol(X)
	
	if (k_folds <= 1 || k_folds > n){
		stop("The number of folds must be at least 2 and less than or equal to n, use \"Inf\" for leave one out")
	}
	
	if (k_folds == Inf){ #leave-one-out
		k_folds = n
	}	
	
	holdout_size = round(n / k_folds)
	split_points = seq(from = 1, to = n, by = holdout_size)[1 : k_folds]
	
	L1_err = 0
	L2_err = 0
	
	Xy = data.frame(X, y) ##set up data
	
	for (k in 1 : k_folds){
		cat(".")
		holdout_index_i = split_points[k]
		holdout_index_f = ifelse(k == k_folds, n, split_points[k + 1] - 1)
		
		test_data_k = Xy[holdout_index_i : holdout_index_f, ]
		training_data_k = Xy[-c(holdout_index_i : holdout_index_f), ]
		
		
		#build bart object
		rbart = bart(x.train = training_data_k[, 1 : p], y.train = as.numeric(training_data_k[, (p + 1)]), x.test = test_data_k[, 1 : p], verbose = FALSE, ntree = 50)
		y_hat = rbart$yhat.test.mean
		
		#tabulate errors
		y_test_k = test_data_k[, (p + 1)]
		L1_err = L1_err + sum(abs(y_test_k - y_hat))
		L2_err = L2_err + sum((y_test_k - y_hat)^2)
	}
	cat("\n")
	list(L1_err = L1_err, L2_err = L2_err, rmse = sqrt(L2_err / n), PseudoRsq = 1 - L2_err / sum((y - mean(y))^2))	
}

k_fold_cv_rf = function(X, y, k_folds = 5, ...){	
	n = nrow(X)	
	p = ncol(X)
	
	if (k_folds <= 1 || k_folds > n){
		stop("The number of folds must be at least 2 and less than or equal to n, use \"Inf\" for leave one out")
	}
	
	if (k_folds == Inf){ #leave-one-out
		k_folds = n
	}	
	
	holdout_size = round(n / k_folds)
	split_points = seq(from = 1, to = n, by = holdout_size)[1 : k_folds]
	
	L1_err = 0
	L2_err = 0
	
	Xy = data.frame(X, y) ##set up data
	
	for (k in 1 : k_folds){
		cat(".")
		holdout_index_i = split_points[k]
		holdout_index_f = ifelse(k == k_folds, n, split_points[k + 1] - 1)
		
		test_data_k = Xy[holdout_index_i : holdout_index_f, ]
		training_data_k = Xy[-c(holdout_index_i : holdout_index_f), ]
		
		
		#build bart object
		rf = randomForest(x = training_data_k[, 1 : p], y = as.numeric(training_data_k[, (p + 1)]), verbose = FALSE)
		y_hat = predict(rf, test_data_k)
		
		#tabulate errors
		y_test_k = test_data_k[, (p + 1)]
		L1_err = L1_err + sum(abs(y_test_k - y_hat))
		L2_err = L2_err + sum((y_test_k - y_hat)^2)
	}
	cat("\n")
	list(L1_err = L1_err, L2_err = L2_err, rmse = sqrt(L2_err / n), PseudoRsq = 1 - L2_err / sum((y - mean(y))^2))	
}

#simulate NREP times for each dataset, print results as they come in
for (nrep in 1 : NREP){
	for (dname in datalist_names){
		data = datalist[[dname]]
		X = data[, 1 : (ncol(data)- 1)]
		y = data[, ncol(data)] ##response is last
		
		rmse_bart_machine = k_fold_cv(X, y, k_folds = KFOLDS, verbose = FALSE)
		oos_rmse_results[dname, 1, nrep] = rmse_bart_machine$rmse
		
		rmse_rbart = k_fold_cv_bayes_tree(X, y, k_folds = KFOLDS)
		oos_rmse_results[dname, 2, nrep] = rmse_rbart$rmse
		
		rmse_rf = k_fold_cv_rf(X, y, k_folds = KFOLDS)
		oos_rmse_results[dname, 3, nrep] = rmse_rf$rmse
		
	}	
	print(oos_rmse_results[,, nrep])	
}
save(oos_rmse_results, file = "oos_rmse_results.RData")


oos_rmse_results_avg = apply(oos_rmse_results, c(1, 2), mean)

#table B1
library(xtable)
xtable(round(oos_rmse_results_avg, 3))

#do naive comparisons
alpha_bonferroni = 0.05 / nrow(oos_rmse_results)
t.test(oos_rmse_results[1, 1 ,], oos_rmse_results[1, 2 ,])$p.value < alpha_bonferroni
t.test(oos_rmse_results[2, 1 ,], oos_rmse_results[2, 2 ,])$p.value < alpha_bonferroni
t.test(oos_rmse_results[3, 1 ,], oos_rmse_results[3, 2 ,])$p.value < alpha_bonferroni
t.test(oos_rmse_results[4, 1 ,], oos_rmse_results[4, 2 ,])$p.value < alpha_bonferroni
t.test(oos_rmse_results[5, 1 ,], oos_rmse_results[5, 2 ,])$p.value < alpha_bonferroni
t.test(oos_rmse_results[6, 1 ,], oos_rmse_results[6, 2 ,])$p.value < alpha_bonferroni
t.test(oos_rmse_results[7, 1 ,], oos_rmse_results[7, 2 ,])$p.value < alpha_bonferroni
t.test(oos_rmse_results[8, 1 ,], oos_rmse_results[8, 2 ,])$p.value < alpha_bonferroni
t.test(oos_rmse_results[9, 1 ,], oos_rmse_results[9, 2 ,])$p.value < alpha_bonferroni

#add the TRUE's to the table as *'s (in the appropriate column)
