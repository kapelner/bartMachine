##performs out-of-sample error estimation for a BART model
k_fold_cv = function(X, y, k_folds = 5, verbose = FALSE, ...){
	if (class(X) != "data.frame"){
		stop("The training data X must be a data frame.")
	}
	if (!(class(y) %in% c("numeric", "integer", "factor"))){
		stop("Your response must be either numeric, an integer or a factor with two levels.\n")
	}	
	
	y_levels = levels(y)
	if (class(y) == "numeric" || class(y) == "integer"){ #if y is numeric, then it's a regression problem
		pred_type = "regression"
	} else if (class(y) == "factor" & length(y_levels) == 2){ #if y is a factor and and binary, then it's a classification problem
		pred_type = "classification"
	}
	
	n = nrow(X)
	Xpreprocess = pre_process_training_data(X)$data
	
	p = ncol(Xpreprocess)
	
	if (k_folds == Inf){ #leave-one-out
		k_folds = n
	}
	
	if (k_folds <= 1 || k_folds > n){
		stop("The number of folds must be at least 2 and less than or equal to n, use \"Inf\" for leave one out")
	}
	
	holdout_size = round(n / k_folds)
	split_points = seq(from = 1, to = n, by = holdout_size)[1 : k_folds]
	
	if (pred_type == "regression"){
		L1_err = 0
		L2_err = 0
	} else {
		confusion_matrix = matrix(0, nrow = 3, ncol = 3)
		rownames(confusion_matrix) = c(paste("actual", y_levels), "use errors")
		colnames(confusion_matrix) = c(paste("predicted", y_levels), "model errors")
	}
	
	Xy = data.frame(Xpreprocess, y) ##set up data
	
	for (k in 1 : k_folds){
		cat(".")
		holdout_index_i = split_points[k]
		holdout_index_f = ifelse(k == k_folds, n, split_points[k + 1] - 1)
		#cat("holdout_index_i", holdout_index_i, "holdout_index_f", holdout_index_f, "holdout_size", holdout_size, "split_points", split_points)
		test_data_k = Xy[holdout_index_i : holdout_index_f, ]
		training_data_k = Xy[-c(holdout_index_i : holdout_index_f), ]
		
		#we cannot afford the time sink of serialization during the grid search, so shut it off manually
		args = list(...)
		args$serialize = FALSE
		
   		#build bart object
		bart_machine_cv = do.call(build_bart_machine, c(list(
							x = training_data_k[, 1 : p, drop = FALSE], 
							y = training_data_k[, (p + 1)], 
							run_in_sample = FALSE, 
							verbose = verbose), args))
		predict_obj = bart_predict_for_test_data(bart_machine_cv, test_data_k[, 1 : p, drop = FALSE], test_data_k[, (p + 1)])
		destroy_bart_machine(bart_machine_cv)
		
		#tabulate errors
		if (pred_type == "regression"){
			L1_err = L1_err + predict_obj$L1_err
			L2_err = L2_err + predict_obj$L2_err
		} else {
			tab = table(factor(test_data_k$y, levels = y_levels), factor(predict_obj$y_hat, levels = y_levels))
#			tab = table(c(test_data_k$y, y_levels[1], y_levels[2]), c(predict_obj$y_hat, y_levels[1], y_levels[2]))
#			tab = tab + diag(2)
#			tab = tab - diag(2)
		print(tab)
			confusion_matrix[1 : 2, 1 : 2] = confusion_matrix[1 : 2, 1 : 2] + tab
		}
	}
	cat("\n")
	if (pred_type == "regression"){
		list(L1_err = L1_err, L2_err = L2_err, rmse = sqrt(L2_err / n), PseudoRsq = 1 - L2_err / sum((y - mean(y))^2))
	} else {			
		#calculate the rest of the confusion matrix and return it plus the errors
		confusion_matrix[3, 1] = round(confusion_matrix[2, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1]), 3)
		confusion_matrix[3, 2] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 2] + confusion_matrix[2, 2]), 3)
		confusion_matrix[1, 3] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 1] + confusion_matrix[1, 2]), 3)
		confusion_matrix[2, 3] = round(confusion_matrix[2, 1] / (confusion_matrix[2, 1] + confusion_matrix[2, 2]), 3)
		confusion_matrix[3, 3] = round((confusion_matrix[1, 2] + confusion_matrix[2, 1]) / sum(confusion_matrix[1 : 2, 1 : 2]), 3)
		list(confusion_matrix = confusion_matrix, misclassification_error = confusion_matrix[3, 3])
	}
	
}

