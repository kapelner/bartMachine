#S3 predict method
predict.bartMachine = function(object, new_data, type = "prob", prob_rule_class = NULL, ...){
	if (is_bart_destroyed(object)){
		stop("This BART machine has been destroyed. Please recreate.")
	}
	if(!(type %in% c("prob", "class"))){
		stop("For classification, type must be either \"prob\" or \"class\". ")
	}
  
	if (object$pred_type == "regression"){	
		bart_machine_get_posterior(object, new_data)$y_hat
	} else { ##classification
	    if (type == "prob"){
	    	bart_machine_get_posterior(object, new_data)$y_hat
	    } else {
	    	labels = bart_machine_get_posterior(object, new_data)$y_hat > ifelse(is.null(prob_rule_class), object$prob_rule_class, prob_rule_class)
	      	#return whatever the raw y_levels were
	      	labels_to_y_levels(object, labels)      
	    }
	}	
}

##private function
labels_to_y_levels = function(bart_machine, labels){
	ifelse(labels == 0, bart_machine$y_levels[1], bart_machine$y_levels[2])
}

##utility function for predicting when test outcomes are known
bart_predict_for_test_data = function(bart_machine, Xtest, ytest){
	if (is_bart_destroyed(bart_machine)){
		stop("This BART machine has been destroyed. Please recreate.")
	}
	
	
	if (bart_machine$pred_type == "regression"){ #regression list
	  ytest_hat = predict(bart_machine, Xtest)
		n = nrow(Xtest)
		L2_err = sum((ytest - ytest_hat)^2)
		
		list(
				y_hat = ytest_hat,
				L1_err = sum(abs(ytest - ytest_hat)),
				L2_err = L2_err,
				rmse = sqrt(L2_err / n),
				e = ytest - ytest_hat
		)
	} else { ##classification list
	  ytest_hat = predict(bart_machine, Xtest, type = "class")
		confusion_matrix = as.data.frame(matrix(NA, nrow = 3, ncol = 3))
		rownames(confusion_matrix) = c(paste("actual", bart_machine$y_levels), "use errors")
		colnames(confusion_matrix) = c(paste("predicted", bart_machine$y_levels), "model errors")		
		confusion_matrix[1 : 2, 1 : 2] = as.integer(table(ytest, ytest_hat)) 
		confusion_matrix[3, 1] = round(confusion_matrix[2, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1]), 3)
		confusion_matrix[3, 2] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 2] + confusion_matrix[2, 2]), 3)
		confusion_matrix[1, 3] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 1] + confusion_matrix[1, 2]), 3)
		confusion_matrix[2, 3] = round(confusion_matrix[2, 1] / (confusion_matrix[2, 1] + confusion_matrix[2, 2]), 3)
		confusion_matrix[3, 3] = round((confusion_matrix[1, 2] + confusion_matrix[2, 1]) / sum(confusion_matrix[1 : 2, 1 : 2]), 3)
		
		list(y_hat = ytest_hat, confusion_matrix = confusion_matrix)
	}
}

##get full set of samples from posterior distribution of f(x)
bart_machine_get_posterior = function(bart_machine, new_data){
	if (is_bart_destroyed(bart_machine)){
		stop("This BART machine has been destroyed. Please recreate.")
	}	
	if (class(new_data) != "data.frame"){		
		stop("\"new_data\" needs to be a data frame with the same column names as the training data.")
	}
	if (!bart_machine$use_missing_data){
		nrow_before = nrow(new_data)
		new_data = na.omit(new_data)
		if (nrow_before > nrow(new_data)){
			cat(nrow_before - nrow(new_data), "rows omitted due to missing data. Try using the missing data feature in \"build_bart_machine\" to be able to predict on all observations.\n")
		}
	}
	
	if (nrow(new_data) == 0){
		stop("No rows to predict.\n")
	}
	#pull out data objects for convenience
	java_bart_machine = bart_machine$java_bart_machine
	num_iterations_after_burn_in = bart_machine$num_iterations_after_burn_in
	n = nrow(new_data)
	
	#check for errors in data
	#
	#now process and make dummies if necessary
	new_data = pre_process_new_data(new_data, bart_machine)
	
	#check for missing data if this feature was not turned on
	if (!bart_machine$use_missing_data){
		M = matrix(0, nrow = nrow(new_data), ncol = ncol(new_data))
		for (i in 1 : nrow(new_data)){
			for (j in 1 : ncol(new_data)){
				if (is.missing(new_data[i, j])){
					M[i, j] = 1
				}
			}
		}
		if (sum(M) > 0){
			warning("missing data found in test data and BART was not built with missing data feature!\n")
		}		
	}
	
	y_hat_posterior_samples = 
		t(sapply(.jcall(bart_machine$java_bart_machine, "[[D", "getGibbsSamplesForPrediction", .jarray(new_data, dispatch = TRUE), as.integer(bart_machine_num_cores())), .jevalArray))
	
	#to get y_hat.. just take straight mean of posterior samples, alternatively, we can let java do it if we want more bells and whistles
	y_hat = rowMeans(y_hat_posterior_samples)
	
	list(y_hat = y_hat, X = new_data, y_hat_posterior_samples = y_hat_posterior_samples)
}

##compute credible intervals
calc_credible_intervals = function(bart_machine, new_data, ci_conf = 0.95){
  	if (is_bart_destroyed(bart_machine)){
    	stop("This BART machine has been destroyed. Please recreate.")
    }
  
	#first convert the rows to the correct dummies etc
	new_data = pre_process_new_data(new_data, bart_machine)
	n_test = nrow(new_data)
	
	ci_lower_bd = array(NA, n_test)
	ci_upper_bd = array(NA, n_test)	
	
	y_hat_posterior_samples = ##get samples
		t(sapply(.jcall(bart_machine$java_bart_machine, "[[D", "getGibbsSamplesForPrediction",  .jarray(new_data, dispatch = TRUE), as.integer(bart_machine_num_cores())), .jevalArray))
	
	#to get y_hat.. just take straight mean of posterior samples, alternatively, we can let java do it if we want more bells and whistles
	y_hat = rowMeans(y_hat_posterior_samples)
	
	for (i in 1 : n_test){		
		ci_lower_bd[i] = quantile(sort(y_hat_posterior_samples[i, ]), (1 - ci_conf) / 2)
		ci_upper_bd[i] = quantile(sort(y_hat_posterior_samples[i, ]), (1 + ci_conf) / 2)
	}
	#put them together and return
	cbind(ci_lower_bd, ci_upper_bd)
}

##compute prediction intervals
calc_prediction_intervals = function(bart_machine, new_data, Z_new_data = NULL, pi_conf = 0.95, num_samples_per_data_point = 1000){

	if (bart_machine$pred_type == "classification"){
		stop("Prediction intervals are not possible for classification.")
	}
    if (is_bart_destroyed(bart_machine)){
    	stop("This BART machine has been destroyed. Please recreate.")
  	}
	if (!bart_machine$use_heteroskedastic_linear_model && !is.null(Z_new_data)){
		stop("You cannot specify Z_new_data for homoskedastic models")
	}
	if (is.null(Z_new_data)){ #default the Z's to the same data
		Z_new_data = new_data
	}
	Z_new_data = as.matrix(Z_new_data) #need to make sure we can call rows and cols on it
  
	#first convert the rows to the correct dummies etc
	new_data = pre_process_new_data(new_data, bart_machine)
	n_test = nrow(new_data)
	
	pi_lower_bd = array(NA, n_test)
	pi_upper_bd = array(NA, n_test)	
	
	y_hat_posterior_samples = 
		t(sapply(.jcall(bart_machine$java_bart_machine, "[[D", "getGibbsSamplesForPrediction", .jarray(new_data, dispatch = TRUE), as.integer(bart_machine_num_cores())), .jevalArray))
	sigsqs = .jcall(bart_machine$java_bart_machine, "[D", "getGibbsSamplesSigsqs") #sigsqs are the sigsqs multiples that modify the gamma linear model in the hBART case

	if (bart_machine$use_heteroskedastic_linear_model){
		gammas_all_gibbs = get_gammas_hetero(bart_machine)
		Z_col_means = bart_machine$Z_col_means
	}
	
	all_prediction_samples = matrix(NA, nrow = n_test, ncol = num_samples_per_data_point)
	for (i in 1 : n_test){
		
		#get all the y_hats in the posterior for this datapoint
		y_hats = y_hat_posterior_samples[i, ]
		if (bart_machine$use_heteroskedastic_linear_model){
			z_i_centered = as.numeric(Z_new_data[i, ]) - Z_col_means
		}
		
		n_gs = sample(1 : bart_machine$num_iterations_after_burn_in, num_samples_per_data_point, replace = TRUE)
		#now make num_samples_per_data_point draws from y_hat
		for (k in 1 : num_samples_per_data_point){
			#draw a gibbs sample
			y_hat_draw = y_hats[n_gs[k]]
			sigsq_draw = sigsqs[n_gs[k]]
			
			if (bart_machine$use_heteroskedastic_linear_model){
				gammas_draw = as.numeric(gammas_all_gibbs[n_gs[k], ])
				
				#cat("k", k, "y_hat_draw", y_hat_draw, "a_draw", sigsq_draw, "gammas_draw", gammas_draw, "e^zg", exp(sum(z_i_centered * gammas_draw)))
				#the sigsq_draw is the multiple in the expression below
				sigsq_draw = sigsq_draw * exp(sum(z_i_centered * gammas_draw)) #the multiple is modified by the linear model
				#cat(" sigsq_draw", sigsq_draw, "\n")
			}
			all_prediction_samples[i, k] = rnorm(1, mean = y_hat_draw, sd = sqrt(sigsq_draw))	
		}
	}
	
	for (i in 1 : n_test){
		pi_lower_bd[i] = quantile(c(all_prediction_samples[i, ]), (1 - pi_conf) / 2) #fun fact: the "c" function is overloaded to vectorize an array
		pi_upper_bd[i] = quantile(c(all_prediction_samples[i, ]), (1 + pi_conf) / 2)
	}
	#put them together and return
	cbind(pi_lower_bd, pi_upper_bd)
}