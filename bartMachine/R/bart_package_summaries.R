##give summary info about bart
summary.bartMachine = function(object, ...){	
	cat(paste("bartMachine v", VERSION, ifelse(object$pred_type == "regression", " for regression", " for classification"), "\n\n", sep = ""))
	if (object$use_missing_data){
		cat("Missing data feature ON\n")
	}
	#first print out characteristics of the training data
	cat(paste("training data n =", object$n, "and p =", object$p, "\n"))
	
  ##build time
	ttb = as.numeric(object$time_to_build, units = "secs")
	if (ttb > 60){
		ttb = as.numeric(object$time_to_build, units = "mins")
		cat(paste("built in", round(ttb, 2), "mins on", object$num_cores, ifelse(object$num_cores == 1, "core,", "cores,"), object$num_trees, "trees,", object$num_burn_in, "burn-in and", object$num_iterations_after_burn_in, "post. samples\n"))
	} else {
		cat(paste("built in", round(ttb, 1), "secs on", object$num_cores, ifelse(object$num_cores == 1, "core,", "cores,"), object$num_trees, "trees,", object$num_burn_in, "burn-in and", object$num_iterations_after_burn_in, "post. samples\n"))
	}
	
	if (object$pred_type == "regression"){
		sigsq_est = sigsq_est(object) ##call private function
		cat(paste("\nsigsq est for y beforehand:", round(object$sig_sq_est, 3), "\n"))
		cat(paste("avg sigsq estimate after burn-in:", round(sigsq_est, 5), "\n"))
		
		if (object$run_in_sample){
			cat("\nin-sample statistics:\n")
			cat(paste(" L1 =", round(object$L1_err_train, 2), "\n",
							"L2 =", round(object$L2_err_train, 2), "\n",
							"rmse =", round(object$rmse_train, 2), "\n"),
					"Pseudo-Rsq =", round(object$PseudoRsq, 4))
			
			es = object$residuals
			normal_p_val = shapiro.test(es)$p.value
			cat("\np-val for shapiro-wilk test of normality of residuals:", round(normal_p_val, 5), "\n")
			
			centered_p_val = t.test(es)$p.value
			cat("p-val for zero-mean noise:", round(centered_p_val, 5), "\n")	
		} else {
			cat("\nno in-sample information available (use option run_in_sample = TRUE next time)\n")
		}		
	} else if (object$pred_type == "classification"){
		if (object$run_in_sample){
			cat("\nconfusion matrix:\n\n")
			print(object$confusion_matrix)
		} else {
			cat("\nno in-sample information available (use option run_in_sample = TRUE next time)\n")
		}		
	}
	cat("\n")
}

#alias for summary
print.bartMachine = function(x, ...){ #alias for summary
	summary(x)
}