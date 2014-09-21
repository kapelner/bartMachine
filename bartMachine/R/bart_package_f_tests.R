##function to permute columns of X and check BART's performance
cov_importance_test = function(bart_machine, covariates = NULL, num_permutation_samples = 100, plot = TRUE){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	#be able to handle regular expressions to find the covariates
	
	all_covariates = bart_machine$training_data_features_with_missing_features
	
	if (is.null(covariates)){
		title = "bartMachine omnibus test for covariate importance\n"
	} else if (length(covariates) <= 3){
		if (class(covariates[1]) == "numeric"){
			cov_names = paste(all_covariates[covariates], collapse = ", ")
		} else {
			cov_names = paste(covariates, collapse = ", ")
		}
		title = paste("bartMachine test for importance of covariate(s):", cov_names, "\n")
	} else {
		title = paste("bartMachine test for importance of", length(covariates), "covariates", "\n")
	}
	cat(title)
	observed_error_estimate = ifelse(bart_machine$pred_type == "regression", bart_machine$PseudoRsq, bart_machine$misclassification_error)
		
	permutation_samples_of_error = array(NA, num_permutation_samples)
	for (nsim in 1 : num_permutation_samples){
		cat(".")
		if (nsim %% 50 == 0){
			cat("\n")
		}	
		#omnibus F-like test - just permute y (same as permuting ALL the columns of X and it's faster)
		if (is.null(covariates)){
			bart_machine_samp = bart_machine_duplicate(bart_machine, y = sample(bart_machine$y), run_in_sample = TRUE, verbose = FALSE) #we have to turn verbose off otherwise there would be too many outputs
		#partial F-like test - permute the columns that we're interested in seeing if they matter
		} else {
			X_samp = bart_machine$X #copy original design matrix
			
			covariates_left_to_permute = c()
			for (cov in covariates){
				if (cov %in% colnames(X_samp)){
					X_samp[, cov] = sample(X_samp[, cov])
				} else {
					covariates_left_to_permute = c(covariates_left_to_permute, cov)
				}
			}

			bart_machine_samp = bart_machine_duplicate(bart_machine, X = X_samp, covariates_to_permute = covariates_left_to_permute, run_in_sample = TRUE, verbose = FALSE) #we have to turn verbose off otherwise there would be too many outputs
		}
		#record permutation result
		permutation_samples_of_error[nsim] = ifelse(bart_machine$pred_type == "regression", bart_machine_samp$PseudoRsq, bart_machine_samp$misclassification_error)	
	}
	cat("\n")
  
	##compute p-value
	pval = ifelse(bart_machine$pred_type == "regression", sum(observed_error_estimate < permutation_samples_of_error), sum(observed_error_estimate > permutation_samples_of_error)) / (num_permutation_samples + 1)
	
	if (plot){
		hist(permutation_samples_of_error, 
				xlim = c(min(permutation_samples_of_error, 0.99 * observed_error_estimate), max(permutation_samples_of_error, 1.01 * observed_error_estimate)),
				xlab = paste("permutation samples\n pval = ", round(pval, 3)),
				br = num_permutation_samples / 10,
				main = paste(title, "Null Samples of", ifelse(bart_machine$pred_type == "regression", "Pseudo-R^2's", "Misclassification Errors")))
		abline(v = observed_error_estimate, col = "blue", lwd = 3)
	}
	cat("p_val = ", pval, "\n")
	invisible(list(permutation_samples_of_error = permutation_samples_of_error, observed_error_estimate = observed_error_estimate, pval = pval))
}

