BART_MAX_MEM_MB_DEFAULT = 1100 #1.1GB is the most a 32bit machine can give without throwing an error or crashing
BART_NUM_CORES_DEFAULT = 1 #Stay conservative as a default

#' @rdname bartMachine
##build a BART model
build_bart_machine = function(X = NULL, y = NULL, Xy = NULL, 
		num_trees = 50, #found many times to not get better after this value... so let it be the default, it's faster too 
		num_burn_in = 250, 
		num_iterations_after_burn_in = 1000, 
		alpha = 0.95,
		beta = 2,
		k = 2,
		q = 0.9,
		nu = 3.0,
		prob_rule_class = 0.5,
		mh_prob_steps = c(2.5, 2.5, 4) / 9, #only the first two matter
		debug_log = FALSE,
		run_in_sample = TRUE,
		s_sq_y = "mse", # "mse" or "var"
		sig_sq_est = NULL, #you can pass this in to speed things up if you have an idea about what you want to use a priori
		print_tree_illustrations = FALSE, #POWER USERS ONLY
		cov_prior_vec = NULL,
		interaction_constraints = NULL,
		use_missing_data = FALSE,
		covariates_to_permute = NULL, #PRIVATE
		num_rand_samps_in_library = 10000, #give the user the option to make a bigger library of random samples of normals and inv-gammas
		use_missing_data_dummies_as_covars = FALSE,
		replace_missing_data_with_x_j_bar = FALSE,
		impute_missingness_with_rf_impute = FALSE,
		impute_missingness_with_x_j_bar_for_lm = TRUE,
		mem_cache_for_speed = TRUE,
		flush_indices_to_save_RAM = TRUE,
		serialize = FALSE,
		seed = NULL,
		verbose = TRUE){

	if (verbose){
		cat("bartMachine initializing with", num_trees, "trees...\n")	
	}	
	t0 = Sys.time()
	
	if (use_missing_data_dummies_as_covars && replace_missing_data_with_x_j_bar){
		stop("You cannot impute by averages and use missing data as dummies simultaneously.")
	}
	
	if ((is.null(X) && is.null(Xy)) || is.null(y) && is.null(Xy)){
		stop("You need to give bartMachine a training set either by specifying X and y or by specifying a matrix Xy which contains the response named \"y.\"\n")
	} else if (!is.null(X) && !is.null(y) && !is.null(Xy)){
		stop("You cannot specify both X,y and Xy simultaneously.")		
	} else if (is.null(X) && is.null(y)){ #they specified Xy, so now just pull out X,y
		#first ensure it's a dataframe
		if (!inherits(Xy, "data.frame")){
			stop(paste("The training data Xy must be a data frame."), call. = FALSE)	
		}
		y = Xy[, ncol(Xy)]
		for (cov in 1 : (ncol(Xy) - 1)){
			if (colnames(Xy)[cov] == ""){
				colnames(Xy)[cov] = paste("V", cov, sep = "")
			}
		}
		X = as.data.frame(Xy[, 1 : (ncol(Xy) - 1)])
		colnames(X) = colnames(Xy)[1 : (ncol(Xy) - 1)]
	}
	
	#make sure it's a data frame
	if (!inherits(X, "data.frame")){
		stop(paste("The training data X must be a data frame."), call. = FALSE)	
	}
	if (verbose){
		cat("bartMachine vars checked...\n")
	}	
	#we are about to construct a bartMachine object. First, let R garbage collect
	#to clean up previous bartMachine objects that are no longer in use. This is important
	#because R's garbage collection system does not "see" the size of Java objects. Thus,
	#you are at risk of running out of memory without this invocation. 
	gc() #Delete at your own risk!	

	#now take care of classification or regression
	y_levels = levels(y)
	if (inherits(y, "numeric") || inherits(y, "integer")){ #if y is numeric, then it's a regression problem
		if (inherits(y, "integer")){
			cat("Warning: The response y is integer, bartMachine will run regression.\n")
		}
		#java expects doubles, not ints, so we need to cast this now to avoid errors later
		if (inherits(y, "integer")){
			y = as.numeric(y)
		}		
		java_bart_machine = .jnew("bartMachine.bartMachineRegressionMultThread")
		y_remaining = y
		pred_type = "regression"
	} else if (inherits(y, "factor") & length(y_levels) == 2){ #if y is a factor and binary
		#convenience for users that use 0/1 variables to ensure positive category is first as a level and a label (i.e. the naive expectation)
		if (all(sort(levels(factor(y))) == c("0", "1"))){
			y = factor(y, levels = c(1, 0), labels = c(1, 0))
			y_levels = levels(y)
		}		
		java_bart_machine = .jnew("bartMachine.bartMachineClassificationMultThread")
		y_remaining = ifelse(y == y_levels[1], 1, 0)
		pred_type = "classification"
	} else { #otherwise throw an error
		stop("Your response must be either numeric, an integer or a factor with two levels.\n")
	}
	
	num_gibbs = num_burn_in + num_iterations_after_burn_in
	
	if (ncol(X) == 0){
		stop("Your data matrix must have at least one attribute.")
	}
	if (nrow(X) == 0){
		stop("Your data matrix must have at least one observation.")
	}
	if (length(y) != nrow(X)){
		stop("The number of responses must be equal to the number of observations in the training data.")
	}
	if (verbose){
		cat("bartMachine java init...\n")
	}
	
	#if no column names, make up names
	if (is.null(colnames(X))){
		colnames(X) = paste("V", seq(from = 1, to = ncol(X), by = 1), sep = "")
	}
	
	if (any(mh_prob_steps < 0)){
		stop("The grow, prune, change ratio parameter vector must all be greater than 0.")
	}
	
	#now we should regenerate the factors for the factor columns
	predictors_which_are_factors = names(which(sapply(X, is.factor)))
	for (predictor in predictors_which_are_factors){
		X[, predictor] = factor(X[, predictor])
	}
	if (verbose){
		cat("bartMachine factors created...\n")
	}
	
	if (sum(is.na(y_remaining)) > 0){
		stop("You cannot have any missing data in your response vector.")
	}
	
	rf_imputations_for_missing = NULL
	if (impute_missingness_with_rf_impute){
		if (nrow(na.omit(X)) == nrow(X)){ #for the cases where it doesn't impute
			warning("No missing entries in the training data to impute.")
			rf_imputations_for_missing = X
		} else {
			#just use cols that HAVE missing data
			predictor_colnums_with_missingness = names(which(colSums(is.na(X)) > 0))
			
			rf_imputations_for_missing = rfImpute(X, y)
			rf_imputations_for_missing = rf_imputations_for_missing[, 2 : ncol(rf_imputations_for_missing)]
			rf_imputations_for_missing = rf_imputations_for_missing[, predictor_colnums_with_missingness]
		}
		colnames(rf_imputations_for_missing) = paste(colnames(rf_imputations_for_missing), "_imp", sep = "")
		if (verbose){
			cat("bartMachine after rf imputations...\n")
		}	
	}
	
	#if we're not using missing data, go on and get rid of it
	if (!use_missing_data && !replace_missing_data_with_x_j_bar){
		rows_before = nrow(X)
		X = na.omit(X)
		rows_after = nrow(X)
		if (rows_before - rows_after > 0){
			stop("You have ", rows_before - rows_after, " observations with missing data. \nYou must either omit your missing data using \"na.omit()\" or turn on the\n\"use_missing_data\" or \"replace_missing_data_with_x_j_bar\" feature in order to use bartMachine.\n")
		}
	} else if (replace_missing_data_with_x_j_bar){
		X = imputeMatrixByXbarjContinuousOrModalForBinary(X, X)
		if (verbose){
			cat("Imputed missing data using attribute averages.\n")
		}
	}
	if (verbose){
		cat("bartMachine before preprocess...\n")
	}
	
	pre_process_obj = pre_process_training_data(X, use_missing_data_dummies_as_covars, rf_imputations_for_missing)
	model_matrix_training_data = cbind(pre_process_obj$data, y_remaining)
	p = ncol(model_matrix_training_data) - 1 # we subtract one because we tacked on the response as the last column
	factor_lengths = pre_process_obj$factor_lengths
	if (verbose){
		cat("bartMachine after preprocess...", p, "total features...\n")
	}
	#now create a default cov_prior_vec that factors in the levels of the factors
	null_cov_prior_vec = is.null(cov_prior_vec)
	if (null_cov_prior_vec && length(factor_lengths) > 0){
		#begin with the uniform
		cov_prior_vec = rep(1, p)
		j_factor_begin = p - sum(factor_lengths) + 1
		for (l in 1 : length(factor_lengths)){
			factor_length = factor_lengths[l]
			cov_prior_vec[j_factor_begin : (j_factor_begin + factor_length - 1)] = 1 / factor_length
			j_factor_begin = j_factor_begin + factor_length
		}
	}	
	
	if (!is.null(interaction_constraints)){
		if (!mem_cache_for_speed){
			stop("In order to use interaction constraints, \"mem_cache_for_speed\" must be set to TRUE.")
		}
		if (!inherits(interaction_constraints, "list")){
			stop("specified parameter \"interaction_constraints\" must be a list")
		} else if (length(interaction_constraints) == 0){
			stop("interaction_constraints list cannot be empty")
		}		
		
		for (a in 1 : length(interaction_constraints)){
			vars_a = interaction_constraints[[a]]
			#check if the constraint components are valid features
			for (b in 1 : length(vars_a)){
				var = vars_a[b]
				if ((inherits(var, "numeric") | inherits(var, "integer")) & !(var %in% (1 : p))){
					stop(paste("Element", var, "in interaction_constraints vector number", a, "is numeric but not one of 1, ...,", p, "where", p, "is the number of columns in X."))
				}
				if (inherits(var, "factor")){
					var = as.character(var)
				}
				if (inherits(var, "character")  & !(var %in% colnames(X))){
					stop(paste("Element", var, "in interaction_constraints vector number", a, "is a string but not one of the column names of X."))
				}
				#force all it be integers and begin index at zero
				if (inherits(var, "integer") | inherits(var, "numeric")){
					vars_a[b] = var - 1
				} else if (inherits(var, "character")){
					vars_a[b] = which(colnames(X) == var) - 1
				}
			}
			interaction_constraints[[a]] = as.integer(vars_a)
		}
	}
	#print("interaction_constraints:"); print(interaction_constraints)

	#this is a private parameter ONLY called by cov_importance_test
	if (!is.null(covariates_to_permute)){
		#first check if these covariates are even in the matrix to begin with
		for (cov in covariates_to_permute){
			if (!(cov %in% colnames(model_matrix_training_data)) && inherits(cov, "character")){
				stop("Covariate \"", cov, "\" not found in design matrix.")
			}
		}
		permuted_order = sample(1 : nrow(model_matrix_training_data), nrow(model_matrix_training_data))
		model_matrix_training_data[, covariates_to_permute] = model_matrix_training_data[permuted_order, covariates_to_permute]
	}
	
	#now set whether we want the program to log to a file
	if (debug_log & verbose){
		cat("warning: printing out the log file will slow down the runtime significantly.\n")
		.jcall(java_bart_machine, "V", "writeStdOutToLogFile")
	}
	#set whether we want there to be tree illustrations
	if (print_tree_illustrations){
		cat("warning: printing tree illustrations is excruciatingly slow.\n")
		.jcall(java_bart_machine, "V", "printTreeIllustations")
	}
	
	#set the std deviation of y to use
	if (ncol(model_matrix_training_data) - 1 >= nrow(model_matrix_training_data)){
		if (verbose){
			cat("warning: cannot use MSE of linear model for s_sq_y if p > n. bartMachine will use sample var(y) instead.\n")
		}
		s_sq_y = "var"
		
	}
	
  	##estimate sigma^2 to be given to the BART model
	if (is.null(sig_sq_est)){
		if (pred_type == "regression"){		
			y_range = max(y) - min(y)
			y_trans = (y - min(y)) / y_range - 0.5
			if (s_sq_y == "mse"){
				X_for_lm = as.data.frame(model_matrix_training_data)[1 : (ncol(model_matrix_training_data) - 1)]
				if (impute_missingness_with_x_j_bar_for_lm){
					X_for_lm = imputeMatrixByXbarjContinuousOrModalForBinary(X_for_lm, X_for_lm)
				}
				else if (nrow(na.omit(X_for_lm)) == 0){
					stop("The data does not have enough full records to estimate a naive prediction error. Please rerun with \"impute_missingness_with_x_j_bar_for_lm\" set to true.")
				}
				mod = lm(y_trans ~ ., X_for_lm)
				mse = var(mod$residuals)
				sig_sq_est = as.numeric(mse)
				.jcall(java_bart_machine, "V", "setSampleVarY", sig_sq_est)
			} else if (s_sq_y == "var"){
				sig_sq_est = as.numeric(var(y_trans))
				.jcall(java_bart_machine, "V", "setSampleVarY", sig_sq_est)
			} else { #if it's not a valid flag, throw an error
				stop("s_sq_y must be \"mse\" or \"var\"", call. = FALSE)
			}
			sig_sq_est = sig_sq_est * y_range^2		
			if (verbose){
				cat("bartMachine sigsq estimated...\n") #only print for regression
			}
		}		
	} else {
		if (verbose){
			cat("bartMachine using previous sigsq estimated...\n")
		}		
	}

	
	#if the user hasn't set a number of cores, set it here
	if (!exists("BART_NUM_CORES", envir = bartMachine_globals)){
		assign("BART_NUM_CORES", BART_NUM_CORES_DEFAULT, bartMachine_globals)
	}
	#load the number of cores the user set
	num_cores = get("BART_NUM_CORES", bartMachine_globals)

	
	#build bart to spec with what the user wants
	.jcall(java_bart_machine, "V", "setNumCores", as.integer(num_cores)) #this must be set FIRST!!!
	.jcall(java_bart_machine, "V", "setNumTrees", as.integer(num_trees))
	.jcall(java_bart_machine, "V", "setNumGibbsBurnIn", as.integer(num_burn_in))
	.jcall(java_bart_machine, "V", "setNumGibbsTotalIterations", as.integer(num_gibbs))
	.jcall(java_bart_machine, "V", "setAlpha", alpha)
	.jcall(java_bart_machine, "V", "setBeta", beta)
	.jcall(java_bart_machine, "V", "setK", k)
	.jcall(java_bart_machine, "V", "setQ", q)
	.jcall(java_bart_machine, "V", "setNU", nu)
	mh_prob_steps = mh_prob_steps / sum(mh_prob_steps) #make sure it's a prob vec
	.jcall(java_bart_machine, "V", "setProbGrow", mh_prob_steps[1])
	.jcall(java_bart_machine, "V", "setProbPrune", mh_prob_steps[2])
	.jcall(java_bart_machine, "V", "setVerbose", verbose)
	.jcall(java_bart_machine, "V", "setMemCacheForSpeed", mem_cache_for_speed)
	.jcall(java_bart_machine, "V", "setFlushIndicesToSaveRAM", flush_indices_to_save_RAM)
	
#	cat("seed", seed, "\n")
	if (!is.null(seed)){
		#set the seed in Java
		.jcall(java_bart_machine, "V", "setSeed", as.integer(seed))
		if (num_cores > 1){
			warning("Setting the seed when using parallelization does not result in deterministic output.\nIf you need deterministic output, you must run \"set_bart_machine_num_cores(1)\" and then build the BART model with the set seed.")
		}
	}
	
	#now we need to set random samples
	.jcall(java_bart_machine, "V", "setNormSamples", rnorm(num_rand_samps_in_library))
	n_plus_hyper_nu = nrow(model_matrix_training_data) + nu	
	.jcall(java_bart_machine, "V", "setGammaSamples", rchisq(num_rand_samps_in_library, n_plus_hyper_nu))
	
	if (length(cov_prior_vec) != 0){
		#put in checks here for user to make sure the covariate prior vec is the correct length
		offset = length(cov_prior_vec) - (ncol(model_matrix_training_data) - 1) 
		if (offset < 0){
			warning(paste("covariate prior vector length =", length(cov_prior_vec), "has to be equal to p =", ncol(model_matrix_training_data) - 1, "(the vector was lengthened with 1's)"))
			cov_prior_vec = c(cov_prior_vec, rep(1, -offset))
		}
		if (length(cov_prior_vec) != ncol(model_matrix_training_data) - 1){
			warning(paste("covariate prior vector length =", length(cov_prior_vec), "has to be equal to p =", ncol(model_matrix_training_data) - 1, "(the vector was shortened)"))
			cov_prior_vec = cov_prior_vec[1 : (ncol(model_matrix_training_data) - 1)]		
		}		
		if (sum(cov_prior_vec > 0) != ncol(model_matrix_training_data) - 1){
			stop("covariate prior vector has to have all its elements be positive", call. = FALSE)
			return(TRUE)
		}
		.jcall(java_bart_machine, "V", "setCovSplitPrior", .jarray(as.numeric(cov_prior_vec)))
	}
	
	if (!is.null(interaction_constraints)){
		.jcall(java_bart_machine, "V", "intializeInteractionConstraints", length(interaction_constraints))
		for (interaction_constraint_vector in interaction_constraints){
			for (b in 1 : length(interaction_constraint_vector)){
				.jcall(java_bart_machine, "V", "addInteractionConstraint",
						as.integer(interaction_constraint_vector[b]),
						.jarray(as.integer(interaction_constraint_vector[-b])))
			}
		}
	}
	
	#now load the training data into BART
	for (i in 1 : nrow(model_matrix_training_data)){
		row_as_char = as.character(model_matrix_training_data[i, ])
		row_as_char = replace(row_as_char, is.na(row_as_char), "NA") #this seems to be necessary for some R-rJava-linux distro-Java combinations
		.jcall(java_bart_machine, "V", "addTrainingDataRow", row_as_char)
	}
	.jcall(java_bart_machine, "V", "finalizeTrainingData")
	if (verbose){
		cat("bartMachine training data finalized...\n")
	}
	
	#build the bart machine and let the user know what type of BART this is
	if (verbose){
		cat("Now building bartMachine for", pred_type)
		if (pred_type == "classification"){
			cat(" where \"", y_levels[1], "\" is considered the target level", sep = "")
		}
		cat("...")
		if (length(cov_prior_vec) != 0){
			cat("Covariate importance prior ON. ")
		}
		if (use_missing_data){
			cat("Missing data feature ON. ")
		}
		if (use_missing_data_dummies_as_covars){
			cat("Missingness used as covariates. ")
		}
		if (impute_missingness_with_rf_impute){
			cat("Missing values imputed via rfImpute. ")
		}
		cat("\n")
	}
	.jcall(java_bart_machine, "V", "Build")
	
	#now once it's done, let's extract things that are related to diagnosing the build of the BART model
	
	bart_machine = list(java_bart_machine = java_bart_machine,
			training_data_features = colnames(model_matrix_training_data)[1 : ifelse(use_missing_data && use_missing_data_dummies_as_covars, (p / 2), p)],
			training_data_features_with_missing_features = colnames(model_matrix_training_data)[1 : p], #always return this even if there's no missing features
			X = X,
			y = y,
			y_levels = y_levels,
			pred_type = pred_type,
			model_matrix_training_data = model_matrix_training_data,
			n = nrow(model_matrix_training_data),
			p = p,
			num_cores = num_cores,
			num_trees = num_trees,
			num_burn_in = num_burn_in,
			num_iterations_after_burn_in = num_iterations_after_burn_in, 
			num_gibbs = num_gibbs,
			alpha = alpha,
			beta = beta,
			k = k,
			q = q,
			nu = nu,
			prob_rule_class = prob_rule_class,
			mh_prob_steps = mh_prob_steps,
			s_sq_y = s_sq_y,
			run_in_sample = run_in_sample,
			sig_sq_est = sig_sq_est,
			time_to_build = Sys.time() - t0,
			cov_prior_vec = cov_prior_vec,
			interaction_constraints = interaction_constraints,
			use_missing_data = use_missing_data,
			use_missing_data_dummies_as_covars = use_missing_data_dummies_as_covars,
			replace_missing_data_with_x_j_bar = replace_missing_data_with_x_j_bar,
			impute_missingness_with_rf_impute = impute_missingness_with_rf_impute,
			impute_missingness_with_x_j_bar_for_lm = impute_missingness_with_x_j_bar_for_lm,			
			verbose = verbose,
			serialize = serialize,
			mem_cache_for_speed = mem_cache_for_speed,
			flush_indices_to_save_RAM = flush_indices_to_save_RAM,
			debug_log = debug_log,
			seed = seed,
			num_rand_samps_in_library = num_rand_samps_in_library
	)
	#if the user used a cov prior vec, pass it back
	if (!null_cov_prior_vec){
		bart_machine$cov_prior_vec = cov_prior_vec
	}
	
	#once its done gibbs sampling, see how the training data does if user wants
	if (run_in_sample){
		if (verbose){
			cat("evaluating in sample data...")
		}
		if (pred_type == "regression"){
			y_hat_posterior_samples = 
				.jcall(bart_machine$java_bart_machine, "[[D", "getGibbsSamplesForPrediction", .jarray(model_matrix_training_data, dispatch = TRUE), as.integer(num_cores), simplify = TRUE)
			
			#to get y_hat.. just take straight mean of posterior samples
			y_hat_train = rowMeans(y_hat_posterior_samples)
			#return a bunch more stuff
			bart_machine$y_hat_train = y_hat_train
			bart_machine$residuals = y_remaining - bart_machine$y_hat_train
			bart_machine$L1_err_train = sum(abs(bart_machine$residuals))
			bart_machine$L2_err_train = sum(bart_machine$residuals^2)
			bart_machine$PseudoRsq = 1 - bart_machine$L2_err_train / sum((y_remaining - mean(y_remaining))^2) #pseudo R^2 acc'd to our dicussion with Ed and Shane
			bart_machine$rmse_train = sqrt(bart_machine$L2_err_train / bart_machine$n)
		} else if (pred_type == "classification"){
			p_hat_posterior_samples = 
				.jcall(bart_machine$java_bart_machine, "[[D", "getGibbsSamplesForPrediction", .jarray(model_matrix_training_data, dispatch = TRUE), as.integer(num_cores), simplify = TRUE)
			
			#to get y_hat.. just take straight mean of posterior samples
			p_hat_train = rowMeans(p_hat_posterior_samples)
			y_hat_train = labels_to_y_levels(bart_machine, p_hat_train > prob_rule_class)
			#return a bunch more stuff
			bart_machine$p_hat_train = p_hat_train
			bart_machine$y_hat_train = y_hat_train
			
			#calculate confusion matrix
			confusion_matrix = as.data.frame(matrix(NA, nrow = 3, ncol = 3))
			rownames(confusion_matrix) = c(paste("actual", y_levels), "use errors")
			colnames(confusion_matrix) = c(paste("predicted", y_levels), "model errors")
			
			confusion_matrix[1 : 2, 1 : 2] = as.integer(table(y, y_hat_train)) 
			confusion_matrix[3, 1] = round(confusion_matrix[2, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1]), 3)
			confusion_matrix[3, 2] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 2] + confusion_matrix[2, 2]), 3)
			confusion_matrix[1, 3] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 1] + confusion_matrix[1, 2]), 3)
			confusion_matrix[2, 3] = round(confusion_matrix[2, 1] / (confusion_matrix[2, 1] + confusion_matrix[2, 2]), 3)
			confusion_matrix[3, 3] = round((confusion_matrix[1, 2] + confusion_matrix[2, 1]) / sum(confusion_matrix[1 : 2, 1 : 2]), 3)
			
			bart_machine$confusion_matrix = confusion_matrix
#			bart_machine$num_classification_errors = confusion_matrix[1, 2] + confusion_matrix[2, 1]
			bart_machine$misclassification_error = confusion_matrix[3, 3]
		}
		if (verbose){
			cat("done\n")
		}
	}
	
	
	#Let's serialize the object if the user wishes
	if (serialize){
		cat("serializing in order to be saved for future R sessions...")
		.jcache(bart_machine$java_bart_machine)
		cat("done\n")
	}
	
	#use R's S3 object orientation
	class(bart_machine) = "bartMachine"
	bart_machine
}

##private function that creates a duplicate of an existing bartMachine object.
bart_machine_duplicate = function(bart_machine, X = NULL, y = NULL, cov_prior_vec = NULL, num_trees = NULL, run_in_sample = NULL, covariates_to_permute = NULL, verbose = NULL, ...){	
	if (is.null(X)){
		X = bart_machine$X
	}
	if (is.null(y)){
		y = bart_machine$y
	}
	if (is.null(cov_prior_vec)){
		cov_prior_vec = bart_machine$cov_prior_vec
	}
	if (is.null(num_trees)){
		num_trees = bart_machine$num_trees
	}	
	if (is.null(run_in_sample)){
		run_in_sample = FALSE
	}
	if (is.null(covariates_to_permute)){
		covariates_to_permute = bart_machine$covariates_to_permute
	}
	if (is.null(verbose)){
		verbose = FALSE
	}	
	build_bart_machine(X, y,
		num_trees = num_trees, #found many times to not get better after this value... so let it be the default, it's faster too 
		num_burn_in = bart_machine$num_burn_in, 
		num_iterations_after_burn_in = bart_machine$num_iterations_after_burn_in, 
		alpha = bart_machine$alpha,
		beta = bart_machine$beta,
		k = bart_machine$k,
		q = bart_machine$q,
		nu = bart_machine$nu,
		prob_rule_class = bart_machine$prob_rule_class,
		mh_prob_steps = bart_machine$mh_prob_steps, #only the first two matter
		run_in_sample = run_in_sample,
		s_sq_y =  bart_machine$s_sq_y, # "mse" or "var"
		cov_prior_vec = cov_prior_vec,
		use_missing_data = bart_machine$use_missing_data,
		covariates_to_permute = covariates_to_permute, #PRIVATE
		num_rand_samps_in_library = bart_machine$num_rand_samps_in_library, #give the user the option to make a bigger library of random samples of normals and inv-gammas
		use_missing_data_dummies_as_covars = bart_machine$use_missing_data_dummies_as_covars,
		replace_missing_data_with_x_j_bar = bart_machine$replace_missing_data_with_x_j_bar,
		impute_missingness_with_rf_impute = bart_machine$impute_missingness_with_rf_impute,
		impute_missingness_with_x_j_bar_for_lm = bart_machine$impute_missingness_with_x_j_bar_for_lm,
		mem_cache_for_speed = bart_machine$mem_cache_for_speed,
		serialize = FALSE, #we do not want to waste CPU time here since these are created internally by us
		verbose = verbose)
}

#build a BART-cv model
#' @rdname bartMachineCV
build_bart_machine_cv = function(X = NULL, y = NULL, Xy = NULL, 
		num_tree_cvs = c(50, 200),
		k_cvs = c(2, 3, 5),
		nu_q_cvs = NULL,
		k_folds = 5, 
		folds_vec = NULL, 
		verbose = TRUE,
		...){
	
	if ((is.null(X) && is.null(Xy)) || is.null(y) && is.null(Xy)){
		stop("You need to give bartMachine a training set either by specifying X and y or by specifying a matrix Xy which contains the response named \"y.\"\n")
	} else if (!is.null(X) && !is.null(y) && !is.null(Xy)){
		stop("You cannot specify both X,y and Xy simultaneously.")	
	} else if (is.null(X) && is.null(y)){ #they specified Xy, so now just pull out X,y
		if (!inherits(Xy, "data.frame")){
			stop(paste("The training data Xy must be a data frame."), call. = FALSE)	
		}
		y = Xy$y
		Xy$y = NULL
		X = Xy
	}	
	if (!is.null(folds_vec) & !inherits(folds_vec, "integer")){
		stop("folds_vec must be an a vector of integers specifying the indexes of each folds.")  
	}
	
	y_levels = levels(y)
	if (inherits(y, "numeric") || inherits(y, "integer")){ #if y is numeric, then it's a regression problem
		pred_type = "regression"
	} else if (inherits(y, "factor") & length(y_levels) == 2){ #if y is a factor and and binary, then it's a classification problem
		pred_type = "classification"
	} else { #otherwise throw an error
		stop("Your response must be either numeric, an integer or a factor with two levels.\n")
	}
	
	if (pred_type == "classification"){
		if (!is.null(nu_q_cvs)){
			stop("For classification, \"nu_q_cvs\" must be set to NULL (the default).")
		}
		nu_q_cvs = list(c(3, 0.9)) #ensure we only do this once, the 3 and the 0.9 don't actually matter, they just need to be valid numbers for the hyperparameters
	} else { #i.e. regression... 
		if (is.null(nu_q_cvs)){ #set it equal to a good default if user didn't specify
			nu_q_cvs = list(c(3, 0.9), c(3, 0.99), c(10, 0.75))
		}
	}
	
	min_rmse_num_tree = NULL
	min_rmse_k = NULL
	min_rmse_nu_q = NULL
	min_oos_rmse = Inf
	min_oos_misclassification_error = Inf
	
	cv_stats = matrix(NA, nrow = length(k_cvs) * length(nu_q_cvs) * length(num_tree_cvs), ncol = 6)
	colnames(cv_stats) = c("k", "nu", "q", "num_trees", "oos_error", "% diff with lowest")
	
	#set up k folds
	if (is.null(folds_vec)){ ##if folds were not pre-set:
		n = nrow(X)
	    if (k_folds == Inf){ #leave-one-out
			k_folds = n
	    }
	  
	    if (k_folds <= 1 || k_folds > n){
			stop("The number of folds must be at least 2 and less than or equal to n, use \"Inf\" for leave one out")
	    }
	  
	    temp = rnorm(n)
	  
	    folds_vec = cut(temp, breaks = quantile(temp, seq(0, 1, length.out = k_folds + 1)), 
			  include.lowest= T, labels = FALSE)
	  } else {
		  k_folds = length(unique(folds_vec)) ##otherwise we know the folds, so just get k
	  }
  
    #cross-validate
	run_counter = 1
	for (k in k_cvs){
		for (nu_q in nu_q_cvs){
			for (num_trees in num_tree_cvs){
				
				if (verbose){
					if (pred_type == "regression"){
						cat(paste("  bartMachine CV try: k:", k, "nu, q:", paste(as.numeric(nu_q), collapse = ", "), "m:", num_trees, "\n"))	
					} else {
						cat(paste("  bartMachine CV try: k:", k, "m:", num_trees, "\n"))
					}					
				}
				
				k_fold_results = k_fold_cv(X, y, 
          			k_folds = k_folds,
					folds_vec = folds_vec, ##will hold the cv folds constant 
					num_trees = num_trees,
					k = k,
					nu = nu_q[1],
					q = nu_q[2], 
					verbose = verbose,
					...)
				
				if (pred_type == "regression" && k_fold_results$rmse < min_oos_rmse){
					min_oos_rmse = k_fold_results$rmse					
					min_rmse_k = k
					min_rmse_nu_q = nu_q
					min_rmse_num_tree = num_trees
				} else if (pred_type == "classification" && k_fold_results$misclassification_error < min_oos_misclassification_error){
					min_oos_misclassification_error = k_fold_results$misclassification_error					
					min_rmse_k = k
					min_rmse_nu_q = nu_q
					min_rmse_num_tree = num_trees					
				}
				
				cv_stats[run_counter, 1 : 5] = c(k, nu_q[1], nu_q[2], num_trees, 
					ifelse(pred_type == "regression", k_fold_results$rmse, k_fold_results$misclassification_error))
				run_counter = run_counter + 1
			}
		}
	}
	if (verbose){
		if (pred_type == "regression"){
			cat(paste("  bartMachine CV win: k:", min_rmse_k, "nu, q:", paste(as.numeric(min_rmse_nu_q), collapse = ", "), "m:", min_rmse_num_tree, "\n"))
		} else {
			cat(paste("  bartMachine CV win: k:", min_rmse_k, "m:", min_rmse_num_tree, "\n"))
		}
	}
	#now that we've found the best settings, return that bart machine. It would be faster to have kept this around, but doing it this way saves RAM for speed.
	bart_machine_cv = build_bart_machine(X, y,
			num_trees = min_rmse_num_tree,
			k = min_rmse_k,
			nu = min_rmse_nu_q[1],
			q = min_rmse_nu_q[2], 
			verbose = verbose,
			...)
	
	#give the user some cv_stats ordered by the best (ie lowest) oosrmse
	cv_stats = cv_stats[order(cv_stats[, "oos_error"]), ]
	cv_stats[, 6] = (cv_stats[, 5] - cv_stats[1, 5]) / cv_stats[1, 5] * 100
	bart_machine_cv$cv_stats = cv_stats
  	bart_machine_cv$folds = folds_vec
	bart_machine_cv
}

##private function for filling in missing data with averages for cont. vars and modes for cat. vars
imputeMatrixByXbarjContinuousOrModalForBinary = function(X_with_missing, X_for_calculating_avgs){
	for (i in 1 : nrow(X_with_missing)){
		for (j in 1 : ncol(X_with_missing)){
			if (is.na(X_with_missing[i, j])){
				#mode for factors, otherwise average
				if (inherits(X_with_missing[, j], "factor")){
					X_with_missing[i, j] = names(which.max(table(X_for_calculating_avgs[, j])))
				} else {
					X_with_missing[i, j] = mean(X_for_calculating_avgs[, j], na.rm = TRUE)
				}
			}
		}
	}
	#now we have to go through and drop columns that are all NaN's if need be
	bad_cols = c()
	for (j in colnames(X_with_missing)){
		if (sum(is.nan(X_with_missing[, j])) == nrow(X_with_missing)){
			bad_cols = c(bad_cols, j)
		}
	}
	for (j in bad_cols){
		X_with_missing[, j] = NULL
	}
	X_with_missing
}

#' Destroy BART Model (deprecated --- do not use!)
#'
#' @description
#' A deprecated function that previously was responsible for cleaning up the RAM
#' associated with a BART model. This is now handled natively by R's garbage collection.
#'
#' @details
#' Removing a ``bart_machine'' object from \code{R} previously did not free heap space from Java.
#' Since BART objects can consume a large amount of RAM, it is important to remove
#' these objects by calling this function if they are no longer needed or many BART
#' objects are being created. This operation is now taken care of by R's garbage collection.
#' This function is deprecated and should not be used. However, running it is harmless.
#' @param bart_machine deprecated --- do not use!
#'
#' @return
#' None.
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @examples
#' ##None
destroy_bart_machine = function(bart_machine){
	warning("the method \"destroy_bart_machine\" does not do anything anymore")
}
