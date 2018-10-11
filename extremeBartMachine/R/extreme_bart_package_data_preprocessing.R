
dummify_data = function(data){
	as.data.frame(pre_process_training_data(data)$data)
}

##private function that handles all pre-processing (dummification, missing data, etc.)
pre_process_training_data = function(data, use_missing_data_dummies_as_covars = FALSE, imputations = NULL){

	#first convert characters to factors
	character_vars = names(which(sapply(data, class) == "character"))
	for (character_var in character_vars){
		data[, character_var] = as.factor(data[, character_var])
	}
	
	factors = names(which(sapply(data, class) == "factor"))
	
	factor_lengths = c()
	for (fac in factors){
		#first create the dummies to be appended for this factor
		dummied = do.call(cbind, lapply(levels(data[, fac]), function(lev){as.numeric(data[, fac] == lev)}))
		#ensure they're named appropriately
		colnames(dummied) = paste(fac, levels(data[, fac]), sep = "_")
		#append them to the data
		data = cbind(data, dummied)
		#delete the factor covariate from the design matrix
		data[, fac] = NULL
		#record the length of this factor
		factor_lengths = c(factor_lengths, ncol(dummied))
	}

	if (use_missing_data_dummies_as_covars){		
		#now take care of missing data - add each column as a missingness dummy
		predictor_columns_with_missingness = as.numeric(which(colSums(is.na(data)) > 0))
		
		#only do something if there are predictors with missingness
		if (length(predictor_columns_with_missingness) > 0){
			M = matrix(0, nrow = nrow(data), ncol = length(predictor_columns_with_missingness))
			for (i in 1 : nrow(data)){
				for (j in 1 : length(predictor_columns_with_missingness)){
					if (is.missing(data[i, predictor_columns_with_missingness[j]])){
						M[i, j] = 1
					}
				}
			}
			colnames(M) = paste("M_", colnames(data)[predictor_columns_with_missingness], sep = "")
			
			#now we may want to add imputations before the missingness dummies
			if (!is.null(imputations)){
				data = cbind(data, imputations)
			}
			#append the missing dummy columns to data as if they're real attributes themselves
			data = cbind(data, M)			
		}
	} else if (!is.null(imputations)){ #now we may want to add imputations
		data = cbind(data, imputations) 
	}
	#make sure to cast it as a data matrix and return it along with the factor_lengths
	list(data = data.matrix(data), factor_lengths = factor_lengths)
}

is.missing = function(x){
	is.na(x) || is.nan(x)
}

pre_process_new_data = function(new_data, bart_machine){
	new_data = as.data.frame(new_data)
	n = nrow(new_data)
	
	imputations = NULL #global namespace?
	if (bart_machine$impute_missingness_with_rf_impute){
		#we ahve to impute with missForest since we don't have y's for the test data we want to predict
		imputations = missForest(rbind(new_data, bart_machine$X), verbose = bart_machine$verbose)$ximp
		imputations = imputations[1 : nrow(new_data), ]
		colnames(imputations) = paste(colnames(imputations), "_imp", sep = "")
	}
	
	#preprocess the new data with the training data to ensure proper dummies
	new_data_and_training_data = rbind(new_data, bart_machine$X)
	#kill all factors again
	predictors_which_are_factors = names(which(sapply(new_data_and_training_data, is.factor)))
	for (predictor in predictors_which_are_factors){
		new_data_and_training_data[, predictor] = factor(new_data_and_training_data[, predictor])
	}
	
	new_data = pre_process_training_data(new_data_and_training_data, bart_machine$use_missing_data_dummies_as_covars, imputations)$data
		
	if (bart_machine$use_missing_data){
		training_data_features = bart_machine$training_data_features_with_missing_features
	} else {
		training_data_features = bart_machine$training_data_features
	}
	
	#The new data features has to be a superset of the training data features, so pare it down even more
	new_data_features_before = colnames(new_data)	
	
	new_data = new_data[1 : n, training_data_features, drop = FALSE]
	
	differences = setdiff(new_data_features_before, training_data_features)
	
	if (length(differences) > 0){
		warning("The following features were found in records for prediction which were not found in the original training data:\n    ", paste(differences, collapse = ", "), "\n  These features will be ignored during prediction.")
	}
	
	new_data_features = colnames(new_data)
	
	if (!all(new_data_features == training_data_features)){
		warning("Are you sure you have the same feature names in the new record(s) as the training data?", call. = FALSE)
	}
	
	#iterate through and see
	for (j in 1 : length(training_data_features)){
		training_data_feature = training_data_features[j]
		new_data_feature = new_data_features[j]
		if (training_data_feature != new_data_feature){
			#create a new col of zeroes
			new_col = rep(0, n)
			#wedge it into the data set
			temp_new_data = cbind(new_data[, 1 : (j - 1)], new_col)
			#give it the same name as in the training set
			colnames(temp_new_data)[j] = training_data_feature
			#tack on the rest of the stuff
			if (ncol(new_data) >= j){
				rhs = new_data[, j : ncol(new_data)]
				if (class(rhs) == "numeric"){
					rhs = as.matrix(rhs)
					colnames(rhs)[1] = new_data_feature
				}
				temp_new_data = cbind(temp_new_data, rhs)
			} 
			new_data = temp_new_data
			
			#update list
			new_data_features = colnames(new_data)
		}
	}
	#coerce to a numeric matrix
	new_data = data.matrix(new_data)
	mode(new_data) = "numeric"
	new_data
}
