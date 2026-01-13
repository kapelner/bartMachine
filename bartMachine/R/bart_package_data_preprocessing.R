
#' Dummify Design Matrix
#'
#' @description
#' Create a data frame with factors converted to dummies.
#'
#' @details
#' The column names of the dummy variables are given by the ``FactorName_LevelName'' and are augmented to the end of the design matrix. See the example below.
#' @param data Data frame to be dummified.
#'
#' @return
#' Returns a data frame with factors converted to dummy indicator variables.
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @note
#' BART handles dummification internally. This function is provided as a utility function.
#'
#' @examples
#' \dontrun{
#' #generate data
#' set.seed(11)
#' x1 = rnorm(20)
#' x2 = as.factor(ifelse(x1 > 0, "A", "B"))
#' x3 = runif(20)
#' X = data.frame(x1,x2,x3)
#' #dummify data
#' X_dummified = dummify_data(X)
#' print(X_dummified)
#' }
#' @export
dummify_data = function(data){
  assert_data_frame(data)

	as.data.frame(pre_process_training_data(data)$data)
}

##private function that handles all pre-processing (dummification, missing data, etc.)
pre_process_training_data = function(data, use_missing_data_dummies_as_covars = FALSE, imputations = NULL){

	#first convert characters to factors
	character_vars = names(which(sapply(data, class) == "character"))
	for (character_var in character_vars){
		data[, character_var] = as.factor(data[, character_var])
	}
	
	factors = names(which(sapply(data, is.factor)))
	
	factor_lengths = c()
	if (length(factors) > 0) {
		# Pre-allocate a list for dummified matrices to avoid repeated cbind
		dummied_list = lapply(factors, function(fac) {
			levs = levels(data[, fac])
			dummied = matrix(0, nrow = nrow(data), ncol = length(levs))
			for (i in seq_along(levs)) {
				dummied[, i] = as.numeric(data[, fac] == levs[i])
			}
			colnames(dummied) = paste(fac, levs, sep = "_")
			dummied
		})
		
		# Record lengths
		factor_lengths = vapply(dummied_list, ncol, integer(1))
		
		# Combine all dummied matrices and non-factor columns
		non_factors = setdiff(names(data), factors)
		data = do.call(cbind, c(list(data[, non_factors, drop = FALSE]), dummied_list))
	}

	if (use_missing_data_dummies_as_covars){		
		#now take care of missing data - add each column as a missingness dummy
		predictor_columns_with_missingness = as.numeric(which(colSums(is.na(data)) > 0))
		
		#only do something if there are predictors with missingness
		if (length(predictor_columns_with_missingness) > 0){
			M = is.na(data[, predictor_columns_with_missingness, drop = FALSE]) + 0
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
	if (bart_machine$replace_missing_data_with_x_j_bar){
		new_data = imputeMatrixByXbarjContinuousOrModalForBinary(new_data, bart_machine$X)
	}
	n = nrow(new_data)
	
	imputations = NULL #global namespace?
	if (bart_machine$impute_missingness_with_rf_impute){
		#we ahve to impute with missForest since we don't have y's for the test data we want to predict
		imputations = missForest(rbind(new_data, bart_machine$X), verbose = bart_machine$verbose)$ximp
		imputations = imputations[1 : nrow(new_data), ]
		colnames(imputations) = paste(colnames(imputations), "_imp", sep = "")
	}
	
	#ensure factor levels include training levels to match dummies without binding all rows
	training_factors = names(which(sapply(bart_machine$X, is.factor)))
	if (length(training_factors) > 0){
		for (predictor in training_factors){
			if (!predictor %in% names(new_data)){
				next
			}
			training_levels = levels(bart_machine$X[, predictor])
			new_levels = levels(factor(new_data[, predictor]))
			combined_levels = union(training_levels, new_levels)
			new_data[, predictor] = factor(new_data[, predictor], levels = combined_levels)
		}
	}
	
	new_data = pre_process_training_data(new_data, bart_machine$use_missing_data_dummies_as_covars, imputations)$data
		
	if (bart_machine$use_missing_data){
		training_data_features = bart_machine$training_data_features_with_missing_features
	} else {
		training_data_features = bart_machine$training_data_features
	}
	
	#The new data features has to be a superset of the training data features, so pare it down even more
	new_data_features_before = colnames(new_data)
	missing_features = setdiff(training_data_features, new_data_features_before)
	if (length(missing_features) > 0){
		missing_matrix = matrix(0, nrow = n, ncol = length(missing_features))
		colnames(missing_matrix) = missing_features
		new_data = cbind(new_data, missing_matrix)
	}
	
	new_data = new_data[1 : n, training_data_features, drop = FALSE]
	
	differences = setdiff(new_data_features_before, training_data_features)
	
	if (length(differences) > 0){
		warning("The following features were found in records for prediction which were not found in the original training data:\n    ", paste(differences, collapse = ", "), "\n  These features will be ignored during prediction.")
	}
	
	new_data_features = colnames(new_data)
	
	if (!all(new_data_features == training_data_features)){
		# Re-order and fill missing columns in one go
		final_new_data = matrix(0, nrow = n, ncol = length(training_data_features))
		colnames(final_new_data) = training_data_features
		
		common_features = intersect(training_data_features, new_data_features)
		final_new_data[, common_features] = new_data[, common_features]
		new_data = final_new_data
	}
	#coerce to a numeric matrix
	new_data = data.matrix(new_data)
	mode(new_data) = "numeric"
	new_data
}
