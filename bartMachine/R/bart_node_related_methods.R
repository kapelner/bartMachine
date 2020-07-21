node_prediction_training_data_indices = function(bart_machine, new_data = NULL){
	if (bart_machine$flush_indices_to_save_RAM){
		stop("Node prediction training data indices cannot be computed if \"flush_indices_to_save_RAM\" was used to construct the BART model.")
	}
	
	if (is.null(new_data)){
		double_vec_null = .jcast(.jnull(), new.class = "[[D", check = FALSE, convert.array = FALSE)
		.jcall(bart_machine$java_bart_machine, "[[[[Z", "getNodePredictionTrainingIndicies", double_vec_null, simplify = TRUE)
	} else {
		.jcall(bart_machine$java_bart_machine, "[[[[Z", "getNodePredictionTrainingIndicies", .jarray(new_data, dispatch = TRUE), simplify = TRUE)
	}	
}

get_projection_weights = function(bart_machine, new_data = NULL, regression_kludge = FALSE){
	if (bart_machine$flush_indices_to_save_RAM){
		stop("Node prediction training data indices cannot be computed if \"flush_indices_to_save_RAM\" was used to construct the BART model.")
	}
	if (regression_kludge){
		if (is.null(new_data)){
			new_data = bart_machine$X
		}
		yhats = predict(bart_machine, new_data)
	}
	if (is.null(new_data)){
		double_vec_null = .jcast(.jnull(), new.class = "[[D", check = FALSE, convert.array = FALSE)
		weights = .jcall(bart_machine$java_bart_machine, "[[D", "getProjectionWeights", double_vec_null, simplify = TRUE)
	} else {
		weights = .jcall(bart_machine$java_bart_machine, "[[D", "getProjectionWeights", .jarray(as.matrix(new_data), dispatch = TRUE), simplify = TRUE)
	}	
	if (regression_kludge){
		yhat_star_projection = as.numeric(weights %*% bart_machine$y)
		weights * as.numeric(coef(lm(yhats ~ yhat_star_projection))[2]) #scale it back
	} else {
		weights
	}
}