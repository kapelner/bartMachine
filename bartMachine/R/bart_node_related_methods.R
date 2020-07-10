node_prediction_training_data_indices = function(bart_machine, new_data = NULL){
	if (bart_machine$flush_indices_to_save_RAM){
		stop("Node prediction training data indices cannot be computed if \"flush_indices_to_save_RAM\" was used to construct the BART model.")
	}
	
	if (is.null(new_data)){
		.jcall(bart_machine$java_bart_machine, "[[[[Z", "getNodePredictionTrainingIndicies", .jcast(.jnull(), new.class = "[[D", check = FALSE, convert.array = FALSE), simplify = TRUE)
	} else {
		.jcall(bart_machine$java_bart_machine, "[[[[Z", "getNodePredictionTrainingIndicies", .jarray(new_data, dispatch = TRUE), simplify = TRUE)
	}
	
}

get_sample_weights = function(bart_machine, new_data = NULL){
	if (bart_machine$flush_indices_to_save_RAM){
		stop("Sample Weights cannot be computed if \"flush_indices_to_save_RAM\" was used to construct the BART model.")
	}
	indices_on_off = node_prediction_training_data_indices(bart_machine, new_data)
	props = t(apply(indices_on_off, 1, function(gibbs_trees_indices){
		apply(apply(gibbs_trees_indices, 3, c), 2, mean)	
	}))
	#scale it
	props = t(apply(props, 1, function(xj){xj / sum(xj)}))
	props
}