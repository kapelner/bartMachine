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

extract_raw_node_data = function(bart_machine, g = 1){
	if (g < 1 | g > bart_machine$num_iterations_after_burn_in){
		stop("g is the gibbs sample number i.e. it must be a natural number between 1 and the number of iterations after burn in")
	}
	raw_data_java = .jcall(bart_machine$java_bart_machine, "[LbartMachine/bartMachineTreeNode;", "extractRawNodeInformation", as.integer(g - 1), simplify = TRUE)
	raw_data = list()
	for (m in 1 : bart_machine$num_trees){
		raw_data[[m]] = extract_node_data(raw_data_java[[m]])	
	}
	raw_data
}

extract_node_data = function(node_java){
	node_data = list()
	node_data$java_obj = node_java
	node_data$parent = node_java$parent
	node_data$left_java_obj = node_java$left
	node_data$right_java_obj = node_java$right
	node_data$depth = node_java$depth
	node_data$isLeaf = node_java$isLeaf
	node_data$splitAttributeM = node_java$splitAttributeM
	node_data$splitValue = node_java$splitValue
	node_data$sendMissingDataRight = node_java$sendMissingDataRight
	node_data$y_pred = node_java$y_pred
	node_data$n_eta = node_java$n_eta
	node_data$string_id = node_java$stringID()
	node_data$is_stump = node_java$isStump()
	node_data$string_location = node_java$stringLocation()
	
	if (!is.jnull(node_java$left)){
		node_data$left = extract_node_data(node_java$left)
	} else {
		node_data$left = NA
	}
	if (!is.jnull(node_java$right)){
		node_data$right = extract_node_data(node_java$right)
	} else {
		node_data$right = NA
	}
	node_data
}
		
		