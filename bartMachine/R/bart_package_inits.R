##package constants
VERSION = "1.1.1"

##color array 
COLORS = array(NA, 500)
for (i in 1 : 500){
	COLORS[i] = rgb(runif(1, 0, 0.7), runif(1, 0, 0.7), runif(1, 0, 0.7))
}

##set number of cores in use
set_bart_machine_num_cores = function(num_cores){
	assign("BART_NUM_CORES", num_cores, bartMachine_globals)
	cat("bartMachine now using", num_cores, "cores.\n")
}

##get number of cores in use
DEFAULT_BART_NUM_CORES = 1
bart_machine_num_cores = function(){
	if (exists("BART_NUM_CORES", envir = bartMachine_globals)){
		get("BART_NUM_CORES", bartMachine_globals)
	} else {
		DEFAULT_BART_NUM_CORES
	}
}

set_bart_machine_memory = function(bart_max_mem){
	cat("This method has been deprecated. Please use 'options(java.parameters = \"-Xmx", bart_max_mem, "m\")' instead.\n", sep = "")
}

##get variable counts
get_var_counts_over_chain = function(bart_machine, type = "splits"){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	if (!(type %in% c("trees", "splits"))){
		stop("type must be \"trees\" or \"splits\"")
	}
	C = t(sapply(.jcall(bart_machine$java_bart_machine, "[[I", "getCountsForAllAttribute", type), .jevalArray))
	colnames(C) = colnames(bart_machine$model_matrix_training_data)[1 : bart_machine$p]
	C
}

#get variable inclusion proportions
get_var_props_over_chain = function(bart_machine, type = "splits"){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	if (!(type %in% c("trees", "splits"))){
		stop("type must be \"trees\" or \"splits\"")
	}	
	attribute_props = .jcall(bart_machine$java_bart_machine, "[D", "getAttributeProps", type)
	names(attribute_props) = colnames(bart_machine$model_matrix_training_data)[1 : bart_machine$p]
	attribute_props
}

##private function called in summary() 
sigsq_est = function(bart_machine){
	sigsqs = .jcall(bart_machine$java_bart_machine, "[D", "getGibbsSamplesSigsqs")	
	sigsqs_after_burnin = sigsqs[(length(sigsqs) - bart_machine$num_iterations_after_burn_in) : length(sigsqs)]	
	mean(sigsqs_after_burnin)
}

#There's no standard R function for this.
sample_mode = function(data){
	as.numeric(names(sort(-table(data)))[1])
}

check_serialization = function(bart_machine){
	if (is.jnull(bart_machine$java_bart_machine)){
		stop("This bartMachine object was loaded from an R image but was not serialized.\n  Please build bartMachine using the option \"serialize = TRUE\" next time.\n")
	}
}
