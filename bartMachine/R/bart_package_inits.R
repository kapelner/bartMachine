##package constants
VERSION = "1.0b"
JAR_DEPENDENCIES = c("bart_java.jar", "commons-math-2.1.jar", "trove-3.0.3.jar", "junit-4.10.jar", "Jama-1.0.3.jar")

##color array 
COLORS = array(NA, 500)
for (i in 1 : 500){
	COLORS[i] = rgb(runif(1, 0, 0.7), runif(1, 0, 0.7), runif(1, 0, 0.7))
}

##set number of cores in use
set_bart_machine_num_cores = function(num_cores){
	assign("BART_NUM_CORES", num_cores, bartMachine_globals)
}

##get number of cores in use
bart_machine_num_cores = function(){
	if (exists("BART_NUM_CORES", envir = bartMachine_globals)){
		get("BART_NUM_CORES", bartMachine_globals)
	} else {
		stop("Number of cores not set yet. Please use \"set_bart_machine_num_cores.\"")
	}
}

##initialize JVM and let the user know how much RAM is available
init_java_for_bart_machine_with_mem_in_mb = function(bart_max_mem){
	if (exists("JVM_INITIALIZED", envir = bartMachine_globals)){
		stop("Java can only be initialized once per R session. If you would like\nto change the amount of memory available to bartMachine, please\nrestart R and run this function again.")
	}
	
	#Actually initialzie the Java (once per R session)
	.jinit(parameters = paste("-Xmx", bart_max_mem, "m", sep = ""))
	for (dependency in JAR_DEPENDENCIES){
		.jaddClassPath(paste(find.package("bartMachine"), "/java/", dependency, sep = ""))
	} 
	
	if (!exists("JVM_INITIALIZED", envir = bartMachine_globals)){
		cat("Java initialized with ", 
			round(.jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9, 2), 
			"GB maximum memory", 
			ifelse(bart_max_mem == BART_MAX_MEM_MB_DEFAULT, " (the default)", ""), 
			".\n", sep = "")
		assign("JVM_INITIALIZED", TRUE, bartMachine_globals)
	}
}

##get variable counts
get_var_counts_over_chain = function(bart_machine, type = "splits"){
	if (!(type %in% c("trees", "splits"))){
		stop("type must be \"trees\" or \"splits\"")
	}
	C = t(sapply(.jcall(bart_machine$java_bart_machine, "[[I", "getCountsForAllAttribute", type), .jevalArray))
	colnames(C) = colnames(bart_machine$model_matrix_training_data)[1 : bart_machine$p]
	C
}

#get variable inclusion proportions
get_var_props_over_chain = function(bart_machine, type = "splits"){
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
