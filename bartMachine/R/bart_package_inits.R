##package constants
VERSION = "1.0.2"
JAR_DEPENDENCIES = c("bart_java.jar", "commons-math-2.1.jar", "trove-3.0.3.jar", "junit-4.10.jar")

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
bart_machine_num_cores = function(){
	if (exists("BART_NUM_CORES", envir = bartMachine_globals)){
		get("BART_NUM_CORES", bartMachine_globals)
	} else {
		stop("Number of cores not set yet. Please use \"set_bart_machine_num_cores.\"")
	}
}

set_bart_machine_memory = function(bart_max_mem){
	init_java_for_bart_machine_with_mem_in_mb(bart_max_mem)
}

##initialize JVM and let the user know how much RAM is available
init_java_for_bart_machine_with_mem_in_mb = function(bart_max_mem){
	if (exists("JVM_INITIALIZED", envir = bartMachine_globals)){
		mem_in_gb = get("JVM_INITIALIZED", bartMachine_globals)
		warning(paste("Java can only be initialized once per R session. Currently \n  there is ", mem_in_gb, "GB available. If you would like\n  to change the amount of memory available to bartMachine, please\n  restart R and run this function again.", sep = ""))
		return
	}
	
	#Actually initialzie the Java (once per R session)
	mem_flag_as_string = paste("-Xmx", bart_max_mem, "m", sep = "")
	#we pass in the mem flag TWICE due to bug in MAC OS X which will be fixed in rJava 0.9.6
	#since it works with all versions of rJava, we keep this here in case someone may happen to
	#be running MAC OS X with rJAVA version < 0.9.6
	.jinit(parameters = c(mem_flag_as_string, mem_flag_as_string))
	for (dependency in JAR_DEPENDENCIES){
		.jaddClassPath(paste(find.package("bartMachine"), "/java/", dependency, sep = ""))
	} 
	
	if (!exists("JVM_INITIALIZED", envir = bartMachine_globals)){
		mem_in_gb = round(.jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9, 2)
		cat("Java initialized with ", 
				mem_in_gb, 
			"GB maximum memory", 
			ifelse(bart_max_mem == BART_MAX_MEM_MB_DEFAULT, " (the default)", ""), 
			".\n", sep = "")
		assign("JVM_INITIALIZED", mem_in_gb, bartMachine_globals)
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
