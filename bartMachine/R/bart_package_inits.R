##color array 
COLORS = array(NA, 500)
for (i in 1 : 500){
	COLORS[i] = rgb(runif(1, 0, 0.7), runif(1, 0, 0.7), runif(1, 0, 0.7))
}

##set number of cores in use
#' Set the Number of Cores for BART
#'
#' @description
#' Sets the number of cores to be used for all parallelized BART functions.
#' @param num_cores Number of cores to use. If the number of cores is more than 1, setting the seed during model construction
#'   cannot be deterministic.
#'
#' @return
#' None.
#'
#' @seealso
#' \code{\link{bart_machine_num_cores}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @examples
#' \dontrun{
#' #set all parallelized functions to use 4 cores
#' set_bart_machine_num_cores(4)
#' }
set_bart_machine_num_cores = function(num_cores){
	assign("BART_NUM_CORES", num_cores, bartMachine_globals)
	cat("bartMachine now using", num_cores, "cores.\n")
}

##get number of cores in use
DEFAULT_BART_NUM_CORES = 1
#' Get Number of Cores Used by BART
#'
#' @description
#' Returns number of cores used by BART
#'
#' @details
#' Returns the number of cores currently being used by parallelized BART functions
#'
#' @return
#' %%  ~Describe the value returned
#' %%  If it is a LIST, use
#' Number of cores currently being used by parallelized BART functions.
#' %%  \item{comp2 }{Description of 'comp2'}
#' %% ...
#'
#' @seealso
#' \code{\link{set_bart_machine_num_cores}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @examples
#' \dontrun{
#' bart_machine_num_cores()
#' }
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
#' Get the Variable Inclusion Counts
#'
#' @description
#' Computes the variable inclusion counts for a BART model.
#' @param bart_machine An object of class ``bartMachine''.
#' @param type If ``splits'', then the number of times each variable is chosen for a splitting rule is computed. If ``trees'', then the number of times each variable appears in a tree is computed.
#'
#' @return
#' Returns a matrix of counts of each predictor across all trees by Gibbs sample. Thus, the dimension is \code{num_iterations_after_burn_in}
#' by \code{p} (where \code{p} is the number of predictors after dummifying factors and adding missingness dummies if specified by \code{use_missing_data_dummies_as_covars}).
#'
#' @seealso
#' \code{\link{get_var_props_over_chain}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @examples
#' \dontrun{
#' 
#' #generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 10
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y, num_trees = 20)
#' 
#' #get variable inclusion counts
#' var_counts = get_var_counts_over_chain(bart_machine)
#' print(var_counts)
#' }
get_var_counts_over_chain = function(bart_machine, type = "splits"){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not

	if (!(type %in% c("trees", "splits"))){
		stop("type must be \"trees\" or \"splits\"")
	}
	C = .jcall(bart_machine$java_bart_machine, "[[I", "getCountsForAllAttribute", type, simplify = TRUE)
	colnames(C) = colnames(bart_machine$model_matrix_training_data)[1 : bart_machine$p]
	C
}

#get variable inclusion proportions
#' Get the Variable Inclusion Proportions
#'
#' @description
#' Computes the variable inclusion proportions for a BART model.
#' @param bart_machine An object of class ``bartMachine''.
#' @param type If ``splits'', then the proportion of times each variable is chosen for a splitting rule versus all splitting rules is computed. If ``trees'', then the proportion of times each variable appears in a tree versus all appearances of variables in trees is computed.
#'
#' @return
#' Returns a vector of the variable inclusion proportions.
#'
#' @seealso
#' \code{\link{get_var_counts_over_chain}}
#'
#' @author
#' Adam Kapelner and Justin Bleich
#'
#' @examples
#' \dontrun{
#' #generate Friedman data
#' set.seed(11)
#' n  = 200
#' p = 10
#' X = data.frame(matrix(runif(n * p), ncol = p))
#' y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
#' 
#' ##build BART regression model
#' bart_machine = bartMachine(X, y, num_trees = 20)
#' 
#' #Get variable inclusion proportions
#' var_props = get_var_props_over_chain(bart_machine)
#' print(var_props)
#' }
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
