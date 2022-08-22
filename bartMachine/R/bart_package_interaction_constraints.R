additiveBartMachine = function(constraints = NULL, cv = FALSE, num_runs_through = 3, run_in_sample = TRUE, ...){
	args = list(...)
	X = args$X
	y = args$y
	
	if ((inherits(y, "factor") | inherits(y, "character")) & length(levels(y)) == 2){ #if y is a factor and binary
		stop("Additive BART is only available for regression responses i.e. y must be numeric or integer")
	}	
	args$run_in_sample = FALSE
	
	if (is.null(X) || is.null(y)){
		stop("Both X and y must be specified to use the additive bart machine")
	}
	p = ncol(X)
	n = nrow(X)
	
	if (is.null(constraints)){
		constraints = as.list(1 : p) #generalized additive model with no interactions between features		
	} else if (!inherits(constraints, "list")){
		stop("parameter \"constraints\" must be a list")
	} else if (length(constraints) == 0){
		stop("constraints list cannot be empty")
	}
	#check if the constraint components are valid features
	for (a in 1 : length(constraints)){
		vars_a = constraints[[a]]
		for (var in vars_a){
			if ((inherits(var, "numeric") | inherits(var, "integer")) & !(var %in% (1 : p))){
				stop(paste("Element", var, "in constraints vector number", a, "is numeric but not one of 1, ...,", p, "where", p, "is the number of columns in X."))
			}
			if (inherits(var, "character")  & !(var %in% colnames(X))){
				stop(paste("Element", var, "in constraints vector number", a, "is a string but not one of the column names of X."))
			}
		}
	}
	original_constraints = constraints
	
	#now we add the remaining features as a final constraint
	if (inherits(X, "data.table")){
		Xcopy = copy(X)
	} else {
		Xcopy = X
	}
	for (a in 1 : length(constraints)){
		vars_a = constraints[[a]]
		for (var in vars_a){
			if (inherits(var, "numeric") | inherits(var, "integer")){
				Xcopy[, colnames(X)[var]] = NULL
			} else {
				Xcopy[, var] = NULL
			}			
		}
	}
	if (length(colnames(Xcopy)) > 0){
		constraints[[length(constraints) + 1]] = colnames(Xcopy); rm(Xcopy)
	}
	
	#now duplicate the constraints over num runs
	constraints = rep(constraints, num_runs_through)
	
	t0 = Sys.time()
	additive_bart_machine = list(
		models = list(), 
		original_constraints = original_constraints, 
		constraints = constraints,
		cv = cv, 
		num_runs_through = num_runs_through,
		run_in_sample = run_in_sample, 
		p = p, 
		n = n, 
		X = X, 
		y = y
	)
	y_leftover = y
	for (a in 1 : length(constraints)){
		vars_a = constraints[[a]]
		Xa = X[, vars_a, drop = FALSE]
		args$X = Xa
		args$y = y_leftover
		#build the actual model
		if (cv){
			mod_a = do.call(bartMachineCV, args)
		} else {
			mod_a = do.call(bartMachine, args)
		}
		#now adjust the remaining response
		additive_bart_machine$models[[a]] = mod_a
		if (a < length(constraints)){
			y_leftover = y_leftover - predict(mod_a, Xa)	
		}			
	}

	
	additive_bart_machine$time_to_build = Sys.time() - t0
	
	if (!is.null(run_in_sample)){
		if (run_in_sample){
			y_hat_train = rep(0, nrow(X))
			for (a in 1 : length(constraints)){
				vars_a = constraints[[a]]
				Xa = X[, vars_a, drop = FALSE]
				y_hat_train = y_hat_train + predict(additive_bart_machine$models[[a]], Xa)
			}			
			#return a bunch more stuff
			additive_bart_machine$y_hat_train = y_hat_train
			additive_bart_machine$residuals = y - additive_bart_machine$y_hat_train
			additive_bart_machine$L1_err_train = sum(abs(additive_bart_machine$residuals))
			additive_bart_machine$L2_err_train = sum(additive_bart_machine$residuals^2)
			additive_bart_machine$PseudoRsq = 1 - additive_bart_machine$L2_err_train / sum((y - mean(y))^2) #pseudo R^2 acc'd to our dicussion with Ed and Shane
			additive_bart_machine$rmse_train = sqrt(additive_bart_machine$L2_err_train / additive_bart_machine$n)
		}
	}
	
	args$X = NULL #already exists in object additive_bart_machine
	args$y = NULL #already exists in object additive_bart_machine
	additive_bart_machine = c(additive_bart_machine, args)
	class(additive_bart_machine) = "additiveBartMachine"	
	additive_bart_machine
}
