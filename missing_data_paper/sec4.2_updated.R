library(bartMachine)


########## CRAZY MODEL


set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)

generate_response_model = function(n, sigma_e = 1, Sigma = NULL, mu_vec = NULL){
	p = 3
	
	if (is.null(Sigma)){
		Sigma = 0.9 * diag(p) + 0.1
		Sigma[1, p] = 2 * Sigma[1, p]
		Sigma[p, 1] = 2 * Sigma[p, 1]
	}
	if (is.null(mu_vec)){
		mu_vec = rep(0, p)
	}
	
	Xs = mvrnorm(n, mu_vec, Sigma)
	error = rnorm(n, 0, sigma_e)
	X1 = Xs[, 1]
	X2 = Xs[, 2]
	X3 = Xs[, 3]
	
	y_crazy = X1 + X2 + X3 - X1^2 + X2^2 + X1 * X2 + error
	
	data.frame(Xs, y_crazy)
}

n = 500


###MCAR
gammas = seq(from = 0, to = 0.9, by = 0.1)
for (gamma in gammas){
	Xy = generate_response_model(n)
	
	for (i in 1 : n){
		for (j in 1 : 3){
			if (runif(1) < gamma){
				Xy[i, j] = NA
			}
		}
	}
	
	###do simulations here
	bart_machine_oos_rmse = k_fold_cv(X = Xy[, 1 : 3], y = Xy[, 4], use_missing_data = TRUE, verbose = FALSE)
	
	Xycc = na.omit(Xy)
	
	bart_machine_cc_oos_rmse = k_fold_cv(X = Xycc[, 1 : 3], y = Xycc[, 4], verbose = FALSE)
	
}

###MAR

###NMAR