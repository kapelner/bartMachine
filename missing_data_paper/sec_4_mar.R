library(bartMachine)
library(MASS)
set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)


generate_response_model = function(n, sigma_e = 1, Sigma = NULL, mu_vec = NULL){
	p = 3
	
	if (is.null(Sigma)){
		Sigma = 0.8 * diag(p) + 0.2
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
	
	y_crazy = X1 + X2 + 2 * X3 - X1^2 + X2^2 + X1 * X2 + error
	
	data.frame(Xs, y_crazy)
}

Nsim = 500
n = 250

###MAR

beta_0 = -3
betas = c(0, 0.8, 1.4, 2, 2.7, 4, 7, 30)


generate_mar_model = function(Xy, beta_0, beta){
	for (i in 1 : nrow(Xy)){
		prob = pnorm(beta_0 + beta * Xy[i, 1] + beta * Xy[i, 2]^2)
		if (runif(1) < prob){
			Xy[i, 3] = NA #missingness in X3 is determined by a linear probit model of X1 and X2
		}
	}
	Xy	
}

#test to see if gammas are appropriate
#Xy = generate_response_model(n)
#approx_prop_missing = seq(from = 0, to = 0.7, by = 0.1)
#for (i in 1 : length(approx_prop_missing)){
#	Xmar = generate_mar_model(Xy[, 1 : 3], beta_0, betas[i])
#	actual_prop_missing = 1 - nrow(na.omit(Xmar)) / nrow(Xmar)
#	cat("purported prop missing:", approx_prop_missing[i], "actual prop missing", actual_prop_missing, "\n")
#}

results_bart_all_all_mar = matrix(NA, nrow = length(betas), ncol = Nsim)
results_bart_all_cc_mar = matrix(NA, nrow = length(betas), ncol = Nsim)
results_bart_cc_all_mar = matrix(NA, nrow = length(betas), ncol = Nsim)
results_bart_cc_cc_mar = matrix(NA, nrow = length(betas), ncol = Nsim)
rownames(results_bart_all_all_mar) = betas
rownames(results_bart_all_cc_mar) = betas
rownames(results_bart_cc_all_mar) = betas
rownames(results_bart_cc_cc_mar) = betas

for (nsim in 1 : Nsim){
	cat("nsim = ", nsim, "\n")
	for (g in 1 : length(betas)){
		beta = betas[g]
		cat("beta = ", beta, "\n")
		
		#generate data
		Xy_train_all = generate_mar_model(generate_response_model(n), beta_0, beta)
		Xy_train_cc = na.omit(Xy_train_all)
		Xy_test_cc = generate_response_model(n)
		Xy_test_all = generate_mar_model(Xy_test_cc, beta_0, beta)
		
		
		#train models
		bart_machine_all = build_bart_machine(Xy = Xy_train_all, use_missing_data = TRUE, use_missing_data_dummies_as_covars = TRUE, verbose = FALSE, run_in_sample = FALSE)
		
		#jet if we have exceedingly few rows in Xycc
		if (nrow(Xy_train_cc) > 5){
			bart_machine_cc = build_bart_machine(Xy = Xy_train_cc, use_missing_data = TRUE, verbose = FALSE, run_in_sample = FALSE)
		}
		#test models		
		results_bart_all_all_mar[g, nsim] = bart_predict_for_test_data(bart_machine_all, Xtest = Xy_test_all[, 1 : 3], ytest = Xy_test_all[, 4])$rmse
		results_bart_all_cc_mar[g, nsim] = bart_predict_for_test_data(bart_machine_all, Xtest = Xy_test_cc[, 1 : 3], ytest = Xy_test_cc[, 4])$rmse
		if (nrow(Xy_train_cc) > 5){ #jet if we have exceedingly few rows in Xycc
			results_bart_cc_all_mar[g, nsim] = bart_predict_for_test_data(bart_machine_cc, Xtest = Xy_test_all[, 1 : 3], ytest = Xy_test_all[, 4])$rmse
			results_bart_cc_cc_mar[g, nsim] = bart_predict_for_test_data(bart_machine_cc, Xtest = Xy_test_cc[, 1 : 3], ytest = Xy_test_cc[, 4])$rmse
		}
	}
	
	avgs_mar_all_all = apply(results_bart_all_all_mar, 1, mean, na.rm = TRUE)
	sd_mar_all_all = apply(results_bart_all_all_mar, 1, sd, na.rm = TRUE)
	rel_mar_avgs_all_all = avgs_mar_all_all / avgs_mar_all_all[1]
	
	avgs_mar_all_cc = apply(results_bart_all_cc_mar, 1, mean, na.rm = TRUE)
	sd_mar_all_cc = apply(results_bart_all_cc_mar, 1, sd, na.rm = TRUE)
	rel_mar_avgs_all_cc = avgs_mar_all_cc / avgs_mar_all_all[1]
	
	avgs_mar_cc_all = apply(results_bart_cc_all_mar, 1, mean, na.rm = TRUE)
	sd_mar_cc_all = apply(results_bart_cc_all_mar, 1, sd, na.rm = TRUE)
	rel_mar_avgs_cc_all = avgs_mar_cc_all / avgs_mar_all_all[1]
	
	avgs_mar_cc_cc = apply(results_bart_cc_cc_mar, 1, mean, na.rm = TRUE)
	sd_mar_cc_cc = apply(results_bart_cc_cc_mar, 1, sd, na.rm = TRUE)
	rel_mar_avgs_cc_cc = avgs_mar_cc_cc / avgs_mar_all_all[1]
	
	
	approx_prop_missing = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7) #this was figured out during simulation to be approximately accurate (the plots don't change that much anyway)

	#Figure 2b
	par(mar = c(4.2,2,0.3,0.2))
	plot(approx_prop_missing, 
			rel_mar_avgs_all_all, 
			col = "blue", 
			type = "o", 
			ylim = c(1, max(rel_mar_avgs_all_all, rel_mar_avgs_all_cc, rel_mar_avgs_cc_all, rel_mar_avgs_cc_cc, na.rm = TRUE)),
			xlab = "Approx. Prop. Missing",
			ylab = "")
	points(approx_prop_missing, rel_mar_avgs_all_cc, col = "blue", type = "o", lty = 3)
	points(approx_prop_missing, rel_mar_avgs_cc_all, col = "red", type = "o")
	points(approx_prop_missing, rel_mar_avgs_cc_cc, col = "red", type = "o", lty = 3)
	
	save.image("sec_4.2_mar.RData")
}


