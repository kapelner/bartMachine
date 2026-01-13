options(java.parameters = c("-Xmx20g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))
library(bartMachine)
library(MASS)

set_bart_machine_num_cores(4)

###constants for simulation
Nsim = 1000
pct_test_data = 0.2


#get the Boston housing data
data(Boston)
X = Boston
#X = cbind(X, rnorm(nrow(X)))
y = X$medv
X$medv = NULL

#unitize the covs
X = data.frame(apply(X, 2, function(xj){(xj - min(xj)) / (max(xj) - min(xj))}))

n_test = round(pct_test_data * nrow(X))



##### NMAR

create_nmar_model_of_bhd = function(X, beta_0, beta){
	for (i in 1 : nrow(X)){
		prob_M_rm = beta_0 + beta * X$rm[i] + beta * X$lstat[i]
		if (runif(1) < prob_M_rm){
			X$rm[i] = NA
		}
		
		prob_M_crim = beta_0 + beta * X$nox[i] + beta * X$crim[i]
		if (runif(1) < prob_M_crim){
			X$crim[i] = NA
		}
	}
	X
}


beta_0 = -3
betas = c(0, 3.25, 3.55, 3.85, 4.1, 4.25, 4.55, 4.8)
approx_prop_missing = seq(from = 0, to = 0.7, by = 0.1)

##test to see if betas are appropriate
#for (i in 1 : length(betas)){
#	Xnmar = create_nmar_model_of_bhd(X, beta_0, betas[i])
#	actual_prop_missing = 1 - nrow(na.omit(Xnmar)) / nrow(Xnmar)
#	cat("purported prop missing:", approx_prop_missing[i], "actual prop missing", actual_prop_missing, "\n")
#}

results_bart_nmar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_bart_w_rfi_and_mf_nmar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_rf_nmar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
rownames(results_bart_nmar) = approx_prop_missing
rownames(results_bart_w_rfi_and_mf_nmar) = approx_prop_missing
rownames(results_rf_nmar) = approx_prop_missing

for (nsim in 1 : Nsim){
	cat("nsim = ", nsim, "\n")
	for (g in 1 : length(betas)){
		test_indices = sample(1 : nrow(X), n_test)
		
		#create the missing matrix and subset the training and test
		Xm = create_nmar_model_of_bhd(X, beta_0, betas[g])
		Xtrain = Xm[-test_indices, ]
		ytrain = y[-test_indices]
		Xtest = Xm[test_indices, ]
		ytest = y[test_indices]	
		
		#now start training models and predicting on them
		#impute both training and test data with MissForest
		Xtrain_MF_imputed = missForest(cbind(ytrain, Xtrain))$ximp[, -1]
		
		bart_mod = build_bart_machine(Xtrain, ytrain, run_in_sample = FALSE, use_missing_data = TRUE, use_missing_data_dummies_as_covars = TRUE, verbose = FALSE)
		bart_mod_rf_imp = build_bart_machine(Xtrain_MF_imputed, ytrain, run_in_sample = FALSE, verbose = FALSE)
		rf_mod = randomForest(x = Xtrain_MF_imputed, y = ytrain)
		
		#impute to create an Xtest without missingness for rf --- give it everything but ytest
		imputed = missForest(rbind(Xtest, Xtrain), verbose = FALSE)$ximp				
		Xtest_miss_rf = imputed[1 : n_test, ]
		
		results_bart_nmar[g, nsim] = bart_predict_for_test_data(bart_mod, Xtest, ytest)$rmse
		results_bart_w_rfi_and_mf_nmar[g, nsim] = bart_predict_for_test_data(bart_mod_rf_imp, Xtest_miss_rf, ytest)$rmse
		y_hat_rf = predict(rf_mod, Xtest_miss_rf)
		results_rf_nmar[g, nsim] = sqrt(sum((ytest - y_hat_rf)^2) / n_test)
		
		cat("bart oosrmse:", results_bart_nmar[g, nsim], "rf oosrmse:", results_rf_nmar[g, nsim], "bart_with_rf_imp oosrmse:", results_bart_w_rfi_and_mf_nmar[g, nsim], "\n")
		
		avgs_nmar_bart = apply(results_bart_nmar, 1, mean, na.rm = TRUE)		
		rel_nmar_avgs_bart = avgs_nmar_bart / avgs_nmar_bart[1]
		sd_nmar_bart = apply(results_bart_nmar / avgs_nmar_bart[1], 1, sd, na.rm = TRUE)
		
		avgs_nmar_bart_w_rfi_and_mf = apply(results_bart_w_rfi_and_mf_nmar, 1, mean, na.rm = TRUE)		
		rel_nmar_avgs_bart_w_rfi_and_mf = avgs_nmar_bart_w_rfi_and_mf / avgs_nmar_bart[1]
		sd_nmar_bart_w_rfi_and_mf = apply(results_bart_w_rfi_and_mf_nmar / avgs_nmar_bart[1], 1, sd, na.rm = TRUE)
		
		avgs_nmar_rf = apply(results_rf_nmar, 1, mean, na.rm = TRUE)		
		rel_nmar_avgs_rf = avgs_nmar_rf / avgs_nmar_bart[1]
		sd_nmar_rf = apply(results_rf_nmar / avgs_nmar_bart[1], 1, sd, na.rm = TRUE)
		
		par(mar = c(4.2,4,0.2,0.2))
		plot(approx_prop_missing, 
				rel_nmar_avgs_bart, 
				col = "green", 
				type = "o", 
				xlab = "Proportion Missing",
				ylab = "Multiple of Baseline Error",
				ylim = c(min(rel_nmar_avgs_bart, rel_nmar_avgs_bart_w_rfi_and_mf, rel_nmar_avgs_rf, na.rm = TRUE), max(rel_nmar_avgs_bart, rel_nmar_avgs_bart_w_rfi_and_mf, rel_nmar_avgs_rf, na.rm = TRUE)))
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y_plot = rel_nmar_avgs_bart[i]
			moe = 1.96 * sd_nmar_bart[i] / sqrt(nsim)
			segments(x, y_plot - moe, x, y_plot + moe, col = "green")
		}
		points(approx_prop_missing, rel_nmar_avgs_bart_w_rfi_and_mf, col = "blue", type = "o")
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y_plot = rel_nmar_avgs_bart_w_rfi_and_mf[i]
			moe = 1.96 * sd_nmar_bart_w_rfi_and_mf[i] / sqrt(nsim)
			segments(x, y_plot - moe, x, y_plot + moe, col = "blue")
		}
		points(approx_prop_missing, rel_nmar_avgs_rf, col = "red", type = "o")
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y_plot = rel_nmar_avgs_rf[i]
			moe = 1.96 * sd_nmar_rf[i] / sqrt(nsim)
			segments(x, y_plot - moe, x, y_plot + moe, col = "red")
		}
	}
	
	save.image("sec_5_nmar_MF_only.RData")
}

