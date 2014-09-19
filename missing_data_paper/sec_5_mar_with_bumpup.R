options(java.parameters = "-Xmx5000m")
library(bartMachine)
library(MASS)

###constants for simulation
Nsim = 500
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

BUMP = (max(y) - min(y)) * .25

############## MAR

create_mar_with_bumpup_model_of_bhd = function(X, y, beta_0, beta){
	
	for (i in 1 : nrow(X)){
		prob_M_rm = beta_0 + beta * X$indus[i] + beta * X$lstat[i] + beta * X$age[i]
		if (runif(1) < prob_M_rm){
			X$rm[i] = NA
		}
		
		prob_M_crim = beta_0 + beta * X$nox[i] + beta * X$rad[i] + beta * X$tax[i]
		if (runif(1) < prob_M_crim){
			X$crim[i] = NA
		}
		
		#bump up
		if (is.na(X$rm[i])){
			y[i] = y[i] + rnorm(1, BUMP, BUMP / 4)
		}
		#bump down
		if (is.na(X$crim[i])){
			y[i] = y[i] - rnorm(1, BUMP, BUMP / 4)
		}

	}
	X
	list(X = X, y = y)
}

beta_0 = -3
betas = c(0, 1.33, 1.47, 1.67, 2.1, 2.6, 3.1, 3.8)
approx_prop_missing = seq(from = 0, to = 0.7, by = 0.1)

##test to see if gammas are appropriate
#for (i in 1 : length(approx_prop_missing)){
#	Xmar_with_bumpup = create_mar_with_bumpup_model_of_bhd(X, y, gammas[i])
#	actual_prop_missing = 1 - nrow(na.omit(Xmar_with_bumpup)) / nrow(Xmar_with_bumpup)
#	cat("purported prop missing:", approx_prop_missing[i], "actual prop missing", actual_prop_missing, "\n")
#}

results_bart_mar_with_bumpup = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_bart_w_rfi_and_mf_mar_with_bumpup = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_rf_mar_with_bumpup = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
rownames(results_bart_mar_with_bumpup) = approx_prop_missing
rownames(results_bart_w_rfi_and_mf_mar_with_bumpup) = approx_prop_missing
rownames(results_rf_mar_with_bumpup) = approx_prop_missing

for (nsim in 1 : Nsim){
	cat("nsim = ", nsim, "\n")
	for (g in 1 : length(betas)){
		test_indices = sample(1 : nrow(X), n_test)
		
		#create the missing matrix and subset the training and test
		pattern_mixture_mar_mod = create_mar_with_bumpup_model_of_bhd(X, y, beta_0, betas[g])
		Xm = pattern_mixture_mar_mod$X
		y_with_bump_up = pattern_mixture_mar_mod$y
		Xtrain = Xm[-test_indices, ]
		ytrain = y_with_bump_up[-test_indices]
		Xtest = Xm[test_indices, ]
		ytest = y_with_bump_up[test_indices]	
		
		#now start training models and predicting on them
		#impute both training and test data with MissForest
		Xtrain_MF_imputed = missForest(cbind(ytrain, Xtrain))$ximp[, -1]
		
		bart_mod = build_bart_machine(Xtrain, ytrain, run_in_sample = FALSE, use_missing_data = TRUE, use_missing_data_dummies_as_covars = TRUE, verbose = FALSE)
		bart_mod_rf_imp = build_bart_machine(Xtrain_MF_imputed, ytrain, run_in_sample = FALSE, verbose = FALSE)
		rf_mod = randomForest(x = Xtrain_MF_imputed, y = ytrain)
		
		#impute to create an Xtest without missingness for rf
		Xtest_miss_rf = missForest(rbind(Xtest, Xtrain), verbose = FALSE)$ximp[1 : n_test, ]
		
		results_bart_mar_with_bumpup[g, nsim] = bart_predict_for_test_data(bart_mod, Xtest, ytest)$rmse
		results_bart_w_rfi_and_mf_mar_with_bumpup[g, nsim] = bart_predict_for_test_data(bart_mod_rf_imp, Xtest_miss_rf, ytest)$rmse
		y_hat_rf = predict(rf_mod, Xtest_miss_rf)
		results_rf_mar_with_bumpup[g, nsim] = sqrt(sum((ytest - y_hat_rf)^2) / n_test)
		
		destroy_bart_machine(bart_mod)
		destroy_bart_machine(bart_mod_rf_imp)
		
		cat("bart oosrmse:", results_bart_mar_with_bumpup[g, nsim], "rf oosrmse:", results_rf_mar_with_bumpup[g, nsim], "bart_with_rf_imp oosrmse:", results_bart_w_rfi_and_mf_mar_with_bumpup[g, nsim], "\n")
		
		#rolling updates!!
		
		avgs_mar_with_bumpup_bart = apply(results_bart_mar_with_bumpup, 1, mean, na.rm = TRUE)		
		rel_mar_with_bumpup_avgs_bart = avgs_mar_with_bumpup_bart / avgs_mar_with_bumpup_bart[1]
		sd_mar_with_bumpup_bart = apply(results_bart_mar_with_bumpup / avgs_mar_with_bumpup_bart[1], 1, sd, na.rm = TRUE)
		
		avgs_mar_with_bumpup_bart_w_rfi_and_mf = apply(results_bart_w_rfi_and_mf_mar_with_bumpup, 1, mean, na.rm = TRUE)		
		rel_mar_with_bumpup_avgs_bart_w_rfi_and_mf = avgs_mar_with_bumpup_bart_w_rfi_and_mf / avgs_mar_with_bumpup_bart[1]
		sd_mar_with_bumpup_bart_w_rfi_and_mf = apply(results_bart_w_rfi_and_mf_mar_with_bumpup / avgs_mar_with_bumpup_bart[1], 1, sd, na.rm = TRUE)
		
		avgs_mar_with_bumpup_rf = apply(results_rf_mar_with_bumpup, 1, mean, na.rm = TRUE)		
		rel_mar_with_bumpup_avgs_rf = avgs_mar_with_bumpup_rf / avgs_mar_with_bumpup_bart[1]
		sd_mar_with_bumpup_rf = apply(results_rf_mar_with_bumpup / avgs_mar_with_bumpup_bart[1], 1, sd, na.rm = TRUE)
		
		par(mar = c(4.2,4,0.2,0.2))
		plot(approx_prop_missing, 
				rel_mar_with_bumpup_avgs_bart, 
				col = "green", 
				type = "o", 
				xlab = "Proportion Missing",
				ylab = "Multiple of Baseline Error",
				ylim = c(1, 1.75))
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y = rel_mar_with_bumpup_avgs_bart[i]
			moe = 1.96 * sd_mar_with_bumpup_bart[i] / sqrt(nsim)
			segments(x, y - moe, x, y + moe, col = "green")
		}
		points(approx_prop_missing, rel_mar_with_bumpup_avgs_bart_w_rfi_and_mf, col = "blue", type = "o")
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y = rel_mar_with_bumpup_avgs_bart_w_rfi_and_mf[i]
			moe = 1.96 * sd_mar_with_bumpup_bart_w_rfi_and_mf[i] / sqrt(nsim)
			segments(x, y - moe, x, y + moe, col = "blue")
		}
		points(approx_prop_missing, rel_mar_with_bumpup_avgs_rf, col = "red", type = "o")
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y = rel_mar_with_bumpup_avgs_rf[i]
			moe = 1.96 * sd_mar_with_bumpup_rf[i] / sqrt(nsim)
			segments(x, y - moe, x, y + moe, col = "red")
		}
	}	
	save.image("sec_5_mar_with_big_bumpup_MF_only.RData")
}






