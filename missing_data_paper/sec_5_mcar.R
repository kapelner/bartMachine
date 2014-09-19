options(java.parameters = "-Xmx5000m")
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

############## MCAR

create_mcar_model_of_bhd = function(X, gamma){
	#rm, crim, lstat, nox, tax
	for (i in 1 : nrow(X)){
		for (j in c("rm", "crim", "lstat", "nox", "tax")){
			if (runif(1) < gamma){
				X[i, j] = NA
			}	
		}
	}
	X
}

approx_prop_missing = seq(from = 0, to = 0.7, by = 0.1)
gammas = 1 - (1 - approx_prop_missing)^(1 / 5) #our mcar procedure is mcar on five variables independently

#test to see if gammas are appropriate
#for (i in 1 : length(approx_prop_missing)){
#	Xmcar = create_mcar_model_of_bhd(X, gammas[i])
#	actual_prop_missing = 1 - nrow(na.omit(Xmcar)) / nrow(Xmcar)
#	cat("purported prop missing:", approx_prop_missing[i], "actual prop missing", actual_prop_missing, "\n")
#}

results_bart_mcar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_bart_w_rfi_and_mf_mcar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_rf_mcar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
rownames(results_bart_mcar) = approx_prop_missing
rownames(results_bart_w_rfi_and_mf_mcar) = approx_prop_missing
rownames(results_rf_mcar) = approx_prop_missing

for (nsim in 1 : Nsim){
	cat("nsim = ", nsim, "\n")
	for (g in 1 : length(gammas)){
		test_indices = sample(1 : nrow(X), n_test)
		
		#create the missing matrix and subset the training and test
		Xm = create_mcar_model_of_bhd(X, gammas[g])
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
		
		results_bart_mcar[g, nsim] = bart_predict_for_test_data(bart_mod, Xtest, ytest)$rmse
		results_bart_w_rfi_and_mf_mcar[g, nsim] = bart_predict_for_test_data(bart_mod_rf_imp, Xtest_miss_rf, ytest)$rmse
		y_hat_rf = predict(rf_mod, Xtest_miss_rf)
		results_rf_mcar[g, nsim] = sqrt(sum((ytest - y_hat_rf)^2) / n_test)
		
		destroy_bart_machine(bart_mod)
		destroy_bart_machine(bart_mod_rf_imp)
		
		cat("bart oosrmse:", results_bart_mcar[g, nsim], "rf oosrmse:", results_rf_mcar[g, nsim], "bart_with_rf_imp oosrmse:", results_bart_w_rfi_and_mf_mcar[g, nsim], "\n")
		
		#rolling updates!!
		
		avgs_mcar_bart = apply(results_bart_mcar, 1, mean, na.rm = TRUE)		
		rel_mcar_avgs_bart = avgs_mcar_bart / avgs_mcar_bart[1]
		sd_mcar_bart = apply(results_bart_mcar / avgs_mcar_bart[1], 1, sd, na.rm = TRUE)
		
		avgs_mcar_bart_w_rfi_and_mf = apply(results_bart_w_rfi_and_mf_mcar, 1, mean, na.rm = TRUE)		
		rel_mcar_avgs_bart_w_rfi_and_mf = avgs_mcar_bart_w_rfi_and_mf / avgs_mcar_bart[1]
		sd_mcar_bart_w_rfi_and_mf = apply(results_bart_w_rfi_and_mf_mcar / avgs_mcar_bart[1], 1, sd, na.rm = TRUE)
		
		avgs_mcar_rf = apply(results_rf_mcar, 1, mean, na.rm = TRUE)		
		rel_mcar_avgs_rf = avgs_mcar_rf / avgs_mcar_bart[1]
		sd_mcar_rf = apply(results_rf_mcar / avgs_mcar_bart[1], 1, sd, na.rm = TRUE)
		
		par(mar = c(4.2,4,0.2,0.2))
		plot(approx_prop_missing, 
				rel_mcar_avgs_bart[1 : length(approx_prop_missing)], 
				col = "green", 
				type = "o", 
				xlab = "Proportion Missing",
				ylab = "Multiple of Baseline Error",
				ylim = c(min(rel_mcar_avgs_bart, rel_mcar_avgs_bart_w_rfi_and_mf, rel_mcar_avgs_rf, na.rm = TRUE), max(rel_mcar_avgs_bart, rel_mcar_avgs_bart_w_rfi_and_mf, rel_mcar_avgs_rf, na.rm = TRUE)))
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y = rel_mcar_avgs_bart[i]
			moe = 1.96 * sd_mcar_bart[i] / sqrt(nsim)
			segments(x, y - moe, x, y + moe, col = "green")
		}
		points(approx_prop_missing, rel_mcar_avgs_bart_w_rfi_and_mf[1 : length(approx_prop_missing)], col = "blue", type = "o")
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y = rel_mcar_avgs_bart_w_rfi_and_mf[i]
			moe = 1.96 * sd_mcar_bart_w_rfi_and_mf[i] / sqrt(nsim)
			segments(x, y - moe, x, y + moe, col = "blue")
		}
		points(approx_prop_missing, rel_mcar_avgs_rf[1 : length(approx_prop_missing)], col = "red", type = "o")
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y = rel_mcar_avgs_rf[i]
			moe = 1.96 * sd_mcar_rf[i] / sqrt(nsim)
			segments(x, y - moe, x, y + moe, col = "red")
		}
	}	
	
	save.image("sec_5_mcar_MF_only.RData")
}






