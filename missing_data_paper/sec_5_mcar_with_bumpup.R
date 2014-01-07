library(bartMachine)
library(MASS)

set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(3000)

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

BUMP = (max(y) - min(y)) * .10

############## MCAR

create_mcar_with_bumpup_model_of_bhd = function(X, y, gamma){
	#rm, crim, lstat, nox, tax
	for (i in 1 : nrow(X)){
		for (j in c("rm", "crim", "lstat", "nox", "tax")){
			if (runif(1) < gamma){
				X[i, j] = NA
			}	
		}
		#bump down
		if (is.na(X[i, "crim"])){
			y[i] = y[i] - rnorm(1, BUMP, BUMP / 4)
		}
		#bump up
		if (is.na(X[i, "tax"])){
			y[i] = y[i] + rnorm(1, BUMP, BUMP / 4)
		}
	}
	list(X = X, y = y)
}

approx_prop_missing = seq(from = 0, to = 0.7, by = 0.1)
gammas = 1 - (1 - approx_prop_missing)^(1 / 5) #our mcar_with_bumpup procedure is mcar_with_bumpup on five variables independently

##test to see if gammas are appropriate
#for (i in 1 : length(approx_prop_missing)){
#	Xmcar_with_bumpup = create_mcar_with_bumpup_model_of_bhd(X, y, gammas[i])
#	actual_prop_missing = 1 - nrow(na.omit(Xmcar_with_bumpup)) / nrow(Xmcar_with_bumpup)
#	cat("purported prop missing:", approx_prop_missing[i], "actual prop missing", actual_prop_missing, "\n")
#}

results_bart_mcar_with_bumpup = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_bart_w_rfi_and_mf_mcar_with_bumpup = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_rf_mcar_with_bumpup = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
rownames(results_bart_mcar_with_bumpup) = approx_prop_missing
rownames(results_bart_w_rfi_and_mf_mcar_with_bumpup) = approx_prop_missing
rownames(results_rf_mcar_with_bumpup) = approx_prop_missing

for (nsim in 1 : Nsim){
	cat("nsim = ", nsim, "\n")
	for (g in 1 : length(gammas)){
		test_indices = sample(1 : nrow(X), n_test)
		
		#create the missing matrix and subset the training and test
		mcar_obj = create_mcar_with_bumpup_model_of_bhd(X, y, gammas[g])
		Xm = mcar_obj$X
		y_with_bump_up = mcar_obj$y
		Xtrain = Xm[-test_indices, ]
		ytrain = y_with_bump_up[-test_indices]
		Xtest = Xm[test_indices, ]
		ytest = y_with_bump_up[test_indices]	
		
		#now start training models and predicting on them
		if (nrow(na.omit(Xtrain)) == nrow(Xtrain)){
			Xtrain_rf_impute = Xtrain		
		} else {
			Xtrain_rf_impute = rfImpute(Xtrain, ytrain, verbose = FALSE)[, 2 : (ncol(Xtrain) + 1)]
			
		}
		bart_mod = build_bart_machine(Xtrain, ytrain, run_in_sample = FALSE, use_missing_data = TRUE, use_missing_data_dummies_as_covars = TRUE, verbose = FALSE)
		bart_mod_rf_imp = build_bart_machine(Xtrain_rf_impute, ytrain, run_in_sample = FALSE, verbose = FALSE)
		rf_mod = randomForest(x = Xtrain_rf_impute, y = ytrain)
		
		#impute to create an Xtest without missingness for rf --- give it everything but ytest
		imputed = missForest(rbind(Xtest, Xtrain), verbose = FALSE)$ximp				
		Xtest_miss_rf = imputed[1 : n_test, ]
		
		results_bart_mcar_with_bumpup[g, nsim] = bart_predict_for_test_data(bart_mod, Xtest, ytest)$rmse
		results_bart_w_rfi_and_mf_mcar_with_bumpup[g, nsim] = bart_predict_for_test_data(bart_mod_rf_imp, Xtest_miss_rf, ytest)$rmse
		results_rf_mcar_with_bumpup[g, nsim] = sqrt((sum(ytest - predict(rf_mod, Xtest_miss_rf))^2) / n_test)
		
		cat("bart oosrmse:", results_bart_mcar_with_bumpup[g, nsim], "rf oosrmse:", results_rf_mcar_with_bumpup[g, nsim], "bart_with_rf_imp oosrmse:", results_bart_w_rfi_and_mf_mcar_with_bumpup[g, nsim], "\n")
		
		#rolling updates!!
		
		avgs_mcar_with_bumpup_bart = apply(results_bart_mcar_with_bumpup, 1, mean, na.rm = TRUE)
		sd_mcar_with_bumpup_bart = apply(results_bart_mcar_with_bumpup, 1, sd, na.rm = TRUE)
		rel_mcar_with_bumpup_avgs_bart = avgs_mcar_with_bumpup_bart / avgs_mcar_with_bumpup_bart[1]
		
		avgs_mcar_with_bumpup_bart_w_rfi_and_mf = apply(results_bart_w_rfi_and_mf_mcar_with_bumpup, 1, mean, na.rm = TRUE)
		sd_mcar_with_bumpup_bart_w_rfi_and_mf = apply(results_bart_w_rfi_and_mf_mcar_with_bumpup, 1, sd, na.rm = TRUE)
		rel_mcar_with_bumpup_avgs_bart_w_rfi_and_mf = avgs_mcar_with_bumpup_bart_w_rfi_and_mf / avgs_mcar_with_bumpup_bart[1]
		
		avgs_mcar_with_bumpup_rf = apply(results_rf_mcar_with_bumpup, 1, mean, na.rm = TRUE)
		sd_mcar_with_bumpup_rf = apply(results_rf_mcar_with_bumpup, 1, sd, na.rm = TRUE)
		rel_mcar_with_bumpup_avgs_rf = avgs_mcar_with_bumpup_rf / avgs_mcar_with_bumpup_bart[1]
		
		plot(approx_prop_missing, rel_mcar_with_bumpup_avgs_bart, col = "green", type = "o", ylim = c(min(rel_mcar_with_bumpup_avgs_bart, rel_mcar_with_bumpup_avgs_bart_w_rfi_and_mf, rel_mcar_with_bumpup_avgs_rf, na.rm = TRUE), max(rel_mcar_with_bumpup_avgs_bart, rel_mcar_with_bumpup_avgs_bart_w_rfi_and_mf, rel_mcar_with_bumpup_avgs_rf, na.rm = TRUE)))
		points(approx_prop_missing, rel_mcar_with_bumpup_avgs_bart_w_rfi_and_mf, col = "blue", type = "o")
		points(approx_prop_missing, rel_mcar_with_bumpup_avgs_rf, col = "red", type = "o")
	}	
}


save.image("sec_5_mcar_with_bumpup")



