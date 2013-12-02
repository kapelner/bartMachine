library(randomForest)
library(missForest)
library(MASS)


###########GRID COMPUTING
LAST_NAME = "kapelner"
NOT_ON_GRID = length(grep("wharton.upenn.edu", Sys.getenv(c("HOSTNAME")))) == 0

if (NOT_ON_GRID){
	directory_where_code_is = "C:\\Users\\kapelner\\workspace\\bartMachine"
} else {
	directory_where_code_is = getwd()
}
setwd(directory_where_code_is)

source("r_scripts/bart_package_inits.R")
source("r_scripts/bart_package_builders.R")
source("r_scripts/bart_package_predicts.R")
source("r_scripts/bart_package_data_preprocessing.R")
source("r_scripts/bart_package_plots.R")
source("r_scripts/bart_package_variable_selection.R")
source("r_scripts/bart_package_f_tests.R")
source("r_scripts/bart_package_summaries.R")
source("r_scripts/missing_data/sims_functions.R")

args = commandArgs(TRUE)
print(paste("args:", args))

if (length(args) > 0){
	for (i in 1 : length(args)){
		eval(parse(text = args[[i]]))
	}
}
if (NOT_ON_GRID){
	iter_num = 1
	set_bart_machine_num_cores(4)
}




#get the Boston housing data
data(Boston)
X = Boston
#X = cbind(X, rnorm(nrow(X)))
y = X$medv
X$medv = NULL

#set simulation params
Nsim = 500
ALPHA = 0.05
BURN_IN = 1000
KnockoutPROP = c(0.01, 0.05, 0.10, 0.2, 0.3, 0.4, 0.5, 0.9)
set_bart_machine_num_cores(4)
pct_test_data = 0.2
n_test = round(pct_test_data * nrow(X))





############ BEGIN CODE


#bottom line metric: oos_rmse
oos_rmse_vanilla_bhd = array(NA, Nsim)
for (nsim in 1 : Nsim){
	test_indices = sample(1 : nrow(X), n_test)
	Xtest = X[test_indices, ]
	ytest = y[test_indices]
	Xtrain = X[-test_indices, ]
	ytrain = y[-test_indices]
	bart_machine = build_bart_machine(Xtrain, ytrain, verbose = FALSE, run_in_sample = FALSE, num_burn_in = BURN_IN)
	predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
	destroy_bart_machine(bart_machine)
	oos_rmse_vanilla_bhd[nsim] = predict_obj$rmse
	cat(".")
}
cat("\n")
write.csv(oos_rmse_vanilla_bhd, file = "oos_rmse_vanilla_bhd.csv")
oos_rmse_vanilla_bhd = read.csv("oos_rmse_vanilla_bhd.csv")[, 2]

############################  MCAR

if (iter_num == 1){

	oos_rmse_bhd_bartm_mcar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){	
		for (nsim in 1 : Nsim){	
			Xm = knockout_mcar(X, KnockoutPROP[i])
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			bart_machine = build_bart_machine(Xtrain, ytrain, verbose = FALSE, run_in_sample = FALSE, num_burn_in = BURN_IN)
			predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
			destroy_bart_machine(bart_machine)
			oos_rmse_bhd_bartm_mcar[i, nsim] = predict_obj$rmse
			print(oos_rmse_bhd_bartm_mcar)
		}
	}
	
	
	bhd_bartm_results_mcar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_bartm_mcar)
	rownames(bhd_bartm_results_mcar) = c(0, KnockoutPROP)
	bhd_bartm_results_mcar = cbind(bhd_bartm_results_mcar, apply(bhd_bartm_results_mcar, 1, mean))
	write.csv(bhd_bartm_results_mcar, "bhd_bartm_results_mcar.csv")
	bhd_bartm_results_mcar = read.csv("bhd_bartm_results_mcar.csv", row.names = 1, header = T)
}


if (iter_num == 2){

	oos_rmse_bhd_lm_mcar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){
			Xm = knockout_mcar(X, KnockoutPROP[i])
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			Xtest = imputeMatrixByXbarj(Xtest, Xtrain)
			Xtrain = imputeMatrixByXbarj(Xtrain, Xtrain)
			Xytrain = data.frame(Xtrain , ytrain)
			lm_mod = lm(ytrain ~., data = Xytrain)
			y_hat = predict(lm_mod, newdata = Xtest)
			oos_rmse_bhd_lm_mcar[i, nsim] = sqrt((sum(ytest - y_hat)^2) / n_test) 
	#		print(oos_rmse_crazy_model_lm)
		}
	}
	
	bhd_lm_results_mcar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_lm_mcar)
	rownames(bhd_lm_results_mcar) = c(0, KnockoutPROP)
	bhd_lm_results_mcar = cbind(bhd_lm_results_mcar, apply(bhd_lm_results_mcar, 1, mean))
	write.csv(bhd_lm_results_mcar, "bhd_lm_results_mcar.csv")	
	bhd_lm_results_mcar = read.csv("bhd_lm_results_mcar.csv", row.names = 1, header = T)
}




if (iter_num == 3){

	oos_rmse_bhd_xbarj_no_M_mcar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){	
			Xm = knockout_mcar(X, KnockoutPROP[i])	
			
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			bart_machine = build_bart_machine(X = Xtrain, y = ytrain, verbose = TRUE, run_in_sample = FALSE, replace_missing_data_with_x_j_bar = TRUE, use_missing_data = FALSE, num_burn_in = BURN_IN)
			
			Xtest = imputeMatrixByXbarj(Xtest, Xtrain)
			predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
			destroy_bart_machine(bart_machine)
			oos_rmse_bhd_xbarj_no_M_mcar[i, nsim] = predict_obj$rmse
			print(oos_rmse_bhd_xbarj_no_M_mcar)
		}
	}
	
	bhd_xbarj_no_M_results_mcar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_xbarj_no_M_mcar)
	rownames(bhd_xbarj_no_M_results_mcar) = c(0, KnockoutPROP)
	bhd_xbarj_no_M_results_mcar = cbind(bhd_xbarj_no_M_results_mcar, apply(bhd_xbarj_no_M_results_mcar, 1, mean))
	write.csv(bhd_xbarj_no_M_results_mcar, "bhd_xbarj_no_M_results_mcar.csv")
	bhd_xbarj_no_M_results_mcar = read.csv("bhd_xbarj_no_M_results_mcar.csv", row.names = 1, header = T)
}


if (iter_num == 4){

	oos_rmse_bhd_rf_mcar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){	
			Xm = knockout_mcar(X, KnockoutPROP[i])	
			
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			
			if (nrow(na.omit(Xtrain)) == nrow(Xtrain)){
				rf_mod = randomForest(x = Xtrain, y = ytrain)				
			} else {
				rf_mod = randomForest(ytrain ~ ., rfImpute(Xtrain, ytrain))		
			}
			
			#to get Xtest, impute also using Xtrain
			imputed = missForest(rbind(Xtest, Xtrain), verbose = TRUE)$ximp		
			Xtest_miss_rf = imputed[1 : n_test, ]		
			
			y_hat = predict(rf_mod, Xtest_miss_rf)
			oos_rmse_bhd_rf_mcar[i, nsim] = sqrt((sum(ytest - y_hat)^2) / n_test)
			print(oos_rmse_bhd_rf_mcar)
		}
	}
	
	bhd_results_rf_mcar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_rf_mcar)
	rownames(bhd_results_rf_mcar) = c(0, KnockoutPROP)
	bhd_results_rf_mcar = cbind(bhd_results_rf_mcar, apply(bhd_results_rf_mcar, 1, mean))
	write.csv(bhd_results_rf_mcar, "bhd_results_rf_mcar.csv")
	bhd_results_rf_mcar = read.csv("bhd_results_rf_mcar.csv", row.names = 1, header = T)

}

if (iter_num == 5){

	oos_rmse_bhd_bart_with_imp_mcar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i_knockout in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){	
			Xm = knockout_mcar(X, KnockoutPROP[i_knockout])
			
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			bart_machine = build_bart_machine(Xtrain, ytrain, verbose = FALSE, run_in_sample = FALSE, impute_missingness_with_rf_impute = TRUE, debug_log = TRUE, num_burn_in = BURN_IN)
			
			print(bart_machine)
			predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
			destroy_bart_machine(bart_machine)
			oos_rmse_bhd_bart_with_imp_mcar[i_knockout, nsim] = predict_obj$rmse
			print(oos_rmse_bhd_bart_with_imp_mcar)
		}
	}
	
	bhd_results_bart_with_imp_mcar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_bart_with_imp_mcar)
	rownames(bhd_results_bart_with_imp_mcar) = c(0, KnockoutPROP)
	bhd_results_bart_with_imp_mcar = cbind(bhd_results_bart_with_imp_mcar, apply(bhd_results_bart_with_imp_mcar, 1, mean))
	write.csv(bhd_results_bart_with_imp_mcar, "bhd_results_bart_with_imp_mcar.csv")
	bhd_results_bart_with_imp_mcar = read.csv("bhd_results_bart_with_imp_mcar.csv", row.names = 1, header = T)

}

if (NOT_ON_GRID){
	par(mar = c(4,4,0.5,0.5))
	windows()
	plot(rownames(bhd_xbarj_no_M_results_mcar), bhd_xbarj_no_M_results_mcar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), 
			type = "n", 
#			main = "MCAR", 
			xlab = "Proportion Data Missing", 
			ylab = "Multiple of Baseline Error", ylim = c(1, 3),
			lwd = 3,
			col = "purple")
	
#	points(rownames(bhd_lm_results_mcar), bhd_lm_results_mcar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "red", lwd = 3, type = "b")
	points(rownames(bhd_bartm_results_mcar), bhd_bartm_results_mcar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "green", lwd = 3, type = "b")
	points(rownames(bhd_results_rf_mcar), bhd_results_rf_mcar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "black", lwd = 3, type = "b")
	points(rownames(bhd_results_bart_with_imp_mcar), bhd_results_bart_with_imp_mcar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "brown", lwd = 3, type = "b")

}



############################  MAR

if (iter_num == 6){

	oos_rmse_bhd_bartm_mar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){	
		for (nsim in 1 : Nsim){	
			Xm = knockout_mar(X, KnockoutPROP[i])
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			if (nrow(na.omit(Xtrain)) == nrow(Xtrain)){
				Xtrain_imputed = cbind(Xtrain, ytrain)
			} else {
				Xtrain_imputed = rfImpute(Xtrain, ytrain)	
				
			}
			Xtrain_imputed = Xtrain_imputed[, 2 : ncol(Xtrain_imputed)]
			
			bart_machine = build_bart_machine(Xtrain_imputed, ytrain, verbose = FALSE, run_in_sample = FALSE, num_burn_in = BURN_IN, use_missing_data = FALSE)
			imputed = missForest(rbind(Xtest, Xtrain), verbose = TRUE)$ximp		
			Xtest_miss_rf = imputed[1 : n_test, ]		
			predict_obj = bart_predict_for_test_data(bart_machine, Xtest_miss_rf, ytest)
			destroy_bart_machine(bart_machine)
			oos_rmse_bhd_bartm_mar[i, nsim] = predict_obj$rmse
			print(oos_rmse_bhd_bartm_mar)
		}
	}
	
	
	bhd_bartm_results_mar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_bartm_mar)
	rownames(bhd_bartm_results_mar) = c(0, KnockoutPROP)
	bhd_bartm_results_mar = cbind(bhd_bartm_results_mar, apply(bhd_bartm_results_mar, 1, mean))
	write.csv(bhd_bartm_results_mar, "bhd_bart_with_rf_impute_results_mar.csv")
	bhd_bartm_results_mar = read.csv("bhd_bartm_results_mar.csv", header = T, row.names = 1)
	bhd_bart_with_rf_impute_results_mar = read.csv("bhd_bart_with_rf_impute_results_mar.csv", header = T, row.names = 1)

}

if (iter_num == 7){
	
	oos_rmse_bhd_lm_mar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){
			Xm = knockout_mar(X, KnockoutPROP[i])
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			Xtest = imputeMatrixByXbarj(Xtest, Xtrain)
			Xtrain = imputeMatrixByXbarj(Xtrain, Xtrain)
			Xytrain = data.frame(Xtrain , ytrain)
			lm_mod = lm(ytrain ~., data = Xytrain)
			y_hat = predict(lm_mod, newdata = Xtest)
			oos_rmse_bhd_lm_mar[i, nsim] = sqrt((sum(ytest - y_hat)^2) / n_test) 
	#		print(oos_rmse_crazy_model_lm)
		}
	}
	
	
	bhd_lm_results_mar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_lm_mar)
	rownames(bhd_lm_results_mar) = c(0, KnockoutPROP)
	bhd_lm_results_mar = cbind(bhd_lm_results_mar, apply(bhd_lm_results_mar, 1, mean))
	write.csv(bhd_lm_results_mar, "bhd_lm_results_mar.csv")
	bhd_lm_results_mar = read.csv("bhd_lm_results_mar.csv", header = T, row.names = 1)

}

if (iter_num == 8){

	oos_rmse_bhd_xbarj_no_M_mar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){	
			Xm = knockout_mar(X, KnockoutPROP[i])	
			
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			bart_machine = build_bart_machine(X = Xtrain, y = ytrain, verbose = TRUE, run_in_sample = FALSE, replace_missing_data_with_x_j_bar = TRUE, use_missing_data = FALSE, num_burn_in = BURN_IN)
			
			Xtest = imputeMatrixByXbarj(Xtest, Xtrain)
			predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
			destroy_bart_machine(bart_machine)
			oos_rmse_bhd_xbarj_no_M_mar[i, nsim] = predict_obj$rmse
			print(oos_rmse_bhd_xbarj_no_M_mar)
		}
	}
	
	bhd_xbarj_no_M_results_mar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_xbarj_no_M_mar)
	rownames(bhd_xbarj_no_M_results_mar) = c(0, KnockoutPROP)
	bhd_xbarj_no_M_results_mar = cbind(bhd_xbarj_no_M_results_mar, apply(bhd_xbarj_no_M_results_mar, 1, mean))
	write.csv(bhd_xbarj_no_M_results_mar, "bhd_xbarj_no_M_results_mar.csv")
	bhd_xbarj_no_M_results_mar = read.csv("bhd_xbarj_no_M_results_mar.csv", header = T, row.names = 1)
}


if (iter_num == 9){

	oos_rmse_bhd_rf_mar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){	
			Xm = knockout_mar(X, KnockoutPROP[i])	
			
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			
			if (nrow(na.omit(Xtrain)) == nrow(Xtrain)){
				rf_mod = randomForest(x = Xtrain, y = ytrain)				
			} else {
				rf_mod = randomForest(ytrain ~ ., rfImpute(Xtrain, ytrain))		
			}
			#to get Xtest, impute also using Xtrain
			imputed = missForest(rbind(Xtest, Xtrain), verbose = TRUE)$ximp		
			Xtest_miss_rf = imputed[1 : n_test, ]		
			
	#		Xtest_miss_rf = imputeMatrixByXbarj(Xtest, Xtrain)
			y_hat = predict(rf_mod, Xtest_miss_rf)
			oos_rmse_bhd_rf_mar[i, nsim] = sqrt((sum(ytest - y_hat)^2) / n_test)
			print(oos_rmse_bhd_rf_mar)
		}
	}
	
	bhd_rf_results_mar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_rf_mar)
	rownames(bhd_rf_results_mar) = c(0, KnockoutPROP)
	bhd_rf_results_mar = cbind(bhd_rf_results_mar, apply(bhd_rf_results_mar, 1, mean))
	write.csv(bhd_rf_results_mar, "bhd_rf_results_mar.csv")
	bhd_rf_results_mar = read.csv("bhd_rf_results_mar.csv", header = T, row.names = 1)

}


if (iter_num == 10){
	
	oos_rmse_bhd_bart_with_imp_mar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i_knockout in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){	
			Xm = knockout_mar(X, KnockoutPROP[i_knockout])	
			
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			bart_machine = build_bart_machine(Xtrain, ytrain, verbose = FALSE, run_in_sample = FALSE, impute_missingness_with_rf_impute = TRUE, debug_log = TRUE, num_burn_in = BURN_IN)
			predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
			destroy_bart_machine(bart_machine)
			oos_rmse_bhd_bart_with_imp_mar[i_knockout, nsim] = predict_obj$rmse
			print(oos_rmse_bhd_bart_with_imp_mar)
		}
	}
	
	bhd_results_bart_with_imp_mar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_bart_with_imp_mar)
	rownames(bhd_results_bart_with_imp_mar) = c(0, KnockoutPROP)
	bhd_results_bart_with_imp_mar = cbind(bhd_results_bart_with_imp_mar, apply(bhd_results_bart_with_imp_mar, 1, mean))
	write.csv(bhd_results_bart_with_imp_mar, "bhd_results_bart_with_imp_mar.csv")
	bhd_results_bart_with_imp_mar = read.csv("bhd_results_bart_with_imp_mar.csv", header = T, row.names = 1)

}

if (NOT_ON_GRID){
	par(mar = c(4,4,0.5,0.5))
	windows()
	plot(rownames(bhd_xbarj_no_M_results_mar), bhd_xbarj_no_M_results_mar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), 
			type = "b", 
#			main = "MAR", 
			xlab = "Proportion Data Missing", 
			ylab = "Multiple of Baseline Error", ylim = c(1, 2.5),
			lwd = 3,
			col = "purple")
	
	points(rownames(bhd_lm_results_mar), bhd_lm_results_mar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "red", lwd = 3, type = "b")
	points(rownames(bhd_bartm_results_mar), bhd_bartm_results_mar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "green", lwd = 3, type = "b")
	points(rownames(bhd_rf_results_mar), bhd_rf_results_mar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "black", lwd = 3, type = "b")
	points(rownames(bhd_results_bart_with_imp_mar), bhd_results_bart_with_imp_mar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "brown", lwd = 3, type = "b")
	points(rownames(bhd_bart_with_rf_impute_results_mar), bhd_bart_with_rf_impute_results_mar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "blue", lwd = 3, type = "b")
	
	
}



############################  NMAR

if (iter_num == 11){

	oos_rmse_bhd_bartm_nmar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){	
		for (nsim in 1 : Nsim){	
			Xm = knockout_nmar(X, KnockoutPROP[i])
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			bart_machine = build_bart_machine(Xtrain, ytrain, verbose = FALSE, run_in_sample = FALSE, num_burn_in = BURN_IN)
			predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
			destroy_bart_machine(bart_machine)
			oos_rmse_bhd_bartm_nmar[i, nsim] = predict_obj$rmse
			print(oos_rmse_bhd_bartm_nmar)
		}
	}
	
	
	bhd_bartm_results_nmar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_bartm_nmar)
	rownames(bhd_bartm_results_nmar) = c(0, KnockoutPROP)
	bhd_bartm_results_nmar = cbind(bhd_bartm_results_nmar, apply(bhd_bartm_results_nmar, 1, mean))
	write.csv(bhd_bartm_results_nmar, "bhd_bartm_results_nmar.csv")
	bhd_bartm_results_nmar = read.csv("bhd_bartm_results_nmar.csv", header = T, row.names = 1)

}


if (iter_num == 12){

	oos_rmse_bhd_lm_nmar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){
			Xm = knockout_nmar(X, KnockoutPROP[i])
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			Xtest = imputeMatrixByXbarj(Xtest, Xtrain)
			Xtrain = imputeMatrixByXbarj(Xtrain, Xtrain)
			Xytrain = data.frame(Xtrain , ytrain)
			lm_mod = lm(ytrain ~., data = Xytrain)
			y_hat = predict(lm_mod, newdata = Xtest)
			oos_rmse_bhd_lm_nmar[i, nsim] = sqrt((sum(ytest - y_hat)^2) / n_test) 
	#		print(oos_rmse_crazy_model_lm)
		}
	}
	
	
	bhd_lm_results_nmar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_lm_nmar)
	rownames(bhd_lm_results_nmar) = c(0, KnockoutPROP)
	bhd_lm_results_nmar = cbind(bhd_lm_results_nmar, apply(bhd_lm_results_nmar, 1, mean))
	write.csv(bhd_lm_results_nmar, "bhd_lm_results_nmar.csv")
	bhd_lm_results_nmar = read.csv("bhd_lm_results_nmar.csv", header = T, row.names =1 )
}

if (iter_num == 13){

	oos_rmse_bhd_xbarj_no_M_nmar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){	
			Xm = knockout_nmar(X, KnockoutPROP[i])	
			
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			bart_machine = build_bart_machine(X = Xtrain, y = ytrain, verbose = TRUE, run_in_sample = FALSE, replace_missing_data_with_x_j_bar = TRUE, use_missing_data = FALSE, num_burn_in = BURN_IN)
			
			Xtest = imputeMatrixByXbarj(Xtest, Xtrain)
			predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
			destroy_bart_machine(bart_machine)
			oos_rmse_bhd_xbarj_no_M_nmar[i, nsim] = predict_obj$rmse
			print(oos_rmse_bhd_xbarj_no_M_nmar)
		}
	}
	
	bhd_xbarj_no_M_results_nmar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_xbarj_no_M_nmar)
	rownames(bhd_xbarj_no_M_results_nmar) = c(0, KnockoutPROP)
	bhd_xbarj_no_M_results_nmar = cbind(bhd_xbarj_no_M_results_nmar, apply(bhd_xbarj_no_M_results_nmar, 1, mean))
	write.csv(bhd_xbarj_no_M_results_nmar, "bhd_xbarj_no_M_results_nmar.csv")
	bhd_xbarj_no_M_results_nmar = read.csv("bhd_xbarj_no_M_results_nmar.csv", header = T, row.names =1)
}


if (iter_num == 14){
	
	oos_rmse_bhd_rf_nmar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){	
			Xm = knockout_nmar(X, KnockoutPROP[i])	
			
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			
			if (nrow(na.omit(Xtrain)) == nrow(Xtrain)){
				rf_mod = randomForest(x = Xtrain, y = ytrain)				
			} else {
				rf_mod = randomForest(ytrain ~ ., rfImpute(Xtrain, ytrain))		
			}
			#to get Xtest, impute also using Xtrain
			imputed = missForest(rbind(Xtest, Xtrain), verbose = TRUE)$ximp		
			Xtest_miss_rf = imputed[1 : n_test, ]		
			
			y_hat = predict(rf_mod, Xtest_miss_rf)
			oos_rmse_bhd_rf_nmar[i, nsim] = sqrt((sum(ytest - y_hat)^2) / n_test)
			print(oos_rmse_bhd_rf_nmar)
		}
	}
	
	bhd_results_rf_nmar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_rf_nmar)
	rownames(bhd_results_rf_nmar) = c(0, KnockoutPROP)
	bhd_results_rf_nmar = cbind(bhd_results_rf_nmar, apply(bhd_results_rf_nmar, 1, mean))
	write.csv(bhd_results_rf_nmar, "bhd_results_rf_nmar.csv")
	bhd_results_rf_nmar = read.csv("bhd_results_rf_nmar.csv", header = T, row.names = 1)
}

if (iter_num == 15){

	oos_rmse_bhd_bart_with_imp_nmar = matrix(NA, nrow = length(KnockoutPROP), ncol = Nsim)
	
	for (i_knockout in 1 : length(KnockoutPROP)){
		for (nsim in 1 : Nsim){	
			Xm = knockout_mar(X, KnockoutPROP[i_knockout])	
			
			test_indices = sample(1 : nrow(X), n_test)
			Xtest = Xm[test_indices, ]
			ytest = y[test_indices]
			Xtrain = Xm[-test_indices, ]
			ytrain = y[-test_indices]
			bart_machine = build_bart_machine(Xtrain, ytrain, verbose = FALSE, run_in_sample = FALSE, impute_missingness_with_rf_impute = TRUE, debug_log = TRUE, num_burn_in = BURN_IN)
			predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
			destroy_bart_machine(bart_machine)
			oos_rmse_bhd_bart_with_imp_nmar[i_knockout, nsim] = predict_obj$rmse
			print(oos_rmse_bhd_bart_with_imp_nmar)
		}
	}
	
	bhd_results_bart_with_imp_nmar = rbind(oos_rmse_vanilla_bhd, oos_rmse_bhd_bart_with_imp_nmar)
	rownames(bhd_results_bart_with_imp_nmar) = c(0, KnockoutPROP)
	bhd_results_bart_with_imp_nmar = cbind(bhd_results_bart_with_imp_nmar, apply(bhd_results_bart_with_imp_nmar, 1, mean))
	write.csv(bhd_results_bart_with_imp_nmar, "bhd_results_bart_with_imp_nmar.csv")
	bhd_results_bart_with_imp_nmar = read.csv("bhd_results_bart_with_imp_nmar.csv", header= T, row.names = 1)
}

if (NOT_ON_GRID){

	windows()
	plot(rownames(bhd_xbarj_no_M_results_nmar), bhd_xbarj_no_M_results_nmar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), 
			type = "n", 
#			main = "NMAR", 
			xlab = "Proportion Data Missing", 
			ylab = "Multiple of Baseline Error", ylim = c(0.95, 1.5),
			lwd = 3,
			col = "purple")
	
#	points(rownames(bhd_lm_results_nmar), bhd_lm_results_nmar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "red", lwd = 3, type = "b")
	points(rownames(bhd_bartm_results_nmar), bhd_bartm_results_nmar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "green", lwd = 3, type = "b")
	points(rownames(bhd_results_rf_nmar), bhd_results_rf_nmar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "black", lwd = 3, type = "b")
	points(rownames(bhd_results_bart_with_imp_nmar), bhd_results_bart_with_imp_nmar[, Nsim + 1] / mean(oos_rmse_vanilla_bhd), col = "brown", lwd = 3, type = "b")

}






###std errors table 3


#NMAR
gamma_ind = 4

sd(as.numeric(bhd_bartm_results_nmar[gamma_ind, 1:Nsim]))
sd(as.numeric(bhd_xbarj_no_M_results_nmar[gamma_ind, 1:Nsim]))
sd(as.numeric(bhd_results_rf_nmar[gamma_ind, 1:Nsim]))
sd(as.numeric(bhd_lm_results_nmar[gamma_ind, 1:Nsim]))

sd(as.numeric(bhd_bartm_results_mar[gamma_ind, 1:Nsim]))
sd(as.numeric(bhd_xbarj_no_M_results_mar[gamma_ind, 1:Nsim]))
sd(as.numeric(bhd_rf_results_mar[gamma_ind, 1:Nsim]))
sd(as.numeric(bhd_lm_results_mar[gamma_ind, 1:Nsim]))

sd(as.numeric(bhd_bartm_results_mcar[gamma_ind, 1:Nsim]))
sd(as.numeric(bhd_xbarj_no_M_results_mcar[gamma_ind, 1:Nsim]))
sd(as.numeric(bhd_results_rf_mcar[gamma_ind, 1:Nsim]))
sd(as.numeric(bhd_lm_results_mcar[gamma_ind, 1:Nsim]))


sd(as.numeric(bhd_bart_with_rf_impute_results_mar[gamma_ind, 1:Nsim])) 
