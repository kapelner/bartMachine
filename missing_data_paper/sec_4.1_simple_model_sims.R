library(bartMachine)
library(MASS)

Xy = generate_simple_model_with_missingness(500, 0, 20, gamma = 0.1)
Xytest = generate_simple_model_with_missingness(500, 0, 20, gamma = 0.1)

set_bart_machine_num_cores(4)
bart_machine = build_bart_machine(Xy = Xy,
		num_trees = 200,
		num_burn_in = 1000,
		cov_prior_vec = c(1, 10),
		num_iterations_after_burn_in = 1000,
		use_missing_data = TRUE,
		use_missing_data_dummies_as_covars = TRUE)
bart_machine
#plot_convergence_diagnostics(bart_machine)
#plot_tree_depths(bart_machine)
#bart_machine$training_data_features
#head(bart_machine$model_matrix_training_data)
plot_y_vs_yhat(bart_machine)
plot_y_vs_yhat(bart_machine, ppis = TRUE)
X1 = as.matrix(Xytest[, 1])
colnames(X1) = colnames(Xytest)[1]
windows()
plot_y_vs_yhat(bart_machine, X = X1, y = Xytest[, 2], ppis = TRUE)

x_new = as.matrix(NA)
colnames(x_new) = "X_1"
predict_obj = bart_machine_get_posterior(bart_machine, x_new)
hist(predict_obj$y_hat_posterior_samples, br=100)
predict_obj$y_hat




par(mar = c(2,4,0.5,0.5))
n_probit = 500
training_data = generate_simple_model_probit_with_missingness(n = n_probit, mu_1 = -0.5, mu_2 = 0, gamma = 0.2) #overlapped a lot
#training_data = generate_simple_model_probit_with_missingness(n = n_probit, mu_1 = -1, mu_2 = 1, gamma = 0.2)
Xy = training_data$Xy
true_probs = training_data$probs

set_bart_machine_num_cores(4)
bart_machine = build_bart_machine(Xy = Xy,
	num_burn_in = 1000,
	num_trees = 200,
	use_missing_data = TRUE,
	use_missing_data_dummies_as_covars = TRUE)
bart_machine

x_new = as.data.frame(Xy[, 1])
colnames(x_new) = "X_1"
probit_hat = predict(bart_machine, x_new, type = "prob")
cred_intervals_phat = calc_credible_intervals(bart_machine, x_new)


windows()
par(mar = c(5, 5, 0.5, 0.5))
plot(true_probs, probit_hat, xlim = c(0,1), ylim = c(0,1), xlab = "p", ylab = expression(hat(p)))
for (i in 1 : n_probit){
	p = true_probs[i]
	a = cred_intervals_phat[i, 1]
	b = cred_intervals_phat[i, 2]
	
	segments(p, a, p, b, col = "gray")
}
points(true_probs, probit_hat, pch = 16)
abline(a = 0, b = 1)

##now do cc-analysis
Xyp = cbind(Xy, true_probs)
Xyp = na.omit(Xyp)
Xy_cc = Xyp[, 1 : 2]
true_probs_cc = Xyp[, 3]

#bart_machine = build_bart_machine(Xy = Xy_cc,
#		num_burn_in = 1000,
#		num_trees = 200)
#bart_machine
#
#x_new = as.data.frame(Xy_cc[, 1])
#colnames(x_new) = "X_1"
#p_hat = predict(bart_machine, x_new, type = "prob")
#cred_intervals_phat = calc_credible_intervals(bart_machine, x_new)
#
#
#windows()
#par(mar = c(5, 5, 0.5, 0.5))
#plot(true_probs_cc, p_hat, xlim = c(0,1), ylim = c(0,1), xlab = "p", ylab = expression(hat(p)))
#for (i in 1 : nrow(Xy_cc)){
#	p = true_probs_cc[i]
#	a = cred_intervals_phat[i, 1]
#	b = cred_intervals_phat[i, 2]
#	
#	segments(p, a, p, b, col = "gray")
#}
#points(true_probs_cc, p_hat, pch = 16)
#abline(a = 0, b = 1)


probit_mod = glm(Y ~ X_1, family = binomial(link = probit), data = Xy_cc)
probit_hat = predict(probit_mod, Xy_cc)

points(true_probs_cc, pnorm(probit_hat), xlim = c(0,1), ylim = c(0,1), xlab = "p", ylab = expression(hat(p)), col = "blue")
#abline(a = 0, b = 1)












