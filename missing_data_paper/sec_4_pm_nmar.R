library(bartMachine)
library(MASS)
set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)


generate_response_model = function(n, sigma_e = 1, mu_b = 5, sigma_b = 0.5, Sigma = NULL, mu_vec = NULL, gamma = 0.3){
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
	b = rnorm(n, mu_b, sigma_b)
	error = rnorm(n, 0, sigma_e)
	X1 = Xs[, 1]
	X2 = Xs[, 2]
	X3 = Xs[, 3]
	y = X1 + X2 + 2 * X3 - X1^2 + X2^2 + X1 * X2 + error
	
	
	#now create missingness M2 if x2 > 0 it goes missing wp gamma = 0.20
	for (i in 1 : n){
		if (X2[i] > 0 && runif(1) < gamma){
			X2[i] = NA
		}		
	}
	
	#now create missigneness M3 if x1 > 0 w.p 0.2
	for (i in 1 : n){
		if (X1[i] > 0 && runif(1) < gamma){
			X3[i] = NA
		}		
	}	
	b = ifelse(is.na(X3), b, 0)
	y = y + b
	
	
	data.frame(cbind(X1, X2, X3), y)
}

plot_hist_of_posterior = function(pred, expectation, cis){
	hist(pred$y_hat_posterior_samples[1,], 
			br = 50,
			main = "",
			xlab = ""
	)
	cat("yhat =", round(mean(pred$y_hat_posterior_samples[1,]), 2), "se(yhat) =", round(sd(pred$y_hat_posterior_samples[1,]), 3), "\n")
	abline(v = expectation, col = "blue", lwd = 3)
	abline(v = pred$y_hat[1], col = "green", lwd = 3)
	
	abline(v = cis[1], col = "orange", lwd = 3)
	abline(v = cis[2], col = "orange", lwd = 3)
}

n = 500
mu_b = 10

graphics.off()
Xy = generate_response_model(n, mu_b = mu_b)
Xya = Xy[!is.na(Xy[, 3]), ]
Xyb = Xy[is.na(Xy[, 3]), ]
windows()
par(mar = c(2,4,0.4,1))
ya = Xya[, 4]
yb = Xyb[, 4]

#Figure 1a
#hist(ya, br = 50, col = rgb(0,0,1,0.3), xlim = c(min(ya), max(ya, yb)), xlab = "y", main = NULL)
#hist(yb, br = 30, add = TRUE, col = rgb(1,0,0,0.3))

bart_machine = build_bart_machine(Xy = Xy, use_missing_data = TRUE, use_missing_data_dummies_as_covars = TRUE, num_burn_in = 1000, run_in_sample = FALSE)
bart_machine

#Figure 1b
windows()
par(mar = c(2,4,0.4,0))
new_data = as.data.frame(t(as.matrix(c(0, 0, 0))))
colnames(new_data) = colnames(Xy[, 1 : 3])
pred = bart_machine_get_posterior(bart_machine, new_data)
expe = 0
plot_hist_of_posterior(pred, expe, calc_credible_intervals(bart_machine, new_data))
#yhat = 0.28 se(yhat) = 0.729

#Figure 1c
windows()
par(mar = c(2,4,0.4,0))
new_data = as.data.frame(t(as.matrix(c(0, NA, 0))))
colnames(new_data) = colnames(Xy[, 1 : 3])
pred = bart_machine_get_posterior(bart_machine, new_data)
expe = sqrt(2 / pi) + 1
plot_hist_of_posterior(pred, expe, calc_credible_intervals(bart_machine, new_data))
#yhat = 2.08 se(yhat) = 0.903

#Figure 1d
windows()
par(mar = c(2,4,0.4,0))
new_data = as.data.frame(t(as.matrix(c(0, 0, NA))))
colnames(new_data) = colnames(Xy[, 1 : 3])
pred = bart_machine_get_posterior(bart_machine, new_data)
expe = 0.4 * sqrt(2 / pi) + mu_b
plot_hist_of_posterior(pred, expe, calc_credible_intervals(bart_machine, new_data))
#yhat = 10.09 se(yhat) = 1.001

#Figure 1e
windows()
par(mar = c(2,4,0.4,0))
new_data = as.data.frame(t(as.matrix(c(0, NA, NA))))
colnames(new_data) = colnames(Xy[, 1 : 3])
pred = bart_machine_get_posterior(bart_machine, new_data)
expe = sqrt(2 / pi) + 0.4 * sqrt(2 / pi) + 1 + mu_b
plot_hist_of_posterior(pred, expe, calc_credible_intervals(bart_machine, new_data))
#yhat = 10.54 se(yhat) = 1.144

###additional images not included in main text of paper

windows()
par(mar = c(2,4,0.4,0))
new_data = as.data.frame(t(as.matrix(c(1, NA, NA))))
colnames(new_data) = colnames(Xy[, 1 : 3])
pred = bart_machine_get_posterior(bart_machine, new_data)
expe = sqrt(2 / pi) + 0.4 * sqrt(2 / pi) + 1 + mu_b
plot_hist_of_posterior(pred, expe, calc_credible_intervals(bart_machine, new_data))

windows()
par(mar = c(2,4,0.4,0))
new_data = as.data.frame(t(as.matrix(c(1, 1, NA))))
colnames(new_data) = colnames(Xy[, 1 : 3])
pred = bart_machine_get_posterior(bart_machine, new_data)
expe = 3 + 0.4 * sqrt(2 / pi) + mu_b
plot_hist_of_posterior(pred, expe, calc_credible_intervals(bart_machine, new_data))

windows()
par(mar = c(2,4,0.4,0))
new_data = as.data.frame(t(as.matrix(c(1, -1, NA))))
colnames(new_data) = colnames(Xy[, 1 : 3])
pred = bart_machine_get_posterior(bart_machine, new_data)
expe = -1 + 0.4 * sqrt(2 / pi) + mu_b
plot_hist_of_posterior(pred, expe, calc_credible_intervals(bart_machine, new_data))

windows()
par(mar = c(2,4,0.4,0))
new_data = as.data.frame(t(as.matrix(c(1, NA, 1))))
colnames(new_data) = colnames(Xy[, 1 : 3])
pred = bart_machine_get_posterior(bart_machine, new_data)
expe = 2 + sqrt(2 / pi) + 1
plot_hist_of_posterior(pred, expe, calc_credible_intervals(bart_machine, new_data))
