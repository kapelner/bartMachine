knockout_mcar = function(X, prop){
	for (i in 1 : nrow(X)){
		for (j in 1 : ncol(X)){
			if (runif(1) < prop){
				X[i, j] = NA
			}
		}
	}
	X
}


knockout_mar = function(X, prop){
	for (i in 1 : nrow(X)){
		for (j in 1 : 12){
			if (X$lstat[i] > 15){
				if (runif(1) < prop){
					X[i, j] = NA
				}
			}
			if (X$lstat[i] < 5 && j == 6){
				if (runif(1) < prop){
					X[i, j] = NA
				}        
			}
		}
	}
	X
}


knockout_nmar = function(X, prop){
	for (i in 1 : nrow(X)){
		if (X$lstat[i] < 10){
			if (runif(1) < prop){
				X$lstat[i] = NA
			}
		}
		if (X$crim[i] > 0.25){
			if (runif(1) < prop){
				X$crim[i] = NA
			}
		}
		if (X$tax[i] > 400){
			if (runif(1) < prop){
				X$tax[i] = NA
			}
		}		
		if (X$rm[i] <= 5.5 || X$rm[i] >= 7){
			if (runif(1) < prop){
				X$rm[i] = NA
			}
		}
		if (X$age[i] > 60){
			if (runif(1) < prop){
				X$age[i] = NA
			}
		}		
	}
	X
}


generate_crazy_model = function(n_crazy, prop, missing_offset, sigma_e, Sigma = NULL, mu_vec = NULL){
	p_crazy = 3
	
	if (is.null(Sigma)){
		Sigma = 0.9 * diag(p_crazy) + 0.1
		Sigma[1, p_crazy] = 2 * Sigma[1, p_crazy]
		Sigma[p_crazy, 1] = 2 * Sigma[p_crazy, 1]
	}
	if (is.null(mu_vec)){
		mu_vec = rep(0, p_crazy)
	}
	
	Xs_crazy = mvrnorm(n_crazy, mu_vec, Sigma)
	error_crazy = rnorm(n_crazy, 0, sigma_e)
	X1 = Xs_crazy[, 1]
	X2 = Xs_crazy[, 2]
	X3 = Xs_crazy[, 3]
	
	y_crazy = X1 + X2 + X3 - X1^2 + X2^2 + X1 * X2 + error_crazy
	
	#X1 is MCAR w.p. gamma
	for (i in 1 : n_crazy){
		if (runif(1) < prop){
			Xs_crazy[i, 1] = NA
		}
	}
	
	#X3 is MAR w.p. gamma if X1 > 0
	for (i in 1 : n_crazy){
		if (runif(1) < prop && X1[i] > 0){
			Xs_crazy[i, 3] = NA
		}
	}
	
	#X2 is NMAR w.p. gamma if X2 > 0
	for (i in 1 : n_crazy){
		if (runif(1) < prop && X2[i] > 0){
			Xs_crazy[i, 2] = NA
		}
	}
	
	#if X3 is missing, y bumps up by 3
	for (i in 1 : n_crazy){
		if (is.na(Xs_crazy[i, 3])){
			y_crazy[i] = y_crazy[i] + missing_offset
		}
	}
	
	data.frame(Xs_crazy, y_crazy)
}

plot_hist_of_posterior = function(pred, expectation){
	hist(pred$y_hat_posterior_samples[1,], 
		br = 50,
		main = "",
		xlab = ""
	)
	cat("yhat =", round(mean(pred$y_hat_posterior_samples[1,]), 2), "se(yhat) =", round(sd(pred$y_hat_posterior_samples[1,]), 3), "\n")
	abline(v = expectation, col = "blue", lwd = 3)
	abline(v = pred$y_hat[1], col = "green", lwd = 3)
	abline(v = pred$ppi_a[1], col = "orange", lwd = 3)
	abline(v = pred$ppi_b[1], col = "orange", lwd = 3)
}

generate_simple_model_with_missingness = function(n, mu_1, mu_2, sigma_1 = 1, sigma_2 = 1, gamma = 0.1){
	X_1 = rnorm(n, mu_1, sigma_1)
	X_1 = ifelse(runif(n) < gamma, NA, X_1)
	X_2 = rnorm(n, mu_2, sigma_2)
	Y = ifelse(is.na(X_1), X_2, X_1)
	cbind(X_1, Y)
}

generate_simple_model_probit_with_missingness = function(n, mu_1 = -1, mu_2 = 1, sigma_1 = 0.2, sigma_2 = 0.2, gamma = 0.1){
	X_1 = rnorm(n, mu_1, sigma_1)
	X_1 = ifelse(runif(n) < gamma, NA, X_1)
	X_2 = rnorm(n, mu_2, sigma_2)
	Z = ifelse(is.na(X_1), X_2, X_1)
	probs = pnorm(Z) #probit model
	h1 = hist(probs[!is.na(X_1)], br = 25) #main = "missing distribution in red, non-missing in blue",
	h2 = hist(probs[is.na(X_1)], br = 25)	
	plot(h1, xlim = c(0, 1), col = rgb(0,0,1,1/4), xlab = "", main = "")
	plot(h2, add = TRUE, col = rgb(1,0,0,1/4), xlab = "", main = "")
	abline(v = mean(probs[!is.na(X_1)]), lwd = 3)
	abline(v = mean(probs[is.na(X_1)]), lwd = 3)
	Y = as.factor(rbinom(n, 1, probs))
	list(Xy = data.frame(X_1, Y), probs = probs)
}


