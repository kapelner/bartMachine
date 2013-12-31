library(bartMachine)
library(MASS)

#get the Boston housing data
data(Boston)
X = Boston
#X = cbind(X, rnorm(nrow(X)))
y = X$medv
X$medv = NULL

#unitize the covs
X = data.frame(apply(X, 2, function(xj){(xj - min(xj)) / (max(xj) - min(xj))}))


create_mar_model_of_bhd = function(X, beta_0, beta){
	for (i in 1 : nrow(X)){
		prob_M_rm = beta_0 + beta * X$indus[i] + beta * X$lstat[i] + beta * X$age[i]
		if (runif(1) < prob_M_rm){
			X$rm[i] = NA
		}

		prob_M_crim = beta_0 + beta * X$nox[i] + beta * X$rad[i] + beta * X$tax[i]
		if (runif(1) < prob_M_crim){
			X$crim[i] = NA
		}
	}
	X
}


beta_0 = 0
betas = c(seq(from = 0, to = 0.3, by = 0.05), 0.4, 0.5)

Nsim = 10

for (nsim in 1 : Nsim){
	for (g in 1 : length(betas)){
		beta = betas[g]
		beta = 0.5
		Xmar = create_mar_model_of_bhd(X, beta_0, beta)
		head(Xmar, 50)
		
		sum(is.na(Xmar$crim)) / nrow(Xmar)
		sum(is.na(Xmar$rm)) / nrow(Xmar)
		(nrow(Xmar) - nrow(na.omit(Xmar))) / nrow(Xmar)
	}
}