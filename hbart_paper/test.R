n = 200

gamma = 7
beta = 100

X = runif(n, 0, 1)
sigsqs = exp(X * gamma)
sigmas = sqrt(sigsqs)
y = beta * X + rnorm(n, 0, sigmas)
max(y) - min(y)

#write.csv(cbind(X, y), "r_hbart.csv")

library(bartMachine, lib.loc = "C:/Program Files/R/R-3.0.2/library/")
graphics.off()
set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)
plot(X, y)
bart_machine = build_bart_machine(Xy = cbind(X, y))
bart_machine


#plot(bart_machine$y_hat, y)


#check_bart_error_assumptions(bart_machine)

hbart_machine = build_bart_machine(Xy = cbind(X, y), use_heteroskedastic_linear_model = TRUE)
hbart_machine
windows()
plot(X, bart_machine$y_hat, col = "red")
points(X, hbart_machine$y_hat, col = "green")

sigsqs_hetero_after_burn_in = get_sigsqs_hetero(hbart_machine)
sigma_hats = colMeans(sqrt(sigsqs_hetero_after_burn_in))

windows()
plot(sigmas, sigma_hats, xlim = c(0, max(sigmas)), ylim = c(0, max(sigma_hats)))

abline(a = 0, b = 1)
cred_ints_sigmas = sqrt(apply(sigsqs_hetero_after_burn_in, 2, quantile, c(0.025, 0.975)))

windows()
plot(1 : n, sigmas, ylim = c(min(sigmas) * 0.5, max(sigmas) * 1.5))
covereds = array(NA, n)
for (i in 1 : n){
	a = cred_ints_sigmas[1, i]
	b = cred_ints_sigmas[2, i]
	covereds[i] = sigmas[i] > a && sigmas[i] < b
	segments(i, a, i, b, col = ifelse(covereds[i], "green", "red"))
}
points(1 : n, sigmas)
sum(covereds) / n


windows()
###look at some points individually
for (i in 1 : n){
#	i = 56
	gibbs_samples = sqrt(sigsqs_hetero_after_burn_in[, i])
	hist(gibbs_samples, br = 100)
	abline(v = sigmas[i], col = "blue")
	abline(v = quantile(gibbs_samples, 0.025), col = "red")
	abline(v = quantile(gibbs_samples, 0.975), col = "red")
	Sys.sleep(1)
}

hbart_machine_shrunk = build_bart_machine(Xy = cbind(X, y), use_heteroskedastic_linear_model = TRUE, hyper_sigma_weights = c(1, 1))
hbart_machine_shrunk

hbart_machine_unshrunk = build_bart_machine(Xy = cbind(X, y), use_heteroskedastic_linear_model = TRUE, hyper_sigma_weights = c(1e9, 1))
hbart_machine_unshrunk
#plot_y_vs_yhat(hbart_machine)

plot(X, bart_machine$y_hat)
points(X, hbart_machine$y_hat, col = "green")
points(X, hbart_machine_shrunk$y_hat, col = "red")
points(X, hbart_machine_unshrunk$y_hat, col = "blue")

#oos testing
Xtest = runif(n)
sigsq_test = exp(Xtest * gamma)
ytest = beta * Xtest + rnorm(n, 0, sqrt(sigsq_test))
Xtest = data.frame(Xtest)
colnames(Xtest) = c("X")

bart_oosrmse = bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse
hbart_oosrmse = bart_predict_for_test_data(hbart_machine, Xtest, ytest)$rmse
bart_oosrmse
hbart_oosrmse
#how much better does it do?
(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100

#leave one out
hbart_oosrmse = k_fold_cv(data.frame(X), y, k_folds = 50, verbose = F, use_heteroskedastic_linear_model = TRUE)$rmse
bart_oosrmse = k_fold_cv(data.frame(X), y, k_folds = 50, verbose = F)$rmse
#how much better does it do?
(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100

######################JB MODEL

n = 500
x1 = rep(1,times = n)
x2 = runif(n, 0, 400)
x3 = runif(n, 10, 23)
x4 = runif(n, 0, 10)
X = cbind(x1,x2,x3,x4)
graphics.off()

beta_vec = c(-35, .35, -1.7, 0)
gamma_vec = c(-6, .03, .02, .05)
sigsqs = exp(X %*% gamma_vec)
sigmas = sqrt(sigsqs)

y = X %*% beta_vec + rnorm(n, mean = 0, sd = sigmas)
plot(X[, 2], y)
plot(X[, 3], y)

bart_machine = build_bart_machine(Xy = cbind(X, y))
bart_machine


#plot(bart_machine$y_hat, y)


#check_bart_error_assumptions(bart_machine)

hbart_machine = build_bart_machine(Xy = cbind(X, y), use_heteroskedastic_linear_model = TRUE)
hbart_machine

#leave one out
hbart_oosrmse = k_fold_cv(data.frame(X), as.numeric(y), k_folds = 10, verbose = F, use_heteroskedastic_linear_model = TRUE)$rmse
bart_oosrmse = k_fold_cv(data.frame(X), as.numeric(y), k_folds = 10, verbose = F)$rmse
#how much better does it do?
(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100


windows()
plot(X %*% beta_vec, bart_machine$y_hat, col = "red")
points(X %*% beta_vec, hbart_machine$y_hat, col = "green")

sigsqs_hetero_after_burn_in = get_sigsqs_hetero(hbart_machine)
sigma_hats = colMeans(sqrt(sigsqs_hetero_after_burn_in))

windows()
plot(sigmas, sigma_hats, xlim = c(0, max(sigmas)), ylim = c(0, max(sigma_hats)))

abline(a = 0, b = 1)
cred_ints_sigmas = sqrt(apply(sigsqs_hetero_after_burn_in, 2, quantile, c(0.025, 0.975)))

windows()
plot(1 : n, sigmas, ylim = c(min(sigmas) * 0.5, max(sigmas) * 1.5))
covereds = array(NA, n)
for (i in 1 : n){
	a = cred_ints_sigmas[1, i]
	b = cred_ints_sigmas[2, i]
	covereds[i] = sigmas[i] > a && sigmas[i] < b
	segments(i, a, i, b, col = ifelse(covereds[i], "green", "red"))
}
points(1 : n, sigmas)
sum(covereds) / n


n_test = 2000
x1 = rep(1, n_test)
x2 = runif(n_test, 0, 400)
x3 = runif(n_test, 10, 23)
x4 = runif(n_test, 0, 10)
Xtest = cbind(x1,x2,x3,x4)

sigsqs_test = exp(Xtest %*% gamma_vec)
sigmas_test = sqrt(sigsqs_test)

exp_y_given_X_test = Xtest %*% beta_vec
ytest = exp_y_given_X_test + rnorm(n, mean = 0, sd = sigmas_test)
Xtest = data.frame(Xtest)
colnames(Xtest) = c("x1", "x2", "x3", "x4")

bart_oosrmse = bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse
hbart_oosrmse = bart_predict_for_test_data(hbart_machine, Xtest, ytest)$rmse
bart_oosrmse
hbart_oosrmse
#how much better does it do?
(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100


plot_y_vs_yhat(hbart_machine, Xtest = Xtest, ytest = exp_y_given_X_test, cred = T)
plot_y_vs_yhat(bart_machine, Xtest = Xtest, ytest = exp_y_given_X_test, cred = T)

plot_y_vs_yhat(hbart_machine, Xtest = Xtest, ytest = exp_y_given_X_test, pred = T)
plot_y_vs_yhat(bart_machine, Xtest = Xtest, ytest = exp_y_given_X_test, pred = T)




#### BHD
library(bartMachine)
set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)


library(MASS)
data(Boston)
X = Boston
y = X$medv
X$medv = NULL

Xtrain = X[1 : (nrow(X) / 2), ]
ytrain = y[1 : (nrow(X) / 2)]
Xtest = X[(nrow(X) / 2 + 1) : nrow(X), ]
ytest = y[(nrow(X) / 2 + 1) : nrow(X)]


#bart_machine = build_bart_machine(X, y)
#bart_machine

#heteroskedasticity_test(bart_machine = bart_machine)
#
##let's take a look at this model and see if we see blatant heteroskedasticity linearly in any attribute
#bart_machine_for_investigating_hetero = build_bart_machine(X, log(bart_machine$residuals^2))
#for (feature in colnames(X)){
#	pd_plot(bart_machine_for_investigating_hetero, j = feature)
#	windows()
#}

bart_machine = build_bart_machine(Xtrain, ytrain)
bart_machine

#do a linear model with just tax and nox
hbart_machine = build_bart_machine(Xtrain, ytrain, use_heteroskedastic_linear_model = TRUE, 
		Z_heteroskedastic_model = Xtrain[, c("nox"), drop = FALSE])
hbart_machine

ggs = get_gammas_hetero(hbart_machine)
heteroskedastic_linear_model_significance(ggs)

bart_oosrmse = bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse
hbart_oosrmse = bart_predict_for_test_data(hbart_machine, Xtest, ytest)$rmse
bart_oosrmse
hbart_oosrmse
#how much better does it do?
(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100


k_fold_cv(X, y, k_folds = Inf, verbose = F, use_heteroskedastic_linear_model = TRUE, Z = X[, c("nox"), drop = FALSE])$rmse
k_fold_cv(X, y, k_folds = Inf, verbose = F)$rmse


###motorcycle
library(bartMachine)
library(boot)
data(motor)
library(tgp)
library(dynaTree)
y = motor$accel
X = as.data.frame(motor$times)

MAX_POLY = 2
mod = lm(y ~ poly(motor$time, MAX_POLY))
Z = as.matrix(mod$model)[, 2 : (MAX_POLY + 1)]


bart_machine = build_bart_machine(X, y, num_burn_in = 1500)
bart_machine

hbart_machine = build_bart_machine(X, y, 
		use_heteroskedastic_linear_model = TRUE,
		Z_heteroskedastic_model = Z)
hbart_machine
pred_ints = calc_prediction_intervals(hbart_machine, X, Z_new_data = Z, pi_conf = 0.90)

moto.btgpllm <- btgpllm(X=X, Z=y, bprior="b0", verb=0)
moto.btgpllm.p <- predict(moto.btgpllm) 
moto.btgpllm.p$Zp.q1
moto.btgpllm.p$Zp.q2

dtree = dynaTree(X,y)
dpreds = predict(dtree,X)

plot(moto.btgpllm, main='treed GP LLM,', layout='surf', ylim = c(-200,100))


plot(X[, 1], bart_machine$y_hat, col = "red", ylim = c(-160, 120))
points(X[, 1], y, pch = "+")
points(X[, 1], hbart_machine$y_hat, col = "blue")
cred_ints = calc_credible_intervals(bart_machine, X, ci_conf = 0.90)
lines(X[, 1], cred_ints[, 1], col = "red", lty = 2)
lines(X[, 1], cred_ints[, 2], col = "red", lty = 2)
pred_ints = calc_prediction_intervals(bart_machine, X, pi_conf = 0.90)
lines(X[, 1], pred_ints[, 1], col = "gold", lty = 1, lwd = 1)
lines(X[, 1], pred_ints[, 2], col = "gold", lty = 1,lwd = 1)

cred_ints = calc_credible_intervals(hbart_machine, X, ci_conf = 0.90)
lines(X[, 1], cred_ints[, 1], col = "blue", lty = 2)
lines(X[, 1], cred_ints[, 2], col = "blue", lty = 2)

pred_ints = calc_prediction_intervals(hbart_machine, X, Z_new_data = Z, pi_conf = 0.90)
lines(X[, 1], pred_ints[, 1], col = "forestgreen", lty = 1)
lines(X[, 1], pred_ints[, 2], col = "forestgreen", lty = 1)


lines(X[, 1], dpreds$q1, col = "purple", lty = 1)
lines(X[, 1], dpreds$q2, col = "purple", lty = 1)


hbart_oosrmse = k_fold_cv(X, y, k_folds = Inf, verbose = F, use_heteroskedastic_linear_model = TRUE, Z = Z)$rmse
bart_oosrmse = k_fold_cv(X, y, k_folds = Inf, verbose = F)$rmse

bart_oosrmse = bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse
hbart_oosrmse = bart_predict_for_test_data(hbart_machine, Xtest, ytest)$rmse
bart_oosrmse
hbart_oosrmse
#how much better does it do?
(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100



library(SemiPar)
data(lidar)
MAX_POLY = 3
mod = lm(lidar$logratio ~ poly(lidar$range, MAX_POLY))
Z = as.matrix(mod$model)[, 2 : (MAX_POLY + 1)]


bart_machine = build_bart_machine(data.frame(lidar$range), lidar$logratio, num_burn_in = 1500)
bart_machine

hbart_machine = build_bart_machine(data.frame(lidar$range), lidar$logratio, 
                                   use_heteroskedastic_linear_model = TRUE,
                                   Z_heteroskedastic_model = Z)
hbart_machine


plot(lidar$range, bart_machine$y_hat, col = "red", ylim = c(-1,.2))
points(lidar$range, lidar$logratio, pch = "+")
points(lidar$range, hbart_machine$y_hat, col = "blue")

pred_ints = calc_prediction_intervals(bart_machine, data.frame(lidar$range), pi_conf = 0.90)
lines(lidar$range, pred_ints[, 1], col = "gold", lty = 1, lwd = 1)
lines(lidar$range, pred_ints[, 2], col = "gold", lty = 1,lwd = 1)
 
pred_ints_h = calc_prediction_intervals(hbart_machine, data.frame(lidar$range), Z_new_data = Z, pi_conf = 0.90)
lines(lidar$range, pred_ints_h[, 1], col = "forestgreen", lty = 1)
lines(lidar$range, pred_ints_h[, 2], col = "forestgreen", lty = 1)


##Goldberg et al.
x = seq(0,1, length.out=100)
y = 2*sin(2*pi*x) + rnorm(100, 0, seq(.5,1.5,length.out=100))

bart_machine = build_bart_machine(Xy = cbind(x, y))
bart_machine

hbart_machine = build_bart_machine(Xy = cbind(x, y), use_heteroskedastic_linear_model = TRUE)
hbart_machine

plot(x, bart_machine$y_hat, col = "red", ylim = c(-5,5))
points(x, y, pch = "+")
points(x, hbart_machine$y_hat, col = "blue")

pred_ints = calc_prediction_intervals(bart_machine, data.frame(x), pi_conf = 0.90)
lines(x, pred_ints[, 1], col = "gold", lty = 1, lwd = 1)
lines(x, pred_ints[, 2], col = "gold", lty = 1,lwd = 1)

pred_ints_h = calc_prediction_intervals(hbart_machine, data.frame(x), Z_new_data = as.matrix(x), pi_conf = 0.90)
lines(x, pred_ints_h[, 1], col = "forestgreen", lty = 1)
lines(x, pred_ints_h[, 2], col = "forestgreen", lty = 1)

rs = numeric(100)
for(i in 1:100){
  x_test = data.frame(seq(0,1, length.out=100))
  colnames(x_test) = "x"
  y_test = 2*sin(2*pi*x_test) + rnorm(100, 0, seq(.5,1.5,length.out=100))
  bart_oosrmse = bart_predict_for_test_data(bart_machine, x_test, y_test)$rmse
  hbart_oosrmse = bart_predict_for_test_data(hbart_machine, x_test, y_test)$rmse
  rs[i]=(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100  
  if(i%%10 ==0) print(i)
}
mean(rs)


library(SemiPar)
data(fossil)
MAX_POLY = 3
mod = lm(lidar$logratio ~ poly(lidar$range, MAX_POLY))
Z = as.matrix(mod$model)[, 2 : (MAX_POLY + 1)]


bart_machine = build_bart_machine(data.frame(lidar$range), lidar$logratio, num_burn_in = 1500)
bart_machine

hbart_machine = build_bart_machine(data.frame(lidar$range), lidar$logratio, 
                                   use_heteroskedastic_linear_model = TRUE,
                                   Z_heteroskedastic_model = Z)
hbart_machine


plot(lidar$range, bart_machine$y_hat, col = "red", ylim = c(-1,.2))
points(lidar$range, lidar$logratio, pch = "+")
points(lidar$range, hbart_machine$y_hat, col = "blue")

pred_ints = calc_prediction_intervals(bart_machine, data.frame(lidar$range), pi_conf = 0.90)
lines(lidar$range, pred_ints[, 1], col = "gold", lty = 1, lwd = 1)
lines(lidar$range, pred_ints[, 2], col = "gold", lty = 1,lwd = 1)

pred_ints_h = calc_prediction_intervals(hbart_machine, data.frame(lidar$range), Z_new_data = Z, pi_conf = 0.90)
lines(lidar$range, pred_ints_h[, 1], col = "forestgreen", lty = 1)
lines(lidar$range, pred_ints_h[, 2], col = "forestgreen", lty = 1)

