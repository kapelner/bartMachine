n = 1000

gamma = 9
beta = 100

X = runif(n)
sigsqs = exp(X * gamma)
sigmas = sqrt(sigsqs)
y = beta * X + rnorm(n, 0, sigmas)


#write.csv(cbind(X, y), "r_hbart.csv")

library(bartMachine)
graphics.off()
set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)
plot(X, y)
bart_machine = build_bart_machine(Xy = cbind(X, y))
bart_machine


#plot(bart_machine$y_hat, y)
plot(X, bart_machine$y_hat)

#check_bart_error_assumptions(bart_machine)

hbart_machine = build_bart_machine(Xy = cbind(X, y), use_heteroskedastic_linear_model = TRUE)
hbart_machine
windows()
plot(X, hbart_machine$y_hat)

sigsqs_hetero_after_burn_in = get_sigsqs_hetero(hbart_machine)
sigma_hats = colMeans(sqrt(sigsqs_hetero_after_burn_in))

windows()
plot(sigmas, sigma_hats, xlim = c(0, max(sigmas)), ylim = c(0, max(sigma_hats)))
abline(a = 0, b = 1)
cred_ints_sigmas = sqrt(apply(sigsqs_hetero_after_burn_in, 2, quantile, c(0.025, 0.975)))

windows()
plot(1 : n, sigmas, ylim = c(min(sigmas) * 0.5, max(sigmas) * 1.5))
for (i in 1 : n){
	a = cred_ints_sigmas[1, i]
	b = cred_ints_sigmas[2, i]
	segments(i, a, i, b, col = ifelse(sigmas[i] > a && sigmas[i] < b, "green", "red"))
}
points(1 : n, sigmas)


###look at some points individually
for (i in 1 : n){
#	i = 56
	hist(sqrt(sigsqs_hetero_after_burn_in[i, ]), br = 100, xlim = c(0, 500))
	abline(v = sigmas[i], col="blue")
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

#### BHD
library(MASS)
data(Boston)
X = Boston
y = X$medv
X$medv = NULL

Xtrain = X[1 : (nrow(X) / 2), ]
ytrain = y[1 : (nrow(X) / 2)]
Xtest = X[(nrow(X) / 2 + 1) : nrow(X), ]
ytest = y[(nrow(X) / 2 + 1) : nrow(X)]


bart_machine = build_bart_machine(Xtrain, ytrain)
bart_machine

hbart_machine = build_bart_machine(Xtrain, ytrain, use_heteroskedastic_linear_model = TRUE, hyper_sigma_weights = rep(1e9, ncol(X)))
hbart_machine

bart_oosrmse = bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse
hbart_oosrmse = bart_predict_for_test_data(hbart_machine, Xtest, ytest)$rmse
bart_oosrmse
hbart_oosrmse
#how much better does it do?
(bart_oosrmse - hbart_oosrmse) / bart_oosrmse * 100
