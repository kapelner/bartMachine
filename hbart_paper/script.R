library(bartMachine)
library(bartMachine, lib.loc = "C:/Program Files/R/R-3.0.2/library/")
setwd("C:/Users/jbleich/Dropbox/BART_hetero/working_paper")
##Univariate Simulation 1

n = 250
gamma = 7
beta = 100

X = seq(0, 1, length.out = n)
sigsqs = exp(X * gamma)
sigmas = sqrt(sigsqs)
y = beta * X + rnorm(n, 0, sigmas)
max(y) - min(y)

#write.csv(cbind(X, y), "r_hbart.csv")


graphics.off()
set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)
plot(X, y)
bart_machine = build_bart_machine(Xy = cbind(X, y), num_burn_in=1000)
bart_machine

heteroskedasticity_test(bart_machine = bart_machine) #pval = 0

hbart_machine = build_bart_machine(Xy = cbind(X, y), num_burn_in=500, use_heteroskedastic_linear_model = TRUE, Z_heteroskedastic_model=as.matrix(X))
hbart_machine
windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))

plot(X, bart_machine$y_hat, col = "red", pch = 16, xlab = "x", ylab = "y", ylim = c(-20, 150))
abline(0,100)
points(X, hbart_machine$y_hat, col = "blue", pch = 16)



windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))
plot(X, y, pch = "+", ylim = c(-20, 150), xlab = "x", ylab = "y", cex = .75)
abline(0,100)
pred_ints = calc_prediction_intervals(bart_machine, data.frame(X), pi_conf = 0.90)
lines(X, pred_ints[, 1], col = "red", lty = 1, lwd = 2)
lines(X, pred_ints[, 2], col = "red", lty = 1,lwd = 2)

pred_ints_h = calc_prediction_intervals(hbart_machine, data.frame(X), Z_new_data = as.matrix(X), pi_conf = 0.90)
lines(X, pred_ints_h[, 1], col = "blue", lty = 1, lwd = 2)
lines(X, pred_ints_h[, 2], col = "blue", lty = 1, lwd = 2)


#graphics.off()


##
#homo univariate
n =250
gamma = 0
beta = 100

X = seq(0, 1, length.out = n)
sigsqs = 9*exp(X * gamma)
sigmas = sqrt(sigsqs)
y = beta * X + rnorm(n, 0, sigmas)
max(y) - min(y)

#write.csv(cbind(X, y), "r_hbart.csv")

#graphics.off()
set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)
#plot(X, y)
bart_machine = build_bart_machine(Xy = cbind(X, y), num_burn_in=1000)
bart_machine

hbart_machine = build_bart_machine(Xy = cbind(X, y), num_burn_in=500, use_heteroskedastic_linear_model = TRUE, Z_heteroskedastic_model=as.matrix(X))
hbart_machine
windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))

plot(X, bart_machine$y_hat, col = "red", pch = 16, xlab = "x", ylab = "y", ylim = c(-20, 150))
abline(0,100)
points(X, hbart_machine$y_hat, col = "blue", pch = 16)



windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))
plot(X, y, pch = "+", ylim = c(-20, 150), xlab = "x", ylab = "y", cex = .75)
abline(0,100)
pred_ints = calc_prediction_intervals(bart_machine, data.frame(X), pi_conf = 0.90)
lines(X, pred_ints[, 1], col = "red", lty = 1, lwd = 2)
lines(X, pred_ints[, 2], col = "red", lty = 1,lwd = 2)

pred_ints_h = calc_prediction_intervals(hbart_machine, data.frame(X), Z_new_data = as.matrix(X), pi_conf = 0.90)
lines(X, pred_ints_h[, 1], col = "blue", lty = 1, lwd = 2)
lines(X, pred_ints_h[, 2], col = "blue", lty = 1, lwd = 2)


graphics.off()



####,multivariate example
##credible intervals


n = 500
x1 = rep(1,times = n)
x2 = runif(n, 0, 400)
x3 = runif(n, 10, 23)
x4 = runif(n, 0, 10)
X = cbind(x1,x2,x3,x4)

beta_vec = c(-35, .35, -1.7, 0)
#gamma_vec = c(-8, .026, 0, .4)
gamma_vec = c(-7, .03, 0, .4)
sigsqs = exp(X %*% gamma_vec)
sigmas = sqrt(sigsqs)

y = X %*% beta_vec + rnorm(n, mean = 0, sd = sigmas)

bart_machine = build_bart_machine(Xy = data.frame(X[, -1], y))
heteroskedasticity_test(bart_machine = bart_machine) #pval = 0

hbart_machine = build_bart_machine(Xy = data.frame(X[, -1], y), run_in_sample = F,
                                   use_heteroskedastic_linear_model = TRUE, 
                                   Z_heteroskedastic_model=as.matrix(X[,-1]))
hbart_machine

#CIs
n_test = 500
x1_test = rep(1,times = n_test)
x2_test = runif(n_test, 0, 400)
x3_test = runif(n_test, 10, 23)
x4_test = runif(n_test, 0, 10)
X_test = cbind(x1_test, x2_test, x3_test, x4_test)
colnames(X_test) = colnames(X)

beta_vec = c(-35, .35, -1.7, 0)
#gamma_vec = c(-8, .026, 0, .4)
gamma_vec = c(-7, .03, 0, .4)
sigsqs_test = exp(X_test %*% gamma_vec)
sigmas_test = sqrt(sigsqs_test)

exp_y_given_X_test = X_test %*% beta_vec 


windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))
plot_y_vs_yhat(hbart_machine, Xtest = as.data.frame(X_test[,-1]), ytest = exp_y_given_X_test, cred = T,print_main = F)
windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))
plot_y_vs_yhat(bart_machine, Xtest = as.data.frame(X_test[,-1]), ytest = exp_y_given_X_test, cred = T, print_main = F)
graphics.off()


###For box plots - hetero

nsim = 100
bart_rmse_vec = numeric(100)
hbart_rmse_vec =numeric(100)

for(i in 1 : nsim){
  beta_vec = c(-35, .35, -1.7, 0)
  #gamma_vec = c(-8, .026, 0, .4)
  gamma_vec = c(-7, .03, 0, .4)
  
  n = 500
  x1 = rep(1,times = n)
  x2 = runif(n, 0, 400)
  x3 = runif(n, 10, 23)
  x4 = runif(n, 0, 10)
  X = cbind(x1,x2,x3,x4)
  
  sigsqs = exp(X %*% gamma_vec)
  sigmas = sqrt(sigsqs)
  
  y = X %*% beta_vec + rnorm(n, mean = 0, sd = sigmas)
  
  bart_machine = build_bart_machine(Xy = cbind(X[, -1], y), run_in_sample = F, verbose = F)
  hbart_machine = build_bart_machine(Xy = cbind(X[, -1], y), run_in_sample = F, verbose =F,
                                    use_heteroskedastic_linear_model = TRUE, Z_heteroskedastic_model=as.matrix(X[,-1]))
  
  
  n_test = 500
  x1_test = rep(1,times = n_test)
  x2_test = runif(n_test, 0, 400)
  x3_test = runif(n_test, 10, 23)
  x4_test = runif(n_test, 0, 10)
  X_test = cbind(x1_test, x2_test, x3_test, x4_test)
  
  sigsqs_test = exp(X_test %*% gamma_vec)
  sigmas_test = sqrt(sigsqs_test)
  
  y_test = X_test %*% beta_vec + rnorm(n_test, mean = 0, sd = sigmas_test)
  
  bart_rmse_vec[i] = bart_predict_for_test_data(bart_machine, Xtest[ ,-1], ytest)$rmse
  hbart_rmse_vec[i] = bart_predict_for_test_data(hbart_machine, Xtest[ ,-1], ytest)$rmse
  
  
  destroy_bart_machine(bart_machine)
  destroy_bart_machine(hbart_machine)
  if(i %% 10 == 0) print(i)
  
}



##box plots for homosked case

nsim = 100
bart_rmse_vec_homo = numeric(100)
hbart_rmse_vec_homo =numeric(100)

for(i in 1 : nsim){
  beta_vec = c(-35, .35, -1.7, 0)
  #gamma_vec = c(-8, .026, 0, .4)
  #gamma_vec = c(-6, .03, 0, .4)
  
  n = 500
  x1 = rep(1,times = n)
  x2 = runif(n, 0, 400)
  x3 = runif(n, 10, 23)
  x4 = runif(n, 0, 10)
  X = cbind(x1,x2,x3,x4)
  
  beta_vec = c(-35, .35, -1.7, 0)
  #gamma_vec = c(-8, .026, 0, .4)
  gamma_vec = c(-6, .03, 0, .4)
  sigsqs = 9
  sigmas = sqrt(sigsqs)
  
  y = X %*% beta_vec + rnorm(n, mean = 0, sd = sigmas)
  
  bart_machine = build_bart_machine(Xy = cbind(X[, -1], y), run_in_sample = F, verbose = F)
  hbart_machine = build_bart_machine(Xy = cbind(X[, -1], y), run_in_sample = F, verbose =F,
                                     use_heteroskedastic_linear_model = TRUE, Z_heteroskedastic_model=as.matrix(X[,-1]))
  
  
  n_test = 500
  x1_test = rep(1,times = n_test)
  x2_test = runif(n_test, 0, 400)
  x3_test = runif(n_test, 10, 23)
  x4_test = runif(n_test, 0, 10)
  X_test = cbind(x1_test, x2_test, x3_test, x4_test)
  
  beta_vec = c(-35, .35, -1.7, 0)
  #gamma_vec = c(-8, .026, 0, .4)
 # gamma_vec = c(-6, .03, 0, .4)
  sigsqs_test = 9
  sigmas_test = sqrt(sigsqs_test)
  
  y_test = X_test %*% beta_vec + rnorm(n_test, mean = 0, sd = sigmas_test)
  
  bart_rmse_vec_homo[i] = bart_predict_for_test_data(bart_machine, Xtest[ ,-1], ytest)$rmse
  hbart_rmse_vec_homo[i] = bart_predict_for_test_data(hbart_machine, Xtest[ ,-1], ytest)$rmse
  
  
  destroy_bart_machine(bart_machine)
  destroy_bart_machine(hbart_machine)
  if(i %% 10 == 0) print(i)
  
}





#hetero_box = list(hbart = hbart_rmse_vec, bart = bart_rmse_vec)
#save(hetero_box, file = "hetero_rmse_vecs.Rdata")
#homo_box = list(hbart = hbart_rmse_vec_homo, bart = bart_rmse_vec_homo)
#save(homo_box, file = "homo_rmse_vecs.Rdata")
load("hetero_rmse_vecs.Rdata")
load("homo_rmse_vecs.Rdata")

windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))
boxplot(hetero_box$bart, hetero_box$hbart, names = c("BART", "HBART"), ylab = "Root Mean Square Error")
windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))
boxplot(homo_box$bart, homo_box$hbart, names = c("BART", "HBART"), ylab = "Root Mean Square Error")
graphics.off()

#######################
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
heteroskedasticity_test(bart_machine = bart_machine) #pval = 0

pred_ints = calc_prediction_intervals(bart_machine, X, pi_conf = 0.90)

hbart_machine = build_bart_machine(X, y, 
                                   use_heteroskedastic_linear_model = TRUE,
                                   Z_heteroskedastic_model = Z)
hbart_machine
h_pred_ints = calc_prediction_intervals(hbart_machine, X, Z_new_data = Z, pi_conf = 0.90)

moto.btgpllm <- btgpllm(X=X, Z=y, bprior="b0", verb=0)
moto.btgpllm.p <- predict(moto.btgpllm) 
moto.btgpllm.p$Zp.q1
moto.btgpllm.p$Zp.q2

dtree = dynaTree(X,y)
dpreds = predict(dtree,X)

#plot
lwd_set = 2
lty_set = 1

windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))
plot(X[, 1], y, pch = 16, ylim = c(-160, 110), xlab = "Time", ylab = "Acceleration")

lines(X[, 1], pred_ints[, 1], col = "red", lty = lty_set, lwd = lwd_set)
lines(X[, 1], pred_ints[, 2], col = "red", lty = lty_set, lwd = lwd_set)

lines(X[, 1], h_pred_ints[, 1], col = "blue", lty = lty_set, lwd = lwd_set)
lines(X[, 1], h_pred_ints[, 2], col = "blue", lty = lty_set, lwd = lwd_set)

#lines(X[, 1], dpreds$mean, col = "purple", lty = 1)
lines(X[, 1], dpreds$q1, col = "forestgreen", lty = lty_set , lwd = lwd_set)
lines(X[, 1], dpreds$q2, col = "forestgreen", lty = lty_set, lwd = lwd_set)

#lines(X[, 1], moto.btgpllm$Zp.mean, col = "red", lty = 1)
lines(X[, 1], moto.btgpllm$Zp.q1, col = "goldenrod1", lty = lty_set, lwd = lwd_set)
lines(X[, 1], moto.btgpllm$Zp.q2, col = "goldenrod1", lty = lty_set, lwd = lwd_set)

legend("topleft", legend=c("BART", "HBART", "dynaTree", "TGP"), 
       col=c("red", "blue", "forestgreen", "goldenrod1"), lty = lty_set, lwd = lwd_set)
graphics.off()

###
library(SemiPar)
data(lidar)
MAX_POLY = 2
mod = lm(lidar$logratio ~ poly(lidar$range, MAX_POLY))
Z = as.matrix(mod$model)[, 2 : (MAX_POLY + 1)]


bart_machine = build_bart_machine(data.frame(lidar$range), lidar$logratio, num_burn_in = 1500)
bart_machine
heteroskedasticity_test(bart_machine = bart_machine) #pval = 0

pred_ints = calc_prediction_intervals(bart_machine, data.frame(lidar$range), pi_conf = 0.90)

hbart_machine = build_bart_machine(data.frame(lidar$range), lidar$logratio, 
                                   use_heteroskedastic_linear_model = TRUE,
                                   Z_heteroskedastic_model = Z)
pred_ints_h = calc_prediction_intervals(hbart_machine, data.frame(lidar$range), Z_new_data = Z, pi_conf = 0.90)

lwd_set = 2
lty_set = 1

windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))
plot(lidar$range, bart_machine$y_hat, col = "red", ylim = c(-1,.2), type = "l", lwd = 2, ylab = "Log(Ratio)", xlab = "Range")
points(lidar$range, lidar$logratio, pch = 16)
points(lidar$range, hbart_machine$y_hat, col = "blue", type = "l", lwd = 2)

windows()
par(mgp=c(1.8, .5, 0), mar=c(3.5, 3.5 ,.4, 1))
plot(lidar$range, lidar$logratio, pch = 16, ylim = c(-1,.2), ylab = "Log(Ratio)", xlab = "Range")
lines(lidar$range, pred_ints[, 1], col = "red", lty = lty_set, lwd = lwd_set )
lines(lidar$range, pred_ints[, 2], col = "red", lty = lty_set, lwd = lwd_set)

lines(lidar$range, pred_ints_h[, 1], col = "blue", lty = lty_set, lwd = lwd_set)
lines(lidar$range, pred_ints_h[, 2], col = "blue", lty = lty_set, lwd = lwd_set)

graphics.off()


###
#sniffer data
###
library(bartMachine)
library(alr3)
data(sniffer)

bart_machine = build_bart_machine(Xy = sniffer, num_burn_in = 1500)
bart_machine
plot_y_vs_yhat(bart_machine, credible_intervals = TRUE)
heteroskedasticity_test(bart_machine = bart_machine) #pval = 0

#now build a model with the log squared residuals and see if there's any marginal linear patterns
log_sq_resid_bart_machine = build_bart_machine(X = sniffer[, 1 : 4], y = log(bart_machine$residuals^2), num_burn_in = 1500)
plot(sniffer[, 1], log(bart_machine$residuals^2))
plot(sniffer[, 2], log(bart_machine$residuals^2))
plot(sniffer[, 3], log(bart_machine$residuals^2))
plot(sniffer[, 4], log(bart_machine$residuals^2))
pd_plot(log_sq_resid_bart_machine, j = "TankTemp")
windows()
pd_plot(log_sq_resid_bart_machine, j = "GasTemp")
windows()
pd_plot(log_sq_resid_bart_machine, j = "TankPres")
windows()
pd_plot(log_sq_resid_bart_machine, j = "GasPres")

#we note that none of the PDP's show any significant pattern (assuming the BART model is correct) except GasTemp where it appears to go up and come down
#let's try a polynomial model of order 2 on it
MAX_POLY = 2
mod = lm(sniffer[,5] ~ poly(sniffer$GasTemp, MAX_POLY))
Z = as.matrix(mod$model)[, 2 : (MAX_POLY + 1)]

hbart_machine = build_bart_machine(Xy = sniffer, use_heteroskedastic_linear_model = TRUE, Z_heteroskedastic_model = Z)
hbart_machine

k_fold_cv(sniffer[, 1 : 4], sniffer[, 5])
#$rmse
#[1] 3.989569
k_fold_cv(sniffer[, 1 : 4], sniffer[, 5], use_heteroskedastic_linear_model = TRUE, Z_heteroskedastic_model = Z)
