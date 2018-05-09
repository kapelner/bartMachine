pacman::p_load(survival, rootSolve)
mod = survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist='weibull')
mod


k_hat = 1 / summary(mod)$scale






cdf_k_at_k_hat = function(d, k_hat_weibull_model, hyper_a, hyper_b, d_max){
  
  kernel_k = function(k){
    (hyper_b * k)^(hyper_a) * exp(-hyper_b * k) * (hyper_b + d^k)^(-hyper_a)
  }
  
  
  numer = integrate(kernel_k, 0, k_hat_weibull_model)$value
  denom = integrate(kernel_k, 0, d_max)$value
  
  repeat {
    # cat("cdf_k_at_k_hat k_hat_weibull_model", k_hat_weibull_model, "d_max", d_max, "numer", numer, "denom", denom, "\n")
    if (numer > denom){ #i.e. impossible... there must be some numeric underflow
      d_max = d_max * 0.9
      denom = integrate(kernel_k, 0, d_max)$value
    } else {
      break
    }
  }
  numer / denom
}

inverse_cdf_k_one_minus_q_target = function(d, hyper_q, hyper_a, hyper_b, k_hat_weibull_model, d_max){
  obj_val = abs(
    cdf_k_at_k_hat(d, k_hat_weibull_model, hyper_a, hyper_b, d_max) - 
      (1 - hyper_q)
  )
  cat("d", d, "obj_val", obj_val, "\n")
  obj_val
}


optim_obj = optim(
  par = 1, 
  fn = inverse_cdf_k_one_minus_q_target, 
  hyper_q = 0.4,
  hyper_a = 3,
  hyper_b = 1,
  d_max = 1000,
  k_hat_weibull_model = 1.13,
  lower = 0.01, 
  upper = 2, 
  method = "Brent",
  control = list(maxit = 1000)
)
optim_obj$par


ds = seq(0, 3, by = 0.01)
inverse_cdf_ds = array(NA, length(ds))
for (i in 1 : length(ds)){
  inverse_cdf_ds[i] = inverse_cdf_k_one_minus_q_target(ds[i])
}
plot(ds, inverse_cdf_ds)
abline(h = 0)






uniroot.all(inverse_cdf_k_one_minus_q, c(0,10))

############

kernel_k = function(k){
  (b * k)^(a) * exp(-b * k) * (b + d^k)^(-a)
}


ks = seq(0, 100, by = 0.01)
kernel_ks = array(NA, length(ks))
for (i in 1 : length(ks)){
  kernel_ks[i] = kernel_k(ks[i])
}
plot(ks, kernel_ks)


