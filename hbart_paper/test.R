n = 100
x1 = rep(1,times = n)
x2 = runif(n, 0, 400)
x3 = runif(n, 10, 23)
x4 = runif(n, 0, 10)
X = cbind(x1,x2,x3)
Z = cbind(x1,x2,x4)

beta_vec = c(-35, .35, -1.7)
gamma_vec = c(-5, .02, -.4)
true_sigsq = exp(Z %*% gamma_vec)
dim(diag(length(true_sigsq)))
y = as.numeric(X %*% beta_vec + rnorm(n, mean = 0, sd = sqrt(true_sigsq)))
plot(X[, 3], y)
plot(X[, 2], y)

X = data.frame(cbind(X[, 2 : 3], x4))
write.csv(cbind(X, y), "hbart.csv")

library(bartMachine)
bart_machine = build_bart_machine(X, y)
bart_machine
check_bart_error_assumptions(bart_machine)

hbart_machine = build_bart_machine(X, y, use_heteroskedastic_linear_model = TRUE, debug_log = T)
hbart_machine
