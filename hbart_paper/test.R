n = 1000

gamma = 7
beta = 100

X = runif(n)
sigsq = exp(X * gamma)
y = beta * X + rnorm(n, 0, sqrt(sigsq))
plot(X, y)

write.csv(cbind(X, y), "r_hbart.csv")

library(bartMachine)
set_bart_nu
set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(5000)

bart_machine = build_bart_machine(data.frame(X), y)
bart_machine
plot_y_vs_yhat(bart_machine)


windows()
plot(bart_machine$y_hat, y)
plot(X, bart_machine$y_hat)

#check_bart_error_assumptions(bart_machine)

hbart_machine = build_bart_machine(data.frame(X), y, use_heteroskedastic_linear_model = TRUE)
hbart_machine
plot_y_vs_yhat(hbart_machine)

plot(X, bart_machine$y_hat)
points(X, hbart_machine$y_hat, col = "green")

#oos testing
Xtest = runif(n)
sigsq_test = exp(Xtest * gamma)
ytest = beta * Xtest + rnorm(n, 0, sqrt(sigsq_test))

bart_predict_for_test_data(bart_machine, data.frame(Xtest), ytest)
bart_predict_for_test_data(hbart_machine, data.frame(Xtest), ytest)