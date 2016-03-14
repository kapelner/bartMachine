options(java.parameters = "-Xmx2500m")
library(bartMachine)
library(MASS)

data(Pima.te)
X = data.frame(Pima.te[, -8])
y = Pima.te[, 8]

set_bart_machine_num_cores(4)

bart_machine_cv = bartMachineCV(X, y)
bart_machine_cv

bart_machine = bartMachine(X, y, prob_rule_class = 0.3)
bart_machine

oos_stats = k_fold_cv(X, y, k_folds = 10)
oos_stats$confusion_matrix

round(predict(bart_machine_cv, X[1 : 2, ], type = "prob"), 3)
predict(bart_machine_cv, X[1 : 2, ], type = "class")

# Figure 11
cov_importance_test(bart_machine_cv, covariates = c("age"))

# Figure 12
pd_plot(bart_machine_cv, j = "glu")

round(calc_credible_intervals(bart_machine_cv, X[1 : 2, ]), 3)
