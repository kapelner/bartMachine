library(bartMachine)
library(MASS)

data(Pima.te)
X = Pima.te[, -8]
y = Pima.te[, 8]

set_bart_machine_num_cores(4)
init_java_for_bart_machine_with_mem_in_mb(2500)

bart_machine_cv = build_bart_machine_cv(X, y)
bart_machine_cv

bart_machine = build_bart_machine(X, y, prob_rule_class = 0.3)
bart_machine

oos_stats = k_fold_cv(X, y, k_folds = 10)
oos_stats$confusion_matrix

predict(bart_machine_cv, X[1 : 2, ], type = "prob")
predict(bart_machine_cv, X[1 : 2, ], type = "class")

# Figure 12
cov_importance_test(bart_machine_cv, covariates = c("age"))

# Figure 13
pd_plot(bart_machine_cv, j = "glu")

calc_credible_intervals(bart_machine_cv, X[1 : 2, ])
