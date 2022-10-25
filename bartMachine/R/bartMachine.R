bartMachine = function(X = NULL, y = NULL, Xy = NULL, 
    num_trees = 50, #found many times to not get better after this value... so let it be the default, it's faster too 
    num_burn_in = 250, 
    num_iterations_after_burn_in = 1000, 
    alpha = 0.95,
    beta = 2,
    k = 2,
    q = 0.9,
    nu = 3.0,
    prob_rule_class = 0.5,
    mh_prob_steps = c(2.5, 2.5, 4) / 9, #only the first two matter
    debug_log = FALSE,
    run_in_sample = TRUE,
    s_sq_y = "mse", # "mse" or "var"
	sig_sq_est = NULL,
    print_tree_illustrations = FALSE, #POWER USERS ONLY
    cov_prior_vec = NULL,
	interaction_constraints = NULL,
    use_missing_data = FALSE,
    covariates_to_permute = NULL, #PRIVATE
    num_rand_samps_in_library = 10000, #give the user the option to make a bigger library of random samples of normals and inv-gammas
    use_missing_data_dummies_as_covars = FALSE,
    replace_missing_data_with_x_j_bar = FALSE,
    impute_missingness_with_rf_impute = FALSE,
    impute_missingness_with_x_j_bar_for_lm = TRUE,
    mem_cache_for_speed = TRUE,
	flush_indices_to_save_RAM = TRUE,
	serialize = FALSE,
	seed = NULL,
    verbose = TRUE){
 		do.call(build_bart_machine, as.list(match.call())[-1])
}


bartMachineCV = function(X = NULL, y = NULL, Xy = NULL, 
   num_tree_cvs = c(50, 200),
   k_cvs = c(2, 3, 5),
   nu_q_cvs = NULL,
   k_folds = 5, verbose = FALSE, ...){
  
  	build_bart_machine_cv(X, y, Xy, 
	   num_tree_cvs,
	   k_cvs,
	   nu_q_cvs,
	   k_folds, ...)
}
