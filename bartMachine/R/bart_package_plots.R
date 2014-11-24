
##check BART error assumptions via plot
check_bart_error_assumptions = function(bart_machine, hetero_plot = "yhats"){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (!(hetero_plot %in% c("ys", "yhats"))){
		stop("You must specify the parameter \"hetero_plot\" as \"ys\" or \"yhats\"")
	}
	if (bart_machine$pred_type == "classification"){
		stop("There are no convergence diagnostics for classification.")
	}	
	graphics.off()
	par(mfrow = c(2, 1))
	es = bart_machine$residuals
	y_hat = bart_machine$y_hat
	
	#plot/test for normality
	normal_p_val = shapiro.test(es)$p.value
	qqp(es, col = "blue",
			main = paste("Assessment of Normality\n", "p-val for shapiro-wilk test of normality of residuals:", round(normal_p_val, 3)),
			xlab = "Normal Q-Q plot for in-sample residuals\n(Theoretical Quantiles)")	
	
	#plot for heteroskedasticity
	if (hetero_plot == "yhats"){
		plot(y_hat, es, main = paste("Assessment of Heteroskedasticity\nFitted vs residuals"), xlab = "Fitted Values", ylab = "Residuals", col = "blue")
	} else if (hetero_plot == "ys") {
		plot(bart_machine$y, es, main = paste("Assessment of Heteroskedasticity\nFitted vs residuals"), xlab = "Actual Values", ylab = "Residuals", col = "blue")
	}
	
	abline(h = 0, col = "black")
	par(mfrow = c(1, 1))
}

##private function for plotting tree depths
plot_tree_depths = function(bart_machine){
	
	tree_depths_after_burn_in = get_tree_depths(bart_machine)
	
	num_after_burn_in_per_core = nrow(tree_depths_after_burn_in)
	
	plot(1 : num_after_burn_in_per_core, rep(0, num_after_burn_in_per_core), type = "n", 
		main = "Tree Depth by MCMC Iteration After Burn-in", xlab = "MCMC Iteration", 
		ylab = paste("Tree Depth for all cores"), ylim = c(0, max(tree_depths_after_burn_in)))
	#plot burn in
	for (t in 1 : ncol(tree_depths_after_burn_in)){
		lines(1 : num_after_burn_in_per_core, tree_depths_after_burn_in[, t], col = rgb(0.9,0.9,0.9))
	}
	lines(1 : num_after_burn_in_per_core, apply(tree_depths_after_burn_in, 1, mean), col = "blue", lwd = 4)
	lines(1 : num_after_burn_in_per_core, apply(tree_depths_after_burn_in, 1, min), col = "black")
	lines(1 : num_after_burn_in_per_core, apply(tree_depths_after_burn_in, 1, max), col = "black")
	
	
	if (bart_machine$num_cores > 1){
		for (c in 2 : bart_machine$num_cores){
			abline(v = (c - 1) * bart_machine$num_iterations_after_burn_in / bart_machine$num_cores, col = "gray")
		}		
	}		
}

#private function for getting tree depths to plot
get_tree_depths = function(bart_machine){
	tree_depths_after_burn_in = NULL
	for (c in 1 : bart_machine$num_cores){
		tree_depths_after_burn_in_core = t(sapply(.jcall(bart_machine$java_bart_machine, "[[I", "getDepthsForTreesInGibbsSampAfterBurnIn", as.integer(c)), .jevalArray))
		tree_depths_after_burn_in = rbind(tree_depths_after_burn_in, tree_depths_after_burn_in_core)
	}
	tree_depths_after_burn_in
}

#private function for plotting number of nodes in the trees
plot_tree_num_nodes = function(bart_machine){
	
	tree_num_nodes_and_leaves_after_burn_in = get_tree_num_nodes_and_leaves(bart_machine)
	
	num_after_burn_in_per_core = nrow(tree_num_nodes_and_leaves_after_burn_in)
	
	plot(1 : num_after_burn_in_per_core, rep(0, num_after_burn_in_per_core), type = "n", 
		main = "Tree Num Nodes And Leaves by\nMCMC Iteration After Burn-in", xlab = "MCMC Iteration", 
		ylab = paste("Tree Num Nodes and Leaves for all cores"), 
		ylim = c(0, max(tree_num_nodes_and_leaves_after_burn_in)))
	#plot burn in
	for (t in 1 : ncol(tree_num_nodes_and_leaves_after_burn_in)){
		lines(1 : num_after_burn_in_per_core, tree_num_nodes_and_leaves_after_burn_in[, t], col = rgb(0.9, 0.9, 0.9))
	}
	lines(1 : num_after_burn_in_per_core, apply(tree_num_nodes_and_leaves_after_burn_in, 1, mean), col = "blue", lwd = 4)
	lines(1 : num_after_burn_in_per_core, apply(tree_num_nodes_and_leaves_after_burn_in, 1, min), col = "black")
	lines(1 : num_after_burn_in_per_core, apply(tree_num_nodes_and_leaves_after_burn_in, 1, max), col = "black")
	
	if (bart_machine$num_cores > 1){
		for (c in 2 : bart_machine$num_cores){
			abline(v = (c - 1) * bart_machine$num_iterations_after_burn_in / bart_machine$num_cores, col = "gray")
		}		
	}	
}
##private function for getting the number of nodes in the trees
get_tree_num_nodes_and_leaves = function(bart_machine){
	tree_num_nodes_and_leaves_after_burn_in = NULL
	for (c in 1 : bart_machine$num_cores){
		tree_num_nodes_and_leaves_after_burn_in_core = t(sapply(.jcall(bart_machine$java_bart_machine, "[[I", "getNumNodesAndLeavesForTreesInGibbsSampAfterBurnIn", as.integer(c)), .jevalArray))
		tree_num_nodes_and_leaves_after_burn_in = rbind(tree_num_nodes_and_leaves_after_burn_in, tree_num_nodes_and_leaves_after_burn_in_core)
	}
	tree_num_nodes_and_leaves_after_burn_in
}

#private function for plotting the MH acceptance proportions by core
plot_mh_acceptance_reject = function(bart_machine){
	
	mh_acceptance_reject = get_mh_acceptance_reject(bart_machine)
	a_r_before_burn_in = mh_acceptance_reject[["a_r_before_burn_in"]]
	a_r_before_burn_in_avg_over_trees = rowSums(a_r_before_burn_in) / bart_machine$num_trees
	
	a_r_after_burn_in_avgs_over_trees = list()
	for (c in 1 : bart_machine$num_cores){
		a_r_after_burn_in = mh_acceptance_reject[["a_r_after_burn_in"]][[c]]
		a_r_after_burn_in_avgs_over_trees[[c]] = rowSums(a_r_after_burn_in) / bart_machine$num_trees		
	}	
	
	num_after_burn_in_per_core = length(a_r_after_burn_in_avgs_over_trees[[1]])
	num_gibbs_per_core = bart_machine$num_burn_in + num_after_burn_in_per_core
	
	
	plot(1 : num_gibbs_per_core, rep(0, num_gibbs_per_core), ylim = c(0, 1), type = "n", 
			main = "Percent Acceptance by MCMC Iteration", xlab = "MCMC Iteration", ylab = "% of Trees Accepting")
	abline(v = bart_machine$num_burn_in, col = "grey")
	#plot burn in
	points(1 : bart_machine$num_burn_in, a_r_before_burn_in_avg_over_trees, col = "grey")
	tryCatch(lines(loess.smooth(1 : bart_machine$num_burn_in, a_r_before_burn_in_avg_over_trees), col = "black", lwd = 4), error = function(e){e})
	
	for (c in 1 : bart_machine$num_cores){
		points((bart_machine$num_burn_in + 1) : num_gibbs_per_core, a_r_after_burn_in_avgs_over_trees[[c]], col = COLORS[c])
		tryCatch(lines(loess.smooth((bart_machine$num_burn_in + 1) : num_gibbs_per_core, a_r_after_burn_in_avgs_over_trees[[c]]), col = COLORS[c], lwd = 4), error = function(e){e})
	}	
	
}

##private function for getting the MH acceptance proportions by core
get_mh_acceptance_reject = function(bart_machine){
	a_r_before_burn_in = t(sapply(.jcall(bart_machine$java_bart_machine, "[[Z", "getAcceptRejectMHsBurnin"), .jevalArray)) * 1
	
	a_r_after_burn_in = list()
	for (c in 1 : bart_machine$num_cores){
		a_r_after_burn_in[[c]] = t(sapply(.jcall(bart_machine$java_bart_machine, "[[Z", "getAcceptRejectMHsAfterBurnIn", as.integer(c)), .jevalArray)) * 1
	}
	
	list(
			a_r_before_burn_in = a_r_before_burn_in,
			a_r_after_burn_in = a_r_after_burn_in	
	)
}

#plot y vs yhat for training or test data
plot_y_vs_yhat = function(bart_machine, Xtest = NULL, ytest = NULL, credible_intervals = FALSE, prediction_intervals = FALSE, interval_confidence_level = 0.95){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if( (!bart_machine$run_in_sample) & (is.null(Xtest) | is.null(ytest)) ){
		stop("To run on training data, you must set \"run_in_sample\" option to TRUE in \"build_bart_machine\"")
	}
  
	if (credible_intervals && prediction_intervals){
		stop("Cannot plot both credibility intervals and prediction intervals simultaneously.")
	}
	if (bart_machine$pred_type == "classification"){
		stop("Cannot plot y vs y_hat for classification.")
	}
  	if( (is.null(Xtest) & !is.null(ytest)) | (!is.null(Xtest) & is.null(ytest)) ){
    	stop("Must pass both X and y to use on test data")
  	}
	
	if (is.null(Xtest) & is.null(ytest)){
		Xtest = bart_machine$X
		ytest = bart_machine$y
		y_hat = bart_machine$y_hat_train
		in_sample = TRUE
	} else {
		predict_obj = bart_predict_for_test_data(bart_machine, Xtest, ytest)
		y_hat = predict_obj$y_hat
		in_sample = FALSE
	}
	
	if (credible_intervals){
		credible_intervals = calc_credible_intervals(bart_machine, Xtest, interval_confidence_level)
		ci_a = credible_intervals[, 1]
		ci_b = credible_intervals[, 2]
		y_in_ppi = ytest >= ci_a & ytest <= ci_b
		prop_ys_in_ppi = sum(y_in_ppi) / length(y_in_ppi)
		
		plot(ytest, y_hat, 
			main = paste(ifelse(in_sample, "In-Sample", "Out-of-Sample"), " Fitted vs. Actual Values\nwith ", round(interval_confidence_level * 100), "% Cred. Int.'s (", round(prop_ys_in_ppi * 100, 2), "% coverage)", sep = ""), 
			xlab = paste("Actual Values", sep = ""), 
			ylab = "Fitted Values", 
			xlim = c(min(min(ytest), min(y_hat)), max(max(ytest), max(y_hat))),
			ylim = c(min(min(ytest), min(y_hat)), max(max(ytest), max(y_hat))),
			cex = 0)
		#draw PPI's
		for (i in 1 : bart_machine$n){
			segments(ytest[i], ci_a[i], ytest[i], ci_b[i], col = "grey", lwd = 0.1)	
		}
		#draw green dots or red dots depending upon inclusion in the PPI
		for (i in 1 : bart_machine$n){
			points(ytest[i], y_hat[i], col = ifelse(y_in_ppi[i], "darkgreen", "red"), cex = 0.6, pch = 16)	
		}		
	} else if (prediction_intervals){
		credible_intervals = calc_prediction_intervals(bart_machine, Xtest, interval_confidence_level)
		ci_a = credible_intervals[, 1]
		ci_b = credible_intervals[, 2]
		y_in_ppi = ytest >= ci_a & ytest <= ci_b
		prop_ys_in_ppi = sum(y_in_ppi) / length(y_in_ppi)
		
		plot(ytest, y_hat, 
				main = paste(ifelse(in_sample, "In-Sample", "Out-of-Sample"), " Fitted vs. Actual Values\nwith ", round(interval_confidence_level * 100), "% Pred. Int.'s (", round(prop_ys_in_ppi * 100, 2), "% coverage)", sep = ""), 
				xlab = paste("Actual Values", sep = ""), 
				ylab = "Fitted Values", 
				xlim = c(min(min(ytest), min(y_hat)), max(max(ytest), max(y_hat))),
				ylim = c(min(min(ytest), min(y_hat)), max(max(ytest), max(y_hat))),
				cex = 0)
		#draw PPI's
		for (i in 1 : bart_machine$n){
			segments(ytest[i], ci_a[i], ytest[i], ci_b[i], col = "grey", lwd = 0.1)	
		}
		#draw green dots or red dots depending upon inclusion in the PPI
		for (i in 1 : bart_machine$n){
			points(ytest[i], y_hat[i], col = ifelse(y_in_ppi[i], "darkgreen", "red"), cex = 0.6, pch = 16)	
		}		
	} else {
		plot(ytest, y_hat, 
			main = "Fitted vs. Actual Values", 
			xlab = "Actual Values", 
			ylab = "Fitted Values", 
			col = "blue", 
			xlim = c(min(min(ytest), min(y_hat)), max(max(ytest), max(y_hat))),
			ylim = c(min(min(ytest), min(y_hat)), max(max(ytest), max(y_hat))),)
	}
	abline(a = 0, b = 1, lty = 2)	
}

##get sigsqs and plot a histogram, if desired
get_sigsqs = function(bart_machine, after_burn_in = T, plot_hist = F, plot_CI = .95, plot_sigma = F){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (bart_machine$pred_type == "classification"){
		stop("There are no sigsq's for classification.")
	}
	
	sigsqs = .jcall(bart_machine$java_bart_machine, "[D", "getGibbsSamplesSigsqs")
	
	num_iterations_after_burn_in = bart_machine$num_iterations_after_burn_in
	num_burn_in = bart_machine$num_burn_in
	num_gibbs = bart_machine$num_gibbs
	num_trees = bart_machine$num_trees
	
	sigsqs_after_burnin = tail(sigsqs, num_iterations_after_burn_in)
	avg_sigsqs = mean(sigsqs_after_burnin, na.rm = TRUE)
	
	if(plot_hist){
    if(plot_sigma){
      var_est_to_plot = sqrt(sigsqs_after_burnin)
    }
    else{
      var_est_to_plot = sigsqs_after_burnin
    }

	  ppi_a = quantile(var_est_to_plot, (.5 - plot_CI/2))
	  ppi_b = quantile(var_est_to_plot, (.5 + plot_CI/2))
	  hist(var_est_to_plot, 
	       br = 100, 
	       main = paste("Histogram of ", ifelse(plot_sigma ==T, "Sigmas", "Sigma^2s"),  " After Burn-in", sep = ""), 
	       xlab = paste("Avg = ", round(mean(var_est_to_plot, na.rm = T), 2), ", " , 100*plot_CI, "% Credible Interval = [", round(ppi_a, 2), ", ", round(ppi_b, 2), "]", sep = ""))
	  abline(v = mean(var_est_to_plot, na.rm = T), col = "blue")
	  abline(v = ppi_a, col = "red")
	  abline(v = ppi_b, col = "red")
	}
  
	if(after_burn_in == T){
    return(sigsqs_after_burnin)
	}else{
    return(sigsqs)
	}
}

#private function for plotting convergence diagnostics for sigma^2
plot_sigsqs_convergence_diagnostics = function(bart_machine){	
	if (bart_machine$pred_type == "classification"){
		stop("There are no convergence diagnostics for classification.")
	}	
	
	sigsqs = get_sigsqs(bart_machine, after_burn_in = FALSE)
	
	num_iterations_after_burn_in = bart_machine$num_iterations_after_burn_in
	num_burn_in = bart_machine$num_burn_in
	num_gibbs = bart_machine$num_gibbs
	num_trees = bart_machine$num_trees
	
	#first look at sigsqs
	sigsqs_after_burnin = sigsqs[(length(sigsqs) - num_iterations_after_burn_in) : length(sigsqs)]
	avg_sigsqs_after_burn_in = mean(sigsqs_after_burnin, na.rm = TRUE)
	
	plot(sigsqs, 
		main = paste("Sigsq Estimates over MCMC Iteration"), 
		xlab = "MCMC Iteration (yellow lines: after burn-in 95% CI)", 
		ylab = paste("Sigsq by MCMC Iteration, avg after burn-in =", round(avg_sigsqs_after_burn_in, 3)),
		ylim = c(quantile(sigsqs, 0.01), quantile(sigsqs, 0.99)),
		pch = ".", 
		cex = 3,
		col = "gray")
	points(sigsqs, pch = ".", col = "red")
	ppi_sigsqs = quantile(sigsqs[num_burn_in : length(sigsqs)], c(.025, .975))
	abline(a = ppi_sigsqs[1], b = 0, col = "yellow")
	abline(a = ppi_sigsqs[2], b = 0, col = "yellow")
	abline(a = avg_sigsqs_after_burn_in, b = 0, col = "blue")
	abline(v = num_burn_in, col = "gray")
	if (bart_machine$num_cores > 1){
		for (c in 2 : bart_machine$num_cores){
			abline(v = num_burn_in + (c - 1) * bart_machine$num_iterations_after_burn_in / bart_machine$num_cores, col = "gray")
		}		
	}

}

##function for investigating variable inclusion proportions
investigate_var_importance = function(bart_machine, type = "splits", plot = TRUE, num_replicates_for_avg = 5, num_trees_bottleneck = 20, num_var_plot = Inf, bottom_margin = 10){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	var_props = array(0, c(num_replicates_for_avg, bart_machine$p))
	for (i in 1 : num_replicates_for_avg){
		if (i == 1 & num_trees_bottleneck == bart_machine$num_trees){ ##if original BART is using right number of trees
			var_props[i, ] = get_var_props_over_chain(bart_machine, type)
		} else {
			bart_machine_dup = bart_machine_duplicate(bart_machine, num_trees = num_trees_bottleneck, run_in_sample = FALSE, verbose = FALSE)			
			var_props[i, ] = get_var_props_over_chain(bart_machine_dup, type)				
		}
		cat(".")
	}
	cat("\n")
	
	avg_var_props = colMeans(var_props)
	names(avg_var_props) = bart_machine$training_data_features_with_missing_features
	sd_var_props = apply(var_props, 2, sd)
	names(sd_var_props) = bart_machine$training_data_features_with_missing_features
	
	if (num_var_plot == Inf){
		num_var_plot = bart_machine$p
	}
	
	avg_var_props_sorted_indices = sort(avg_var_props, decreasing = TRUE, index.return = TRUE)$ix
	avg_var_props = avg_var_props[avg_var_props_sorted_indices][1 : num_var_plot]
	sd_var_props = sd_var_props[avg_var_props_sorted_indices][1 : num_var_plot]		
	
	if (plot){
		par(mar = c(bottom_margin, 6, 3, 0))
		if (is.na(sd_var_props[1])){
			moe = 0
		} else {
			moe = 1.96 * sd_var_props / sqrt(num_replicates_for_avg)
		}
		bars = barplot(avg_var_props, 
			names.arg = names(avg_var_props), 
			las = 2, 
#			xlab = "Predictor",
			ylab = "Inclusion Proportion", 
#			main = paste("Important Variables Averaged over", num_replicates_for_avg, "Replicates by", ifelse(type == "splits", "Number of Variable Splits", "Number of Trees")),
			col = "gray",#rgb(0.39, 0.39, 0.59),
			ylim = c(0, max(avg_var_props + moe))
		)
		conf_upper = avg_var_props + 1.96 * sd_var_props / sqrt(num_replicates_for_avg)
		conf_lower = avg_var_props - 1.96 * sd_var_props / sqrt(num_replicates_for_avg)
		segments(bars, avg_var_props, bars, conf_upper, col = rgb(0.59, 0.39, 0.39), lwd = 3) # Draw error bars
		segments(bars, avg_var_props, bars, conf_lower, col = rgb(0.59, 0.39, 0.39), lwd = 3)
		par(mar = c(5.1, 4.1, 4.1, 2.1))
	}	
	invisible(list(avg_var_props = avg_var_props, sd_var_props = sd_var_props))	
}

##user function calling private plotting methods
plot_convergence_diagnostics = function(bart_machine, plots = c("sigsqs", "mh_acceptance", "num_nodes", "tree_depths")){
  check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
  if(length(plots) > 2){
    par(mfrow = c(2, 2))	  
	} else if (length(plots) == 2){
	  par(mfrow = c(1, 2))
	} else {
	  par(mfrow = c(1, 1))
	}    

  if ("sigsqs" %in% plots){
    if (bart_machine$pred_type == "regression"){
      plot_sigsqs_convergence_diagnostics(bart_machine)
    }
  }
	if ("mh_acceptance" %in% plots){
	  plot_mh_acceptance_reject(bart_machine)
	}
	if ("num_nodes" %in% plots){
	  plot_tree_num_nodes(bart_machine)
	}
	if ("tree_depths" %in% plots){
	  plot_tree_depths(bart_machine)
	}
	
	par(mfrow = c(1, 1))
}

##private function
shapiro_wilk_p_val = function(vec){
	tryCatch(shapiro.test(vec)$p.value, error = function(e){})
}

##function for investigating interactions
interaction_investigator = function(bart_machine, plot = TRUE, num_replicates_for_avg = 5, num_trees_bottleneck = 20, num_var_plot = 50, cut_bottom = NULL, bottom_margin = 10){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	interaction_counts = array(NA, c(bart_machine$p, bart_machine$p, num_replicates_for_avg))
	
	for (r in 1 : num_replicates_for_avg){
		if (r == 1 & num_trees_bottleneck == bart_machine$num_trees){
			interaction_counts[, , r] = sapply(.jcall(bart_machine$java_bart_machine, "[[I", "getInteractionCounts"), .jevalArray)
		} else {
			bart_machine_dup = bart_machine_duplicate(bart_machine, num_trees = num_trees_bottleneck)			
			interaction_counts[, , r] = sapply(.jcall(bart_machine_dup$java_bart_machine, "[[I", "getInteractionCounts"), .jevalArray)
			cat(".")
			if (r %% 40 == 0){
				cat("\n")
			}					
		}
	}
	cat("\n")
	
	interaction_counts_avg = apply(interaction_counts, 1 : 2, mean)
	
	if (bart_machine$use_missing_data == T){
		rownames(interaction_counts_avg) = bart_machine$training_data_features_with_missing_features
	    colnames(interaction_counts_avg) = bart_machine$training_data_features_with_missing_features
	} else {
	    rownames(interaction_counts_avg) = bart_machine$training_data_features
	    colnames(interaction_counts_avg) = bart_machine$training_data_features	
	}
	interaction_counts_sd = apply(interaction_counts, 1 : 2, sd)
	
	#now vectorize the interaction counts
	avg_counts = array(NA, bart_machine$p * (bart_machine$p - 1) / 2)
	sd_counts = array(NA, bart_machine$p * (bart_machine$p - 1) / 2)
	iter = 1
	for (i in 1 : bart_machine$p){
		for (j in 1 : bart_machine$p){
			if (j <= i){
				avg_counts[iter] = interaction_counts_avg[i, j]
				sd_counts[iter] = interaction_counts_sd[i, j]
				names(avg_counts)[iter] = paste(rownames(interaction_counts_avg)[i], "x", rownames(interaction_counts_avg)[j])
				iter = iter + 1
			}
		}
	}
	num_total_interactions = bart_machine$p * (bart_machine$p + 1) / 2
	if (num_var_plot == Inf || num_var_plot > num_total_interactions){
		num_var_plot = num_total_interactions
	}
	
	avg_counts_sorted_indices = sort(avg_counts, decreasing = TRUE, index.return = TRUE)$ix
	avg_counts = avg_counts[avg_counts_sorted_indices][1 : num_var_plot]
	sd_counts = sd_counts[avg_counts_sorted_indices][1 : num_var_plot]
	
	if (is.null(cut_bottom)){
		ylim_bottom = 0
	} else {
		ylim_bottom = cut_bottom * min(avg_counts)
	}
	if (plot){
		#now create the bar plot
		par(mar = c(bottom_margin, 6, 3, 0))
		if (is.na(sd_counts[1])){
			moe = 0
		} else {
			moe = 1.96 * sd_counts / sqrt(num_replicates_for_avg)
		}
		bars = barplot(avg_counts, 
			names.arg = names(avg_counts), 
			las = 2, 
			ylab = "Relative Importance", 
			col = "gray",#rgb(0.39, 0.39, 0.59),
			ylim = c(ylim_bottom, max(avg_counts + moe)),
#			main = paste("Interactions in bartMachine Model Averaged over", num_replicates_for_avg, "Replicates"),
			xpd = FALSE #clips the bars outside of the display region (why is this not a default setting?)
		)
		if (!is.na(sd_counts[1])){
			conf_upper = avg_counts + 1.96 * sd_counts / sqrt(num_replicates_for_avg)
			conf_lower = avg_counts - 1.96 * sd_counts / sqrt(num_replicates_for_avg)
			segments(bars, avg_counts, bars, conf_upper, col = rgb(0.59, 0.39, 0.39), lwd = 3) # Draw error bars
			segments(bars, avg_counts, bars, conf_lower, col = rgb(0.59, 0.39, 0.39), lwd = 3)			
		}
		par(mar = c(5.1, 4.1, 4.1, 2.1))		
	}
	
	invisible(list(interaction_counts_avg = interaction_counts_avg, interaction_counts_sd = interaction_counts_sd))
}

##partial dependence plot
pd_plot = function(bart_machine, j, levs = c(0.05, seq(from = 0.10, to = 0.90, by = 0.10), 0.95), lower_ci = 0.025, upper_ci = 0.975){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (class(j) == "numeric" && (j < 1 || j > bart_machine$p)){
		stop(paste("You must set j to a number between 1 and p =", bart_machine$p))
	} else if (class(j) == "character" && !(j %in% bart_machine$training_data_features)){
		stop("j must be the name of one of the training features (see \"<bart_model>$training_data_features\")")
	} else if (!(class(j) == "numeric" || class(j) == "character")){
		stop("j must be a column number or column name")
	}
	
	x_j = bart_machine$model_matrix_training_data[, j]
	x_j_quants = quantile(x_j, levs)
	bart_predictions_by_quantile = array(NA, c(length(levs), bart_machine$n, bart_machine$num_iterations_after_burn_in))
	
	for (q in 1 : length(levs)){
		x_j_quant = x_j_quants[q]
		
		#now create test data matrix
		test_data = bart_machine$X
		test_data[, j] = rep(x_j_quant, bart_machine$n)
		
		bart_predictions_by_quantile[q, , ] = bart_machine_get_posterior(bart_machine, test_data)$y_hat_posterior_samples
		cat(".")
	}
	cat("\n")
	
  	if (bart_machine$pred_type == "classification"){ ##convert to probits
    	bart_predictions_by_quantile = qnorm(bart_predictions_by_quantile)
  	}
  
	bart_avg_predictions_by_quantile_by_gibbs = array(NA, c(length(levs), bart_machine$num_iterations_after_burn_in))
	for (q in 1 : length(levs)){
		for (g in 1 : bart_machine$num_iterations_after_burn_in){
			bart_avg_predictions_by_quantile_by_gibbs[q, g] = mean(bart_predictions_by_quantile[q, , g])
		}		
	}
	
	bart_avg_predictions_by_quantile = apply(bart_avg_predictions_by_quantile_by_gibbs, 1, mean)
	bart_avg_predictions_lower = apply(bart_avg_predictions_by_quantile_by_gibbs, 1, quantile, probs = lower_ci)
	bart_avg_predictions_upper = apply(bart_avg_predictions_by_quantile_by_gibbs, 1, quantile, probs = upper_ci)
	
	var_name = ifelse(class(j) == "character", j, bart_machine$training_data_features[j])
  ylab_name = ifelse(bart_machine$pred_type == "classification", "Partial Effect (Probits)", "Partial Effect")
	plot(x_j_quants, bart_avg_predictions_by_quantile, 
			type = "o", 
			main = "Partial Dependence Plot",
			ylim = c(min(bart_avg_predictions_lower, bart_avg_predictions_upper), max(bart_avg_predictions_lower, bart_avg_predictions_upper)),
			ylab = ylab_name,
			xlab = paste(var_name, "plotted at specified quantiles"))
	lines(x_j_quants, bart_avg_predictions_lower, type = "o", col = "blue")
	lines(x_j_quants, bart_avg_predictions_upper, type = "o", col = "blue")
	
	invisible(list(x_j_quants = x_j_quants, bart_avg_predictions_by_quantile = bart_avg_predictions_by_quantile))
}

##plot and invisibly return out-of-sample RMSE by the number of trees
rmse_by_num_trees = function(bart_machine, tree_list = c(5, seq(10, 50, 10), 100, 150, 200), in_sample = FALSE, plot = TRUE, holdout_pctg = 0.3, num_replicates = 4, ...){
	check_serialization(bart_machine) #ensure the Java object exists and fire an error if not
	
	if (bart_machine$pred_type == "classification"){
		stop("This function does not work for classification.")
	}		
	X = bart_machine$X
	y = bart_machine$y
	n = bart_machine$n
	
	rmses = array(NA, c(num_replicates, length(tree_list)))
	cat("num_trees = ")
	for (t in 1 : length(tree_list)){
		for (r in 1 : num_replicates){
			if (in_sample){
				bart_machine_dup = bart_machine_duplicate(bart_machine, num_trees = tree_list[t], run_in_sample = TRUE)
				rmses[r, t] = bart_machine_dup$rmse_train				
			} else {
				holdout_indicies = sample(1 : n, holdout_pctg * n)
				Xtrain = X[setdiff(1 : n, holdout_indicies), ]
				ytrain = y[setdiff(1 : n, holdout_indicies)]
				Xtest = X[holdout_indicies, ]
				ytest = y[holdout_indicies]
				
				bart_machine_dup = bart_machine_duplicate(bart_machine, Xtrain, ytrain, num_trees = tree_list[t])
				predict_obj = suppressWarnings(bart_predict_for_test_data(bart_machine_dup, Xtest, ytest)) ##predict on holdout
				rmses[r, t] = predict_obj$rmse				
			}
			cat("..")
			cat(tree_list[t])			
		}
	}
	cat("\n")
	
	rmse_means = colMeans(rmses)

	if (plot){ ##plotting
		rmse_sds = apply(rmses, 2, sd)
		y_mins = rmse_means - 2 * rmse_sds
		y_maxs = rmse_means + 2 * rmse_sds
		plot(tree_list, rmse_means, 
			type = "o", 
			xlab = "Number of Trees", 
			ylab = paste(ifelse(in_sample, "In-Sample", "Out-Of-Sample"), "RMSE"), 
			main = paste(ifelse(in_sample, "In-Sample", "Out-Of-Sample"), "RMSE by Number of Trees"), 
			ylim = c(min(y_mins), max(y_maxs)), ...)
		if (num_replicates > 1){
			for (t in 1 : length(tree_list)){
				lowers = rmse_means[t] - 1.96 * rmse_sds[t] / sqrt(num_replicates)
				uppers = rmse_means[t] + 1.96 * rmse_sds[t] / sqrt(num_replicates)
				segments(tree_list[t], lowers, tree_list[t], uppers, col = "grey", lwd = 0.1)
			}
		}
	}
	invisible(rmse_means)
}

