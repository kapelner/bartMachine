package bartMachine;

import java.util.ArrayList;

/**
 * This portion of the code performs the Metropolis-Hastings tree search step
 * 
 * @author Adam Kapelner and Justin Bleich
 */
public abstract class bartMachine_g_mh extends bartMachine_f_gibbs_internal {
	
	/** turning this flag on prints out debugging information about the Metropolis-Hastings tree search step */
	private static final boolean DEBUG_MH = false;
	
	/** the hyperparameter of the probability of picking a grow step during the Metropolis-Hastings tree proposal */
	protected double prob_grow;
	/** the hyperparameter of the probability of picking a prune step during the Metropolis-Hastings tree proposal */
	protected double prob_prune;

	/** the types of steps in the Metropolis-Hastings tree search */
	public enum Steps {GROW, PRUNE, CHANGE};
	
	/**
	 * Performs one Metropolis-Hastings step for one tree
	 * 
	 * @param T_i				The original tree to be changed by a proposal step
	 * @param iteration_name	we just use this when naming the image file of this illustration (debugging only)
	 * @return 					the next tree (T_{i+1}) via one iteration of M-H which can be the proposal tree (if the step was accepted) or the original tree (if the step was rejected)
	 */
	protected bartMachineTreeNode metroHastingsPosteriorTreeSpaceIteration(bartMachineTreeNode T_i, int tree_num, boolean[][] accept_reject_mh, char[][] accept_reject_mh_steps) {
		bartMachineTreeNode T_star = T_i.clone();		
		//each proposal will calculate its own value, but this has to be initialized atop		
		double log_r = 0;
		
		//if it's a stump force a GROW change, otherwise pick randomly from the steps according to the "hidden parameters"
		switch (T_i.isStump() ? Steps.GROW : randomlyPickAmongTheProposalSteps()){
			case GROW:
				accept_reject_mh_steps[gibbs_sample_num][tree_num] = 'G';
				log_r = doMHGrowAndCalcLnR(T_i, T_star);
				break;
			case PRUNE:
				accept_reject_mh_steps[gibbs_sample_num][tree_num] = 'P';
				log_r = doMHPruneAndCalcLnR(T_i, T_star);
				break;
			case CHANGE:
				accept_reject_mh_steps[gibbs_sample_num][tree_num] = 'C';
				log_r = doMHChangeAndCalcLnR(T_i, T_star);
				break;				
		}	
		//draw from a Uniform 0, 1 and log it
		double ln_u_0_1 = Math.log(StatToolbox.rand());
		if (DEBUG_MH){
			System.out.println("ln u = " + ln_u_0_1 + 
					" <? ln(r) = " + 
					(log_r < -99999 ? "very small" : log_r));
		}
		if (ln_u_0_1 < log_r){ //accept proposal if the draw is less than the ratio
			if (DEBUG_MH){
				System.out.println("proposal ACCEPTED\n\n");
			}
			//mark it was accepted
			accept_reject_mh[gibbs_sample_num][tree_num] = true;
			return T_star;
		}		
		//reject proposal
		if (DEBUG_MH){
			System.out.println("proposal REJECTED\n\n");
		}
		accept_reject_mh[gibbs_sample_num][tree_num] = false;
		return T_i;
	}

	/**
	 * Perform the grow step on a tree and return the log Metropolis-Hastings ratio
	 * 
	 * @param T_i		The root node of the original tree 
	 * @param T_star	The root node of a copy of the original tree where one node will be grown
	 * @return			The log Metropolis-Hastings ratio
	 * @see 			Section A.1 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected double doMHGrowAndCalcLnR(bartMachineTreeNode T_i, bartMachineTreeNode T_star) {
		//first select a node that can be grown
		bartMachineTreeNode grow_node = pickGrowNode(T_star);
		//if we couldn't find a node that be grown, then we can't grow, so reject offhand
		if (grow_node == null){
			return Double.NEGATIVE_INFINITY;					
		}
		
		//now start the growth process
		//first pick the attribute and then the split
		grow_node.splitAttributeM = pickRandomPredictorThatCanBeAssigned(grow_node);
		T_star.increment_variable_count(grow_node.splitAttributeM);
		grow_node.splitValue = grow_node.pickRandomSplitValue();
		//now pick randomly which way the missing data goes - left (false) or right (true)
		grow_node.sendMissingDataRight = bartMachineTreeNode.pickRandomDirectionForMissingData();
		//inform the user if things go awry
		if (grow_node.splitValue == bartMachineTreeNode.BAD_FLAG_double){
			return Double.NEGATIVE_INFINITY;					
		}			
		grow_node.isLeaf = false;
		grow_node.left = new bartMachineTreeNode(grow_node);
		grow_node.right = new bartMachineTreeNode(grow_node);
		grow_node.propagateDataByChangedRule();

		if (grow_node.left.n_eta <= 0 || grow_node.right.n_eta <= 0){
			if (DEBUG_MH){
				System.err.println("ERROR GROW <<" + grow_node.stringLocation(true) + ">> cannot split a node where daughter only has NO data points   proposal ln(r) = -oo DUE TO CANNOT GROW");
			}
			return Double.NEGATIVE_INFINITY;
		}
		
		double ln_transition_ratio_grow = calcLnTransRatioGrow(T_i, T_star, grow_node);
		double ln_likelihood_ratio_grow = calcLnLikRatioGrow(grow_node);
		double ln_tree_structure_ratio_grow = calcLnTreeStructureRatioGrow(grow_node);
		
		if (DEBUG_MH){
			System.out.println(gibbs_sample_num + " GROW  <<" + grow_node.stringLocation(true) + ">> ---- X_" + (grow_node.splitAttributeM) + 
				" < " + TreeIllustration.two_digit_format.format(grow_node.splitValue) + " & " + (grow_node.sendMissingDataRight ? "M -> R" : "M -> L")+ 
				"\n  ln trans ratio: " + ln_transition_ratio_grow + " ln lik ratio: " + ln_likelihood_ratio_grow + " ln structure ratio: " + ln_tree_structure_ratio_grow +			
				"\n  trans ratio: " + 
				(Math.exp(ln_transition_ratio_grow) < 0.00001 ? "very small" : Math.exp(ln_transition_ratio_grow)) +
				"  lik ratio: " + 
				(Math.exp(ln_likelihood_ratio_grow) < 0.00001 ? "very small" : Math.exp(ln_likelihood_ratio_grow)) +
				"  structure ratio: " + 
				(Math.exp(ln_tree_structure_ratio_grow) < 0.00001 ? "very small" : Math.exp(ln_tree_structure_ratio_grow)));
		}
		return ln_transition_ratio_grow + ln_likelihood_ratio_grow + ln_tree_structure_ratio_grow;
	}
	
	/**
	 * Perform the prune step on a tree and return the log Metropolis-Hastings ratio
	 * 
	 * @param T_i		The root node of the original tree 
	 * @param T_star	The root node of a copy of the original tree where one set of terminal nodes will be pruned
	 * @return			The log Metropolis-Hastings ratio
	 * @see 			Section A.2 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected double doMHPruneAndCalcLnR(bartMachineTreeNode T_i, bartMachineTreeNode T_star) {
		//first select a node that can be pruned
		bartMachineTreeNode prune_node = pickPruneNodeOrChangeNode(T_star);
		//if we didn't find one to prune, then we can't prunce, so reject offhand
		if (prune_node == null){
			return Double.NEGATIVE_INFINITY;
		}		
		T_star.decrement_variable_count(prune_node.splitAttributeM);
		double ln_transition_ratio_prune = calcLnTransRatioPrune(T_i, T_star, prune_node);
		double ln_likelihood_ratio_prune = -calcLnLikRatioGrow(prune_node); //inverse of before (will speed up later)
		double ln_tree_structure_ratio_prune = -calcLnTreeStructureRatioGrow(prune_node);
		
		if (DEBUG_MH){
			System.out.println(gibbs_sample_num + " PRUNE <<" + prune_node.stringLocation(true) + 
					">> ---- X_" + (prune_node.splitAttributeM == bartMachineTreeNode.BAD_FLAG_int ? "null" : (prune_node.splitAttributeM + 1)) + " < " + TreeIllustration.two_digit_format.format(prune_node.splitValue == bartMachineTreeNode.BAD_FLAG_double ? Double.NaN : prune_node.splitValue) + 
				"\n  ln trans ratio: " + ln_transition_ratio_prune + " ln lik ratio: " + ln_likelihood_ratio_prune + " ln structure ratio: " + ln_tree_structure_ratio_prune +
				"\n  trans ratio: " + 
				(Math.exp(ln_transition_ratio_prune) < 0.00001 ? "very small" : Math.exp(ln_transition_ratio_prune)) +
				"  lik ratio: " + 
				(Math.exp(ln_likelihood_ratio_prune) < 0.00001 ? "very small" : Math.exp(ln_likelihood_ratio_prune)) +
				"  structure ratio: " + 
				(Math.exp(ln_tree_structure_ratio_prune) < 0.00001 ? "very small" : Math.exp(ln_tree_structure_ratio_prune)));
		}
		bartMachineTreeNode.pruneTreeAt(prune_node);
		return ln_transition_ratio_prune + ln_likelihood_ratio_prune + ln_tree_structure_ratio_prune;
	}	
	
	/**
	 * This function picks a node suitable for pruning or changing. In our implementation this is a 
	 * node that is "singly internal" (ie it has two children and its children are both terminal nodes)
	 * 
	 * @param T					The root of the tree we wish to find singly internal nodes	
	 * @return					A singly internal node selected at random from all candididates or null if none exist
	 */
	protected bartMachineTreeNode pickPruneNodeOrChangeNode(bartMachineTreeNode T) {
		
		//Two checks need to be performed first before we run a search on the tree structure
		//a) If this is the root, we can't prune so return null
		//b) If there are no prunable nodes (not sure how that could happen), return null as well
		
		if (T.isStump()){
			return null;			
		}
		
		ArrayList<bartMachineTreeNode> prunable_and_changeable_nodes = T.getPrunableAndChangeableNodes();
		if (prunable_and_changeable_nodes.size() == 0){
			return null;
		}		
		
		//now we pick one of these nodes randomly
		return prunable_and_changeable_nodes.get((int)Math.floor(StatToolbox.rand() * prunable_and_changeable_nodes.size()));
	}
	
	/**
	 * Calculates the log transition ratio for a grow step
	 * 
	 * @param T_i					The root node of the original tree
	 * @param T_star				The root node of the proposal tree
	 * @param node_grown_in_Tstar	The node that was grown in the proposal tree
	 * @return						The log of the transition ratio
	 * @see 						Section A.1.1 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected double calcLnTransRatioGrow(bartMachineTreeNode T_i, bartMachineTreeNode T_star, bartMachineTreeNode node_grown_in_Tstar) {
		int b = T_i.numLeaves();
		double p_adj = pAdj(node_grown_in_Tstar);
		int n_adj = node_grown_in_Tstar.nAdj();
		int w_2_star = T_star.numPruneNodesAvailable();
		return Math.log(b) + Math.log(p_adj) + Math.log(n_adj) - Math.log(w_2_star); 
	}

	/**
	 * Calculates the log of the likelihood ratio for a grow step
	 * 
	 * @param grow_node		The node that was grown in the proposal tree
	 * @return				The log of the likelihood ratio
	 * @see 				Section A.1.2 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected double calcLnLikRatioGrow(bartMachineTreeNode grow_node) {
		if (grow_node.ln_lik_ratio_grow == null){
			double sigsq = gibbs_samples_of_sigsq[gibbs_sample_num - 1];
			int n_ell = grow_node.n_eta;
			int n_ell_L = grow_node.left.n_eta;
			int n_ell_R = grow_node.right.n_eta;
	
			//now go ahead and calculate it out	in an organized fashion:
			double sigsq_plus_n_ell_hyper_sisgsq_mu = sigsq + n_ell * hyper_sigsq_mu;
			double sigsq_plus_n_ell_L_hyper_sisgsq_mu = sigsq + n_ell_L * hyper_sigsq_mu;
			double sigsq_plus_n_ell_R_hyper_sisgsq_mu = sigsq + n_ell_R * hyper_sigsq_mu;
			double c = 0.5 * (
					Math.log(sigsq) 
					+ Math.log(sigsq_plus_n_ell_hyper_sisgsq_mu) 
					- Math.log(sigsq_plus_n_ell_L_hyper_sisgsq_mu) 
					- Math.log(sigsq_plus_n_ell_R_hyper_sisgsq_mu));
			double d = hyper_sigsq_mu / (2 * sigsq);
			double e = grow_node.left.sumResponsesQuantitySqd() / sigsq_plus_n_ell_L_hyper_sisgsq_mu
					+ grow_node.right.sumResponsesQuantitySqd() / sigsq_plus_n_ell_R_hyper_sisgsq_mu
					- grow_node.sumResponsesQuantitySqd() / sigsq_plus_n_ell_hyper_sisgsq_mu;
			grow_node.ln_lik_ratio_grow = c + d * e;
		}
		return grow_node.ln_lik_ratio_grow;
	}	

	/**
	 * Calculates the log transition ratio for a grow step
	 * 
	 * @param grow_node		The node that was grown in the proposal tree
	 * @return				The log of the transition ratio
	 * @see 				Section A.1.3 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected double calcLnTreeStructureRatioGrow(bartMachineTreeNode grow_node) {
		int d_eta = grow_node.depth;
		double p_adj = pAdj(grow_node);
		int n_adj = grow_node.nAdj();
		return Math.log(alpha) 
				+ 2 * Math.log(1 - alpha / Math.pow(2 + d_eta, beta))
				- Math.log(Math.pow(1 + d_eta, beta) - alpha)
				- Math.log(p_adj) 
				- Math.log(n_adj);
	}	
	
	/**
	 * This calculates the log transition ratio for the prune step.
	 * 
	 * @param T_i			The root node of the original tree
	 * @param T_star		The root node of the proposal tree
	 * @param prune_node	The node that was pruned
	 * @return				The log transition ratio
	 * @see 				Section A.2.1 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected double calcLnTransRatioPrune(bartMachineTreeNode T_i, bartMachineTreeNode T_star, bartMachineTreeNode prune_node) {
		int w_2 = T_i.numPruneNodesAvailable();
		int b = T_i.numLeaves();
		double p_adj = pAdj(prune_node);
		int n_adj = prune_node.nAdj();
		return Math.log(w_2) - Math.log(b - 1) - Math.log(p_adj) - Math.log(n_adj); 
	}

	/**
	 * Selects a node in a tree that is eligible for being grown with two children
	 * 
	 * @param T		The root node of the tree to be searched
	 * @return		The node that is viable for growing
	 */
	protected bartMachineTreeNode pickGrowNode(bartMachineTreeNode T) {
		ArrayList<bartMachineTreeNode> growth_nodes = T.getTerminalNodesWithDataAboveOrEqualToN(2);
		
		//2 checks
		//a) If there is no nodes to grow, return null
		//b) If the node we picked CANNOT grow due to no available predictors, return null as well
		
		//do check a
		if (growth_nodes.size() == 0){
			return null;
		}		
		
		//now we pick one of these nodes with enough data points randomly
		bartMachineTreeNode growth_node = growth_nodes.get((int)Math.floor(StatToolbox.rand() * growth_nodes.size()));

		//do check b
		if (pAdj(growth_node) == 0){
			return null;			
		}
		//if we passed, we can use this node
		return growth_node;
	}

	/**
	 * Perform the change step on a tree and return the log Metropolis-Hastings ratio
	 * 
	 * @param T_i		The root node of the original tree 
	 * @param T_star	The root node of a copy of the original tree where one node will be changed
	 * @return			The log Metropolis-Hastings ratio
	 * @see 			Section A.3 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected double doMHChangeAndCalcLnR(bartMachineTreeNode T_i, bartMachineTreeNode T_star) {
		bartMachineTreeNode eta_star = pickPruneNodeOrChangeNode(T_star);		
		bartMachineTreeNode eta_just_for_calculation = eta_star.clone();
		
		//now start the growth process
		//first pick the attribute and then the split and then which way to send the missing data		
		eta_star.splitAttributeM = pickRandomPredictorThatCanBeAssigned(eta_star);
		eta_star.splitValue = eta_star.pickRandomSplitValue();
		eta_star.sendMissingDataRight = bartMachineTreeNode.pickRandomDirectionForMissingData();
		//inform the user if things go awry
		if (eta_star.splitValue == bartMachineTreeNode.BAD_FLAG_double){
			return Double.NEGATIVE_INFINITY;					
		}
		
		eta_star.propagateDataByChangedRule();
		//the children no longer have the right data!
		eta_star.left.clearRulesAndSplitCache();
		eta_star.right.clearRulesAndSplitCache();
		
		T_star.decrement_variable_count(eta_just_for_calculation.splitAttributeM);
		T_star.increment_variable_count(eta_star.splitAttributeM);
		
		double ln_tree_structure_ratio_change = calcLnLikRatioChange(eta_just_for_calculation, eta_star);
		if (DEBUG_MH){
			System.out.println(gibbs_sample_num + " CHANGE  <<" + eta_star.stringLocation(true) + ">> ---- X_" + (eta_star.splitAttributeM + 1) + 
				" < " + TreeIllustration.two_digit_format.format(eta_star.splitValue) + " & " + (eta_star.sendMissingDataRight ? "M -> R" : "M -> L") + " from " + 
				"X_" + (eta_just_for_calculation.splitAttributeM + 1) + 
				" < " + TreeIllustration.two_digit_format.format(eta_just_for_calculation.splitValue) + " & " + (eta_just_for_calculation.sendMissingDataRight ? "M -> R" : "M -> L") + 	
				"\n  ln lik ratio: " + ln_tree_structure_ratio_change  +			
				"  lik ratio: " + 
				(Math.exp(ln_tree_structure_ratio_change) < 0.00001 ? "very small" : Math.exp(ln_tree_structure_ratio_change)));
		}		
		return ln_tree_structure_ratio_change; //the transition ratio cancels out the tree structure ratio
	}
	
	/**
	 * Calculates the log likelihood ratio for a change step
	 * 
	 * @param eta		The node in the original tree that was targeted for a change in the splitting rule
	 * @param eta_star	The same node but now with a different splitting rule
	 * @return			The log likelihood ratio
	 * @see 			Section A.3.2 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	private double calcLnLikRatioChange(bartMachineTreeNode eta, bartMachineTreeNode eta_star) {
		int n_1_star = eta_star.left.n_eta;
		int n_2_star = eta_star.right.n_eta;
		int n_1 = eta.left.n_eta;
		int n_2 = eta.right.n_eta;
		
		double sigsq = gibbs_samples_of_sigsq[gibbs_sample_num - 1];
		double ratio_sigsqs = sigsq / hyper_sigsq_mu;
		double n_1_plus_ratio_sigsqs = n_1 + ratio_sigsqs;
		double n_2_plus_ratio_sigsqs = n_2 + ratio_sigsqs;
		
		//NOTE: this can be sped up by just taking the diffs
		double sum_sq_1_star = eta_star.left.sumResponsesQuantitySqd(); 
		double sum_sq_2_star = eta_star.right.sumResponsesQuantitySqd(); 
		double sum_sq_1 = eta.left.sumResponsesQuantitySqd();
		double sum_sq_2 = eta.right.sumResponsesQuantitySqd();
		
		//couple checks
		if (n_1_star == 0 || n_2_star == 0){
			eta.printNodeDebugInfo("PARENT BEFORE");
			eta_star.printNodeDebugInfo("PARENT AFTER");
			eta.left.printNodeDebugInfo("LEFT BEFORE");
			eta.right.printNodeDebugInfo("RIGHT BEFORE");
			eta_star.left.printNodeDebugInfo("LEFT AFTER");
			eta_star.right.printNodeDebugInfo("RIGHT AFTER");
			return Double.NEGATIVE_INFINITY;
		}

		//do simplified calculation if the n's remain the same
		if (n_1_star == n_1){
			return 1 / (2 * sigsq) * (
					(sum_sq_1_star - sum_sq_1) / n_1_plus_ratio_sigsqs + 
					(sum_sq_2_star - sum_sq_2) / n_2_plus_ratio_sigsqs
				);
		}
		//otherwise do the lengthy calculation
		else {
			double n_1_star_plus_ratio_sigsqs = n_1_star + ratio_sigsqs;
			double n_2_star_plus_ratio_sigsqs = n_2_star + ratio_sigsqs;
			
			double a = Math.log(n_1_plus_ratio_sigsqs) + 
						Math.log(n_2_plus_ratio_sigsqs) - 
						Math.log(n_1_star_plus_ratio_sigsqs) - 
						Math.log(n_2_star_plus_ratio_sigsqs);
			double b = (
					sum_sq_1_star / n_1_star_plus_ratio_sigsqs + 
					sum_sq_2_star / n_2_star_plus_ratio_sigsqs -
					sum_sq_1 / n_1_plus_ratio_sigsqs - 
					sum_sq_2 / n_2_plus_ratio_sigsqs 					
				);
			
			return 0.5 * a + 1 / (2 * sigsq) * b;
		}
	}

	/**
	 * Randomly chooses among the valid tree proposal steps from a multinomial distribution
	 * 
	 * @return	The step that was chosen
	 */
	protected Steps randomlyPickAmongTheProposalSteps() {
		double roll = StatToolbox.rand();
		if (roll < prob_grow){
			return Steps.GROW;
		}			
		if (roll < prob_grow + prob_prune){
			return Steps.PRUNE;
		}
		return Steps.CHANGE;	
	}

	public void setProbGrow(double prob_grow) {
		this.prob_grow = prob_grow;
	}

	public void setProbPrune(double prob_prune) {
		this.prob_prune = prob_prune;
	}
	
}
