package bartMachine;

import gnu.trove.list.array.TIntArrayList;

/**
 * This portion of the code implements the informed prior information on covariates feature.
 * 
 * @author 	Adam Kapelner and Justin Bleich
 * @see 	Section 4.10 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
 */
public class bartMachine_i_prior_cov_spec extends bartMachine_h_eval {
	
	/** Do we use this feature in this BART model? */
	protected boolean use_prior_cov_spec;
	/** This is a probability vector which is the prior on which covariates to split instead of the uniform discrete distribution by default */
	protected double[] cov_split_prior;	

	
	/**
	 * Pick one predictor from a set of valid predictors that can be part of a split rule at a node
	 * while accounting for the covariate prior.
	 * 
	 * @param node	The node of interest
	 * @return		The index of the column to split on
	 */
	private int pickRandomPredictorThatCanBeAssignedPriorCovSpec(bartMachineTreeNode node){
		TIntArrayList predictors = node.predictorsThatCouldBeUsedToSplitAtNode();
		//get probs of split prior based on predictors that can be used and weight it accordingly
		double[] weighted_cov_split_prior_subset = getWeightedCovSplitPriorSubset(predictors);
		//choose predictor based on random prior value	
		return StatToolbox.multinomial_sample(predictors, weighted_cov_split_prior_subset);
	}
	
	/**
	 * The prior-adjusted number of covariates available to be split at this node
	 *  
	 * @param node		The node of interest
	 * @return			The prior-adjusted number of covariates that can be split
	 */
	private double pAdjPriorCovSpec(bartMachineTreeNode node) {
		if (node.padj == null){
			node.padj = node.predictorsThatCouldBeUsedToSplitAtNode().size();
		}
		if (node.padj == 0){
			return 0;
		}
		if (node.isLeaf){
			return node.padj;
		}			
		//pull out weighted cov split prior subset vector
		TIntArrayList predictors = node.predictorsThatCouldBeUsedToSplitAtNode();
		//get probs of split prior based on predictors that can be used and weight it accordingly
		double[] weighted_cov_split_prior_subset = getWeightedCovSplitPriorSubset(predictors);	
		
		//find index inside predictor vector
		int index = bartMachineTreeNode.BAD_FLAG_int;
		for (int i = 0; i < predictors.size(); i++){
			if (predictors.get(i) == node.splitAttributeM){
				index = i;
				break;
			}
		}
		
		//return inverse probability		
		return 1 / weighted_cov_split_prior_subset[index];
	}
	
	/**
	 * Given a set of valid predictors return the probability vector that corresponds to the
	 * elements of <code>cov_split_prior</code> re-normalized because some entries may be deleted
	 * 
	 * @param predictors	The indices of the valid covariates
	 * @return				The updated and renormalized prior probability vector on the covariates to split
	 */
	private double[] getWeightedCovSplitPriorSubset(TIntArrayList predictors) {
		double[] weighted_cov_split_prior_subset = new double[predictors.size()];
		for (int i = 0; i < predictors.size(); i++){
			weighted_cov_split_prior_subset[i] = cov_split_prior[predictors.get(i)];
		}
		Tools.normalize_array(weighted_cov_split_prior_subset);
		return weighted_cov_split_prior_subset;
	}	

	public void setCovSplitPrior(double[] cov_split_prior) {
		this.cov_split_prior = cov_split_prior;
		//if we're setting the vector, we're using this feature
		use_prior_cov_spec = true;
	}
	
	/////////////nothing but scaffold code below, do not alter!
	
	public int pickRandomPredictorThatCanBeAssigned(bartMachineTreeNode node){
		if (use_prior_cov_spec){
			return pickRandomPredictorThatCanBeAssignedPriorCovSpec(node);
		}
		return super.pickRandomPredictorThatCanBeAssigned(node);
	}	
	
	public double pAdj(bartMachineTreeNode node){
		if (use_prior_cov_spec){
			return pAdjPriorCovSpec(node);
		}
		return super.pAdj(node);
	}	
	
}
