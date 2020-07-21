package bartMachine;

import java.io.Serializable;

/**
 * This portion of the code performs everything in 
 * the Gibbs sampling except for the posterior sampling itself
 * 
 * @author Adam Kapelner and Justin Bleich
 */
@SuppressWarnings("serial")
public abstract class bartMachine_e_gibbs_base extends bartMachine_d_init implements Serializable{

	/** Builds a BART model by unleashing the Gibbs sampler */
	public void Build() {
		SetupGibbsSampling();
		DoGibbsSampling();
	}	

	/** Run the Gibbs sampler for the total number of samples prespecified while flushing unneeded memory from the previous sample */
	protected void DoGibbsSampling(){
		while (gibbs_sample_num <= num_gibbs_total_iterations){			
			DoOneGibbsSample();
			//now flush the previous previous gibbs sample to not leak memory
			FlushDataForSample(gibbs_samples_of_bart_trees[gibbs_sample_num - 1]);
			DeleteBurnInsOnPreviousSamples();
			gibbs_sample_num++;
		}
	}
	
	/** Run one Gibbs sample at the current sample number */ 
	protected void DoOneGibbsSample(){
		//this array is the array of trees for this given sample
		final bartMachineTreeNode[] bart_trees = new bartMachineTreeNode[num_trees];				
		final TreeArrayIllustration tree_array_illustration = new TreeArrayIllustration(gibbs_sample_num, unique_name);

		//we cycle over each tree and update it according to formulas 15, 16 on p274
		double[] R_j = new double[n];
		for (int t = 0; t < num_trees; t++){
			if (verbose){
				GibbsSampleDebugMessage(t);
			}
			R_j = SampleTree(gibbs_sample_num, t, bart_trees, tree_array_illustration);
			SampleMusWrapper(gibbs_sample_num, t);				
		}
		//now we have the last residual vector which we pass on to sample sigsq
		SampleSigsq(gibbs_sample_num, getResidualsFromFullSumModel(gibbs_sample_num, R_j));
		if (tree_illust){
			illustrate(tree_array_illustration);
		}
	}

	/** Print a Gibbs sample debug message */
	protected void GibbsSampleDebugMessage(int t) {
		if (t == 0 && gibbs_sample_num % 100 == 0){
			String message = "Iteration " + gibbs_sample_num + "/" + num_gibbs_total_iterations;
			if (num_cores > 1){
				message += "  thread: " + (threadNum + 1);
			}
			if (ON_WINDOWS){
				long mem_used = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
				long max_mem = Runtime.getRuntime().maxMemory();
				message += "  mem: " + TreeIllustration.one_digit_format.format(mem_used / 1000000.0) + "/" + TreeIllustration.one_digit_format.format(max_mem / 1000000.0) + "MB";
			}
			System.out.println(message);
		}
	}

	/** 
	 * A wrapper for sampling the mus (mean predictions at terminal nodes). This function implements part of the "residual diffing" explained in the paper.
	 * 
	 * @param sample_num	The current sample number of the Gibbs sampler
	 * @param t				The tree index number in 1...<code>num_trees</code>
	 * @see Section 3.1 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected void SampleMusWrapper(int sample_num, int t) {
		bartMachineTreeNode previous_tree = gibbs_samples_of_bart_trees[sample_num - 1][t];
		//subtract out previous tree's yhats
		sum_resids_vec = Tools.subtract_arrays(sum_resids_vec, previous_tree.yhats);
		bartMachineTreeNode tree = gibbs_samples_of_bart_trees[sample_num][t];

		double current_sigsq = gibbs_samples_of_sigsq[sample_num - 1];
		assignLeafValsBySamplingFromPosteriorMeanAndSigsqAndUpdateYhats(tree, current_sigsq);
		
		//after mus are sampled, we need to update the sum_resids_vec
		//add in current tree's yhats		
		sum_resids_vec = Tools.add_arrays(sum_resids_vec, tree.yhats);
	}

	/** deletes from memory tree Gibbs samples in the burn-in portion of the chain */
	private void DeleteBurnInsOnPreviousSamples() {
		if (gibbs_sample_num <= num_gibbs_burn_in + 1 && gibbs_sample_num >= 2){
			gibbs_samples_of_bart_trees[gibbs_sample_num - 2] = null;
		}
	}

	/**
	 * A wrapper that is responsible for drawing variance values
	 *  
	 * @param sample_num	The current sample number of the Gibbs sampler
	 * @param es			The vector of residuals at this point in the Gibbs chain
	 */
	protected void SampleSigsq(int sample_num, double[] es) {
		double sigsq = drawSigsqFromPosterior(sample_num, es);
		gibbs_samples_of_sigsq[sample_num] = sigsq;
	}
	
	/**
	 * This function calculates the residuals from the sum-of-trees model using the diff trick explained in the paper
	 * 
	 * @param sample_num	The current sample number of the Gibbs sampler
	 * @param R_j			The residuals of the model save the last tree's contribution
	 * @return				The vector of residuals at this point in the Gibbs chain
	 * @see Section 3.1 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected double[] getResidualsFromFullSumModel(int sample_num, double[] R_j){	
		//all we need to do is subtract the last tree's yhats now
		bartMachineTreeNode last_tree = gibbs_samples_of_bart_trees[sample_num][num_trees - 1];
		for (int i = 0; i < n; i++){
			R_j[i] -= last_tree.yhats[i];
		}
		return R_j;
	}
	
	/**
	 * A wrapper for sampling one tree during the Gibbs sampler
	 * 
	 * @param sample_num					The current sample number of the Gibbs sampler
	 * @param t								The current tree to be sampled
	 * @param trees							The trees in this Gibbs sampler
	 * @param tree_array_illustration		The tree array (for debugging purposes only)
	 * @return								The responses minus the sum of the trees' contribution up to this point
	 */
	protected double[] SampleTree(int sample_num, int t, bartMachineTreeNode[] trees, TreeArrayIllustration tree_array_illustration) {
		//first copy the tree from the previous gibbs position
		final bartMachineTreeNode copy_of_old_jth_tree = gibbs_samples_of_bart_trees[sample_num - 1][t].clone();
		
		//okay so first we need to get "y" that this tree sees. This is defined as R_j in formula 12 on p274
		//just go to sum_residual_vec and subtract it from y_trans
		double[] R_j = Tools.add_arrays(Tools.subtract_arrays(y_trans, sum_resids_vec), copy_of_old_jth_tree.yhats);
		
		//now, (important!) set the R_j's as this tree's data.
		copy_of_old_jth_tree.updateWithNewResponsesRecursively(R_j);
		
		//sample from T_j | R_j, \sigma
		//now we will run one M-H step on this tree with the y as the R_j
		bartMachineTreeNode new_jth_tree = metroHastingsPosteriorTreeSpaceIteration(copy_of_old_jth_tree, t, accept_reject_mh, accept_reject_mh_steps);
		
		//add it to the vector of current sample's trees
		trees[t] = new_jth_tree;
		
		//now set the new trees in the gibbs sample pantheon
		gibbs_samples_of_bart_trees[sample_num] = trees;
		tree_array_illustration.AddTree(new_jth_tree);		
		//return the updated residuals
		return R_j;
	}
	
	protected abstract double drawSigsqFromPosterior(int sample_num, double[] es);
	
	protected abstract bartMachineTreeNode metroHastingsPosteriorTreeSpaceIteration(bartMachineTreeNode copy_of_old_jth_tree, int t, boolean[][] accept_reject_mh, char[][] accept_reject_mh_steps);

	protected abstract void assignLeafValsBySamplingFromPosteriorMeanAndSigsqAndUpdateYhats(bartMachineTreeNode node, double current_sigsq);
}
