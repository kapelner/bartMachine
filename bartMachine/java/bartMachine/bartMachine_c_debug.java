package bartMachine;

import java.io.Serializable;

/**
 * This portion of the code used to have many debug functions. These have 
 * been removed during the tidy up for release.
 * 
 * @author Adam Kapelner and Justin Bleich
 */
@SuppressWarnings("serial")
public abstract class bartMachine_c_debug extends bartMachine_b_hyperparams implements Serializable{

	/** should we create illustrations of the trees and save the images to the debug directory? */
	protected boolean tree_illust = false;
	
	/** the hook that gets called to save the tree illustrations when the Gibbs sampler begins */
	protected void InitTreeIllustrations() {
		bartMachineTreeNode[] initial_trees = gibbs_samples_of_bart_trees[0];
		TreeArrayIllustration tree_array_illustration = new TreeArrayIllustration(0, unique_name);
		
		for (bartMachineTreeNode tree : initial_trees){
			tree_array_illustration.AddTree(tree);
			tree_array_illustration.addLikelihood(0);			
		}
		tree_array_illustration.CreateIllustrationAndSaveImage();
	}
	
	/** the hook that gets called to save the tree illustrations for each Gibbs sample */
	protected void illustrate(TreeArrayIllustration tree_array_illustration) {
		if (tree_illust){ //
			tree_array_illustration.CreateIllustrationAndSaveImage();
		}
	}
	
	/**
	 * Get the untransformed samples of the sigsqs from the Gibbs chaing
	 * 
	 * @return	The vector of untransformed variances over all the Gibbs samples
	 */
	public double[] getGibbsSamplesSigsqs(){
		double[] sigsqs_to_export = new double[gibbs_samples_of_sigsq.length];
		for (int n_g = 0; n_g < gibbs_samples_of_sigsq.length; n_g++){			
			sigsqs_to_export[n_g] = un_transform_sigsq(gibbs_samples_of_sigsq[n_g]);		
		}
		return sigsqs_to_export;
	}	
	
	/**
	 * Queries the depths of the <code>num_trees</code> trees between a range of Gibbs samples
	 * 
	 * @param n_g_i		The Gibbs sample number to start querying
	 * @param n_g_f		The Gibbs sample number (+1) to stop querying
	 * @return			The depths of all <code>num_trees</code> trees for each Gibbs sample specified
	 */
	public int[][] getDepthsForTrees(int n_g_i, int n_g_f){
		int[][] all_depths = new int[n_g_f - n_g_i][num_trees];
		for (int g = n_g_i; g < n_g_f; g++){
			for (int t = 0; t < num_trees; t++){
				all_depths[g - n_g_i][t] = gibbs_samples_of_bart_trees[g][t].deepestNode();
			}
		}
		return all_depths;
	}
	
	/**
	 * Queries the number of nodes (terminal and non-terminal) in the <code>num_trees</code> trees between a range of Gibbs samples
	 * 
	 * @param n_g_i		The Gibbs sample number to start querying
	 * @param n_g_f		The Gibbs sample number (+1) to stop querying
	 * @return			The number of nodes of all <code>num_trees</code> trees for each Gibbs sample specified
	 */
	public int[][] getNumNodesAndLeavesForTrees(int n_g_i, int n_g_f){
		int[][] all_new_nodes = new int[n_g_f - n_g_i][num_trees];
		for (int g = n_g_i; g < n_g_f; g++){
			for (int t = 0; t < num_trees; t++){
				all_new_nodes[g - n_g_i][t] = gibbs_samples_of_bart_trees[g][t].numNodesAndLeaves();
			}
		}
		return all_new_nodes;
	}	


}
