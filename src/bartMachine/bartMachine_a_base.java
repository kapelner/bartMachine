package bartMachine;

import java.io.Serializable;

/**
 * The base class for any BART implementation. Contains
 * mostly instance variables and settings for the algorithm
 * 
 * @author Adam Kapelner and Justin Bleich
 */
public abstract class bartMachine_a_base extends Classifier implements Serializable {

	/** all Gibbs samples for burn-in and post burn-in where each entry is a vector of pointers to the <code>num_trees</code> trees in the sum-of-trees model */
	protected bartMachineTreeNode[][] gibbs_samples_of_bart_trees;
	/** Gibbs samples post burn-in where each entry is a vector of pointers to the <code>num_trees</code> trees in the sum-of-trees model */
	protected bartMachineTreeNode[][] gibbs_samples_of_bart_trees_after_burn_in;
	/** Gibbs samples for burn-in and post burn-in of the variances */
	protected double[] gibbs_samples_of_sigsq;
	/** Gibbs samples for post burn-in of the variances */
	protected double[] gibbs_samples_of_sigsq_after_burn_in;
	/** a record of whether each Gibbs sample accepted or rejected the MH step within each of the <code>num_trees</code> trees */
	protected boolean[][] accept_reject_mh;
	/** a record of the proposal of each Gibbs sample within each of the <code>m</code> trees: G, P or C for "grow", "prune", "change" */
	protected char[][] accept_reject_mh_steps;

	/** the number of trees in our sum-of-trees model */
	protected int num_trees;
	/** how many Gibbs samples we burn-in for */
	protected int num_gibbs_burn_in;
	/** how many total Gibbs samples in a BART model creation */
	protected int num_gibbs_total_iterations;

	/** the current thread being used to run this Gibbs sampler */
	protected int threadNum;
	/** how many CPU cores to use during model creation */
	protected int num_cores;
	/** 
	 * whether or not we use the memory cache feature
	 * 
	 * @see Section 3.1 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected boolean mem_cache_for_speed;	
	/** saves indices in nodes (useful for computing weights) */
	protected boolean flush_indices_to_save_ram;
	/** should we print stuff out to screen? */
	protected boolean verbose = true;
	


	/** Remove unnecessary data from the Gibbs chain to conserve RAM */
	protected void FlushData() {
		for (bartMachineTreeNode[] bart_trees : gibbs_samples_of_bart_trees){
			FlushDataForSample(bart_trees);
		}	
	}
	
	/** Remove unnecessary data from an individual Gibbs sample */
	protected void FlushDataForSample(bartMachineTreeNode[] bart_trees) {
		for (bartMachineTreeNode tree : bart_trees){
			tree.flushNodeData();
		}
	}	

	/** Must be implemented, but does nothing */
	public void StopBuilding(){}	

	public void setThreadNum(int threadNum) {
		this.threadNum = threadNum;
	}
	
	public void setVerbose(boolean verbose){
		this.verbose = verbose;
	}
	
	public void setTotalNumThreads(int num_cores) {
		this.num_cores = num_cores;
	}

	public void setMemCacheForSpeed(boolean mem_cache_for_speed){
		this.mem_cache_for_speed = mem_cache_for_speed;
	}
	
	public void setFlushIndicesToSaveRAM(boolean flush_indices_to_save_ram) {
		this.flush_indices_to_save_ram = flush_indices_to_save_ram;
	}

	public void setNumTrees(int m){
		this.num_trees = m;
	}
}
