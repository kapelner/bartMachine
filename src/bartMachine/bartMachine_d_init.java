package bartMachine;

/**
 * This portion of the code initializes the Gibbs sampler
 * 
 * @author Adam Kapelner and Justin Bleich
 */
public abstract class bartMachine_d_init extends bartMachine_c_debug {

	/** during debugging, we may want to fix sigsq */
	protected double fixed_sigsq;
	/** the number of the current Gibbs sample */
	protected int gibbs_sample_num;
	/** cached current sum of residuals vector */
	protected double[] sum_resids_vec;	
	
	/** Initializes the Gibbs sampler setting all zero entries and moves the counter to the first sample */
	protected void SetupGibbsSampling(){
		InitGibbsSamplingData();	
		InitizializeSigsq();
		InitializeTrees();
		InitializeMus();	
		//the zeroth gibbs sample is the initialization we just did; now we're onto the first in the chain
		gibbs_sample_num = 1;
		
		sum_resids_vec = new double[n];
	}
	
	/** Initializes the vectors that hold information across the Gibbs sampler */
	protected void InitGibbsSamplingData(){
		//now initialize the gibbs sampler array for trees and error variances
		gibbs_samples_of_bart_trees = new bartMachineTreeNode[num_gibbs_total_iterations + 1][num_trees];
		gibbs_samples_of_bart_trees_after_burn_in = new bartMachineTreeNode[num_gibbs_total_iterations - num_gibbs_burn_in + 1][num_trees];
		gibbs_samples_of_sigsq = new double[num_gibbs_total_iterations + 1];	
		gibbs_samples_of_sigsq_after_burn_in = new double[num_gibbs_total_iterations - num_gibbs_burn_in];
		
		accept_reject_mh = new boolean[num_gibbs_total_iterations + 1][num_trees];	
		accept_reject_mh_steps = new char[num_gibbs_total_iterations + 1][num_trees];
	}
	
	/** Initializes the tree structures in the zeroth Gibbs sample to be merely stumps */
	protected void InitializeTrees() {
		//create the array of trees for the zeroth gibbs sample
		bartMachineTreeNode[] bart_trees = new bartMachineTreeNode[num_trees];		
		for (int i = 0; i < num_trees; i++){
			bartMachineTreeNode stump = new bartMachineTreeNode(this);
			stump.setStumpData(X_y, y_trans, p);
			bart_trees[i] = stump;
		}	
		gibbs_samples_of_bart_trees[0] = bart_trees;	
	}

	
	/** Initializes the leaf structure (the mean predictions) by setting them to zero (in the transformed scale, this is the center of the range) */
	protected void InitializeMus() {
		for (bartMachineTreeNode stump : gibbs_samples_of_bart_trees[0]){
			stump.y_pred = 0;
		}
	}
	
	/** Initializes the first variance value by drawing from the prior */
	protected void InitizializeSigsq() {
		gibbs_samples_of_sigsq[0] = StatToolbox.sample_from_inv_gamma(hyper_nu / 2, 2 / (hyper_nu * hyper_lambda));
	}	
	
	/** this is the number of posterior Gibbs samples afte burn-in (thinning was never implemented) */
	public int numSamplesAfterBurningAndThinning(){
		return num_gibbs_total_iterations - num_gibbs_burn_in;
	}

	public void setNumGibbsBurnIn(int num_gibbs_burn_in){
		this.num_gibbs_burn_in = num_gibbs_burn_in;
	}
	
	public void setNumGibbsTotalIterations(int num_gibbs_total_iterations){
		this.num_gibbs_total_iterations = num_gibbs_total_iterations;
	}
	
	public void setSigsq(double fixed_sigsq){
		this.fixed_sigsq = fixed_sigsq;
	}
	
	public boolean[][] getAcceptRejectMH(){
		return accept_reject_mh;
	}
}
