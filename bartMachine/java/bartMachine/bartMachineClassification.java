package bartMachine;

import java.io.Serializable;

import OpenSourceExtensions.StatUtil;

/**
 * The class that is instantiated to build a binary classification BART model
 * 
 * @author Adam Kapelner and Justin Bleich
 *
 */
public class bartMachineClassification extends bartMachineRegression implements Serializable{

	public bartMachineClassification() {
		super();		
	}

	/**
	 * A Gibbs sample for binary classification BART is a little different
	 * than for regression BART. We no longer sample sigsq's. We instead {@link #SampleZs()},
	 * the latent variables that allow us to estimate the prob(Y = 1).
	 */
	protected void DoOneGibbsSample(){
		//this array is the array of trees for this given sample
		final bartMachineTreeNode[] bart_trees = new bartMachineTreeNode[num_trees];				
		final TreeArrayIllustration tree_array_illustration = new TreeArrayIllustration(gibbs_sample_num, unique_name);

		//get Z's
		SampleZs();
		for (int t = 0; t < num_trees; t++){
			if (verbose){
				GibbsSampleDebugMessage(t);
			}
			SampleTree(gibbs_sample_num, t, bart_trees, tree_array_illustration);
			SampleMusWrapper(gibbs_sample_num, t);				
		}
	}
	
	/** We sample the latent variables, Z, for each of the n observations
	 * 
	 * @see Section 2.3 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	private void SampleZs() {
		for (int i = 0; i < n; i++){
			double g_x_i = 0;
			bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees[gibbs_sample_num - 1];
			for (int t = 0; t < num_trees; t++){
				g_x_i += trees[t].Evaluate(X_y.get(i));
			}
			//y_trans is the Z's from the paper
			y_trans[i] = SampleZi(g_x_i, y_orig[i]);
		}
	}

	/** We sample one latent variable, Z_i
	 * 
	 * @see Section 2.3 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	private double SampleZi(double g_x_i, double y_i) {
		double u = StatToolbox.rand();
		if (y_i == 1){ 
			return g_x_i + StatUtil.getInvCDF((1 - u) * StatToolbox.normal_cdf(-g_x_i) + u, false);
		} 
		else if (y_i == 0){
			return g_x_i - StatUtil.getInvCDF((1 - u) * StatToolbox.normal_cdf(g_x_i) + u, false);
		}
		System.err.println("SampleZi RESPONSE NOT ZERO / ONE");
		System.exit(0);
		return -1;
	}

	/** A dummy value for the unused sigsq's in binary classification BART */
	private static final double SIGSQ_FOR_PROBIT = 1;
	/** 
	 * Sets up Gibbs sampling. We should also blank out the vector <code>gibbs_samples_of_sigsq</code> with dummy values.
	 */
	protected void SetupGibbsSampling(){
		super.SetupGibbsSampling();
		//all sigsqs are now 1 all the time
		for (int g = 0; g < num_gibbs_total_iterations; g++){
			gibbs_samples_of_sigsq[g] = SIGSQ_FOR_PROBIT;
		}
	}

	/**
	 * Calculates the hyperparameters needed for binary classifcation BART.
	 * This only need <code>hyper_sigsq_mu</code>
	 */
	protected void calculateHyperparameters() {
		hyper_mu_mu = 0;
		hyper_sigsq_mu = Math.pow(3 / (hyper_k * Math.sqrt(num_trees)), 2);	
	}		
	
	protected void transformResponseVariable() {
		y_trans = new double[y_orig.length]; //do nothing		
	}	
	
	public double un_transform_y(double yt_i){
		return yt_i; //do nothing
	}	
}
