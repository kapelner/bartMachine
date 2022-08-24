package bartMachine;

import java.io.Serializable;

import it.unimi.dsi.fastutil.ints.IntArrayList;

/**
 * This portion of the code that performs the posterior sampling
 * in the Gibbs sampler except for the Metropolis-Hastings tree search step
 * 
 * @author Adam Kapelner and Justin Bleich
 */
@SuppressWarnings("serial")
public abstract class bartMachine_f_gibbs_internal extends bartMachine_e_gibbs_base implements Serializable{

	/**
	 * Assigns a value to this terminal node; the value is the prediction at this corner of X space.
	 * 
	 * @param node		The node to assign a prediction guess for
	 * @param sigsq		The current guess of the variance of the model errors
	 */
	protected void assignLeafValsBySamplingFromPosteriorMeanAndSigsqAndUpdateYhats(bartMachineTreeNode node, double sigsq) {
		if (node.isLeaf){
			//update ypred
			node.posterior_var = calcLeafPosteriorVar(node, sigsq);
			//draw from posterior distribution
			node.posterior_mean = calcLeafPosteriorMean(node, sigsq, node.posterior_var);
			node.y_pred = StatToolbox.sample_from_norm_dist(node.posterior_mean, node.posterior_var);
			if (node.y_pred == StatToolbox.ILLEGAL_FLAG){				
				node.y_pred = 0.0; //this could happen on an empty node
				System.err.println("ERROR assignLeafFINAL " + node.y_pred + " (sigsq = " + sigsq + ")");
			}
			//now update yhats
			node.updateYHatsWithPrediction();
		}
		else {
			assignLeafValsBySamplingFromPosteriorMeanAndSigsqAndUpdateYhats(node.left, sigsq);
			assignLeafValsBySamplingFromPosteriorMeanAndSigsqAndUpdateYhats(node.right, sigsq);
		}
	}

	/**
	 * Calculate the posterior mean of the prediction distribution at a certain node
	 * 
	 * @param node				The node we are calculating the posterior mean for
	 * @param sigsq				The current guess of the variance of the model errors
	 * @param posterior_var		The posterior variance of the prediction distribution at this node
	 * @return					The posterior mean for this node
	 */
	protected double calcLeafPosteriorMean(bartMachineTreeNode node, double sigsq, double posterior_var) {
		node.y_avg = node.avgResponse();
		return (hyper_mu_mu / hyper_sigsq_mu + node.n_eta / sigsq * node.avgResponse()) * posterior_var;
	}

	/**
	 * Calculate the posterior variance of the prediction distribution at a certain node
	 * 
	 * @param node		The node we are calculating the posterior variance for
	 * @param sigsq		The current guess of the variance of the model errors
	 * @return			The posterior variance for this node
	 */
	protected double calcLeafPosteriorVar(bartMachineTreeNode node, double sigsq) {
		return 1 / (1 / hyper_sigsq_mu + node.n_eta / sigsq);
	}
	
	/**
	 * Draws one variance from the posterior distribution
	 * 
	 * @param sample_num	The current sample number of the Gibbs sampler
	 * @param es			The vector of residuals at this point in the Gibbs chain
	 */
	protected double drawSigsqFromPosterior(int sample_num, double[] es) {
		//first calculate the SSE
		double sse = 0;
		for (double e : es){
			sse += e * e; 
		}
		//we're sampling from sigsq ~ InvGamma((nu + n) / 2, 1/2 * (sum_i error^2_i + lambda * nu))
		//which is equivalent to sampling (1 / sigsq) ~ Gamma((nu + n) / 2, 2 / (sum_i error^2_i + lambda * nu))
		return StatToolbox.sample_from_inv_gamma((hyper_nu + es.length) / 2, 2 / (sse + hyper_nu * hyper_lambda));
	}

	/**
	 * Pick a random predictor from the set of valid, possible predictors at this point in the tree
	 * 
	 * @param node	The node to pick a predictor for
	 * @return		The index of the picked predictor
	 */
	public int pickRandomPredictorThatCanBeAssigned(bartMachineTreeNode node){
        IntArrayList predictors = node.predictorsThatCouldBeUsedToSplitAtNode();
        return predictors.getInt((int)Math.floor(StatToolbox.rand() * pAdj(node)));
	}
	
	/**
	 * Gets the total number of predictors that could be used for rules at this point in the tree
	 * 
	 * @param node 	The node to calculate the number of predictors for
	 * @return		The number of valid predictors
	 */
	public double pAdj(bartMachineTreeNode node){
		if (node.padj == null){
			node.padj = node.predictorsThatCouldBeUsedToSplitAtNode().size();
		}
		return node.padj;
	}	
}
