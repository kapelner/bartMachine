package bartMachine;

import java.util.Arrays;

/**
 * This portion of the code performs the evaluation / prediction on the BART model
 * 
 * @author Adam Kapelner and Justin Bleich
 */
public abstract class bartMachine_h_eval extends bartMachine_g_mh {
	
	/**
	 * The default BART evaluation of a new observations is done via sample average of the 
	 * posterior predictions. Other functions can be used here such as median, mode, etc.
	 * 
	 * @param record				The observation to be evaluated / predicted
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 */
	public double Evaluate(double[] record, int num_cores_evaluate) {		
		return EvaluateViaSampAvg(record, num_cores_evaluate);
	}		
	
	/**
	 * Evaluates a new observations via sample average of the posterior predictions.
	 * 
	 * @param record				The observation to be evaluated / predicted
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 */
	public double EvaluateViaSampAvg(double[] record, int num_cores_evaluate) {	
		return StatToolbox.sample_average(getGibbsSamplesForPrediction(record, num_cores_evaluate));
	}
	
	/**
	 * Evaluates a new observations via sample median of the posterior predictions.
	 * 
	 * @param record				The observation to be evaluated / predicted
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 */
	public double EvaluateViaSampMed(double[] record, int num_cores_evaluate) {	
		return StatToolbox.sample_median(getGibbsSamplesForPrediction(record, num_cores_evaluate));
	}

	/**
	 * For each sum-of-trees in each psoterior of the Gibbs samples, evaluate / predict these new records by summing over
	 * the prediction for each tree
	 * 
	 * @param record				The observation to be evaluated / predicted
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 * @return						The predicted values as the result of the sum over many trees for all posterior gibbs samples
	 */
	protected double[] getGibbsSamplesForPrediction(double[] record, int num_cores_evaluate){
		//the results for each of the gibbs samples
		double[] y_gibbs_samples = new double[numSamplesAfterBurningAndThinning()];	
		for (int g = 0; g < numSamplesAfterBurningAndThinning(); g++){
			bartMachineTreeNode[] bart_trees = gibbs_samples_of_bart_trees_after_burn_in[g];
			double yt_g = 0;
			for (bartMachineTreeNode tree : bart_trees){ //sum of trees right?
				yt_g += tree.Evaluate(record);
			}			
			y_gibbs_samples[g] = un_transform_y(yt_g);
		}
		return y_gibbs_samples;
	}
	
	/**
	 * For each sum-of-trees in each psoterior of the Gibbs samples, evaluate / predict these new records by summing over
	 * the prediction for each tree then order these by value and create an uncertainty interval
	 * 
	 * @param record				The observation for which to create an uncertainty interval		
	 * @param coverage				The percent coverage (between 0 and 1)
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 * @return						A tuple which is the lower value in the interval followed by the higher value
	 */
	protected double[] getPostPredictiveIntervalForPrediction(double[] record, double coverage, int num_cores_evaluate){
		//get all gibbs samples sorted
		double[] y_gibbs_samples_sorted = getGibbsSamplesForPrediction(record, num_cores_evaluate);
		Arrays.sort(y_gibbs_samples_sorted);
		
		//calculate index of the CI_a and CI_b
		int n_bottom = (int)Math.round((1 - coverage) / 2 * y_gibbs_samples_sorted.length) - 1; //-1 because arrays start at zero
		int n_top = (int)Math.round(((1 - coverage) / 2 + coverage) * y_gibbs_samples_sorted.length) - 1; //-1 because arrays start at zero
		double[] conf_interval = {y_gibbs_samples_sorted[n_bottom], y_gibbs_samples_sorted[n_top]};
		return conf_interval;
	}
	
	/**
	 * For each sum-of-trees in each psoterior of the Gibbs samples, evaluate / predict these new records by summing over
	 * the prediction for each tree then order these by value and create a 95% uncertainty interval
	 * 
	 * @param record				The observation for which to create an uncertainty interval
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 * @return						A tuple which is the lower value in the 95% interval followed by the higher value
	 */
	protected double[] get95PctPostPredictiveIntervalForPrediction(double[] record, int num_cores_evaluate){
		return getPostPredictiveIntervalForPrediction(record, 0.95, num_cores_evaluate);
	}	
}
