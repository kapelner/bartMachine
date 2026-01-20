package bartMachine;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.Future;

import OpenSourceExtensions.StatUtil;

/**
 * This class handles the parallelization of many Gibbs chains over many CPU cores
 * to create one BART regression model. It also handles all operations on the completed model.
 * @author Adam Kapelner and Justin Bleich
 * 
 */
@SuppressWarnings("serial")
public class bartMachineClassificationMultThread extends bartMachineRegressionMultThread implements Serializable{
	
	/** The default value of the <code>classification_rule</code> */
	private static double DEFAULT_CLASSIFICATION_RULE = 0.5;
	/** The value of the classification rule which if the probability estimate of Y = 1 is greater than, we predict 1 */
	private double classification_rule;

	/** Set up an array of binary classification BARTs with length equal to <code>num_cores</code>, the number of CPU cores requested */
	protected void SetupBARTModels() {
		bart_gibbs_chain_threads = new bartMachineClassification[num_cores];
		for (int t = 0; t < num_cores; t++){
			SetupBartModel(new bartMachineClassification(), t);
		}
		classification_rule = DEFAULT_CLASSIFICATION_RULE;
	}
	
	/**
	 * Predicts the best guess of the class for an observation
	 * 
	 * @param record				The record who's class we wish to predict
	 * @param num_cores_evaluate	The number of CPU cores to use during this operation
	 * @return						The best guess of the class based on the probability estimate evaluated against the {@link classification_rule}
	 */
	public double Evaluate(double[] record, int num_cores_evaluate) {
		return EvaluateViaSampAvg(record, num_cores_evaluate) > classification_rule ? 1 : 0;
	}	
	
	/**
	 * This returns the Gibbs sample predictions for all trees and all posterior samples.
	 * This differs from the parent implementation because we convert the response value to
	 * a probability estimate using the normal CDF.
	 * 
	 *  @param data					The data for which to generate predictions
	 *  @param num_cores_evaluate	The number of CPU cores to use during this operation
	 *  @return						The predictions as a vector of size number of posterior samples of vectors of size number of trees
	 */
	protected double[][] getGibbsSamplesForPrediction(final double[][] data, final int num_cores_evaluate){
		final int num_samples_after_burn_in = numSamplesAfterBurning();
		final int n_star = data.length;
		final double[][] y_gibbs_samples_probs = new double[n_star][num_samples_after_burn_in];
		if (n_star == 0 || num_samples_after_burn_in <= 0){
			return y_gibbs_samples_probs;
		}
		
		int[] allIndices = new int[n_star];
		for (int i = 0; i < n_star; i++){
			allIndices[i] = i;
		}
		
		if (num_cores_evaluate == 1){
			double[] yt_g = new double[n_star];
			for (int g = 0; g < num_samples_after_burn_in; g++){
				Arrays.fill(yt_g, 0.0);
				bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
				for (int m = 0; m < num_trees; m++){
					trees[m].evaluateBatch(data, allIndices, yt_g);
				}
				for (int i = 0; i < n_star; i++){
					y_gibbs_samples_probs[i][g] = StatUtil.normal_cdf(yt_g[i]);
				}
			}
		} else {
			int chunk = Math.max(1, num_samples_after_burn_in / num_cores_evaluate);
			ArrayList<Future<?>> futures = new ArrayList<>();
			try (var executor = java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor()) {
				for (int g_start = 0; g_start < num_samples_after_burn_in; g_start += chunk){
					final int start = g_start;
					final int end = Math.min(num_samples_after_burn_in, g_start + chunk);
					futures.add(executor.submit(() -> {
						double[] yt_g = new double[n_star];
						for (int g = start; g < end; g++){
							Arrays.fill(yt_g, 0.0);
							int[] localIndices = allIndices.clone();
							bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
							for (int m = 0; m < num_trees; m++){
								trees[m].evaluateBatch(data, localIndices, yt_g);
							}
							for (int i = 0; i < n_star; i++){
								y_gibbs_samples_probs[i][g] = StatUtil.normal_cdf(yt_g[i]);
							}
						}
						return null;
					}));
				}
				for (Future<?> future : futures){
					future.get();
				}
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
		return y_gibbs_samples_probs;
	}	
	
	@Override
	public double[] getPosteriorMeanForPrediction(final double[][] records, final int num_cores_evaluate){
		final int num_samples_after_burn_in = numSamplesAfterBurning();
		final int n_star = records.length;
		final double[] p_hat_sum = new double[n_star];
		if (n_star == 0 || num_samples_after_burn_in <= 0){
			return p_hat_sum;
		}

		int[] allIndices = new int[n_star];
		for (int i = 0; i < n_star; i++){
			allIndices[i] = i;
		}

		if (num_cores_evaluate == 1){
			double[] yt_g = new double[n_star];
			for (int g = 0; g < num_samples_after_burn_in; g++){
				Arrays.fill(yt_g, 0.0);
				bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
				for (int m = 0; m < num_trees; m++){
					trees[m].evaluateBatch(records, allIndices, yt_g);
				}
				for (int i = 0; i < n_star; i++){
					p_hat_sum[i] += StatUtil.normal_cdf(yt_g[i]);
				}
			}
		} else {
			int chunk = Math.max(1, num_samples_after_burn_in / num_cores_evaluate);
			ArrayList<Future<double[]>> futures = new ArrayList<>();
			try (var executor = java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor()) {
				for (int g_start = 0; g_start < num_samples_after_burn_in; g_start += chunk){
					final int start = g_start;
					final int end = Math.min(num_samples_after_burn_in, g_start + chunk);
					futures.add(executor.submit(() -> {
						double[] localSum = new double[n_star];
						double[] yt_g = new double[n_star];
						for (int g = start; g < end; g++){
							Arrays.fill(yt_g, 0.0);
							int[] localIndices = allIndices.clone();
							bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
							for (int m = 0; m < num_trees; m++){
								trees[m].evaluateBatch(records, localIndices, yt_g);
							}
							for (int i = 0; i < n_star; i++){
								localSum[i] += StatUtil.normal_cdf(yt_g[i]);
							}
						}
						return localSum;
					}));
				}
				for (Future<double[]> future : futures){
					double[] local = future.get();
					for (int i = 0; i < n_star; i++){
						p_hat_sum[i] += local[i];
					}
				}
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		for (int i = 0; i < n_star; i++){
			p_hat_sum[i] /= num_samples_after_burn_in;
		}
		return p_hat_sum;
	}
	
	public void setClassificationRule(double classification_rule) {
		this.classification_rule = classification_rule;
	}	
}
