package bartMachine;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import OpenSourceExtensions.UnorderedPair;
import it.unimi.dsi.fastutil.doubles.DoubleArrayList;
import it.unimi.dsi.fastutil.ints.IntOpenHashSet;

/**
 * This class handles the parallelization of many Gibbs chains over many CPU cores
 * to create one BART regression model. It also handles all operations on the completed model.
 * 
 * @author Adam Kapelner and Justin Bleich
 */
@SuppressWarnings("serial")
public class bartMachineRegressionMultThread extends Classifier implements Serializable {
	
	/** the number of CPU cores to build many different Gibbs chain within a BART model */
	protected int num_cores = 1; //default
	/** the number of trees in this BART model on all Gibbs chains */
	protected int num_trees = 50; //default
	
	/** the collection of <code>num_cores</code> BART models which will run separate Gibbs chains */
	protected bartMachineRegression[] bart_gibbs_chain_threads;
	/** this is the combined gibbs samples after burn in from all of the <code>num_cores</code> chains */
	protected bartMachineTreeNode[][] gibbs_samples_of_bart_trees_after_burn_in;
	
	/** the estimate of some upper limit of the variance of the response which is usually the MSE from a a linear regression */
	private Double sample_var_y;
	/** the number of burn-in samples in each Gibbs chain */
	protected int num_gibbs_burn_in = 250; //default
	/** the total number of gibbs samples where each chain gets a number of burn-in and then the difference from the total divided by <code>num_cores</code> */ 
	protected int num_gibbs_total_iterations = 1250; //default
	/** the total number of Gibbs samples for each of the <code>num_cores</code> chains */
	protected int total_iterations_multithreaded;

	/** The probability vector that samples covariates for selecting split rules */
	protected double[] cov_split_prior;
	/** A hyperparameter that controls how easy it is to grow new nodes in a tree independent of depth */
	protected Double alpha = 0.95;
	/** A hyperparameter that controls how easy it is to grow new nodes in a tree dependent on depth which makes it more difficult as the tree gets deeper */
	protected Double beta = 2.0;
	/** this controls where to set <code>hyper_sigsq_mu</code> by forcing the variance to be this number of standard deviations on the normal CDF */
	protected Double hyper_k = 2.0;
	/** At a fixed <code>hyper_nu</code>, this controls where to set <code>hyper_lambda</code> by forcing q proportion to be at that value in the inverse gamma CDF */
	protected Double hyper_q = 0.9;
	/** half the shape parameter and half the multiplicand of the scale parameter of the inverse gamma prior on the variance */
	protected Double hyper_nu = 3.0;
	/** the hyperparameter of the probability of picking a grow step during the Metropolis-Hastings tree proposal */
	protected Double prob_grow = 2.5 / 9.0;
	/** the hyperparameter of the probability of picking a prune step during the Metropolis-Hastings tree proposal */
	protected Double prob_prune = 2.5 / 9.0;
	
	/** should we print select messages to the screen */
	protected boolean verbose = true;
	/** 
	 * whether or not we use the memory cache feature
	 * 
	 * @see Section 3.1 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected boolean mem_cache_for_speed = true;
	/** saves indices in nodes (useful for computing weights) */
	protected boolean flush_indices_to_save_ram = true;
	private boolean tree_illust;
	private HashMap<Integer, IntOpenHashSet> interaction_constraints;

	
	/** the default constructor sets the number of total iterations each Gibbs chain is charged with sampling */
	public bartMachineRegressionMultThread(){	
		setNumGibbsTotalIterations(num_gibbs_total_iterations);
	}
	
	/**
	 * This is a simple setter for the number of total Gibbs samples and
	 * it also sets the total iterations per Gibbs chain running on each CPU
	 * core (see formula in the code)
	 * 
	 * @param num_gibbs_total_iterations	The number of total Gibbs iterations to set
	 */
	public void setNumGibbsTotalIterations(int num_gibbs_total_iterations){
		this.num_gibbs_total_iterations = num_gibbs_total_iterations;
		total_iterations_multithreaded = num_gibbs_burn_in + (int)Math.ceil((num_gibbs_total_iterations - num_gibbs_burn_in) / (double) num_cores);
	}
	
	/** The number of samples after burning is simply the title minus the burn-in */
	public int numSamplesAfterBurning(){
		return num_gibbs_total_iterations - num_gibbs_burn_in;
	}
	
	/** Set up an array of regression BARTs with length equal to <code>num_cores</code>, the number of CPU cores requested */
	protected void SetupBARTModels() {
		bart_gibbs_chain_threads = new bartMachineRegression[num_cores];
		for (int t = 0; t < num_cores; t++){
			bartMachineRegression bart = new bartMachineRegression();
			SetupBartModel(bart, t);
		}
	}

	/**
	 * Initialize one of the <code>num_cores</code> BART models by setting
	 * all its custom parameters
	 * 
	 * @param bart		The BART model to initialize
	 * @param t			The number of the core this BART model corresponds to
	 */
	protected void SetupBartModel(bartMachineRegression bart, int t) {
		bart.setVerbose(verbose);
		//now set specs on each of the bart models
		bart.num_trees = num_trees;
		bart.num_gibbs_total_iterations = total_iterations_multithreaded;
		bart.num_gibbs_burn_in = num_gibbs_burn_in;
		bart.sample_var_y = sample_var_y;		
		//now some hyperparams
		bart.setAlpha(alpha);
		bart.setBeta(beta);
		bart.setK(hyper_k);
		bart.setProbGrow(prob_grow);
		bart.setProbPrune(prob_prune);
		//set thread num and data
		bart.setThreadNum(t);
		bart.setTotalNumThreads(num_cores);
		bart.setMemCacheForSpeed(mem_cache_for_speed);
		bart.setFlushIndicesToSaveRAM(flush_indices_to_save_ram);
		
		//set features
		if (cov_split_prior != null){
			bart.setCovSplitPrior(cov_split_prior);
		}
		//set interaction constraints
		if (interaction_constraints != null) {
			bart.setInteractionConstraints(interaction_constraints);
		}
		//do special stuff for regression model
		if (!(bart instanceof bartMachineClassification)){
			bart.setNu(hyper_nu);		
			bart.setQ(hyper_q);
		}
		//once the params are set, now you can set the data
		bart.setData(X_y);
		bart.tree_illust = tree_illust;
		bart_gibbs_chain_threads[t] = bart;
	}
	
	/**
	 * Takes a library of standard normal samples provided external and caches them
	 * 
	 * @param norm_samples	The externally provided cache
	 */
	public void setNormSamples(double[] norm_samples){
		bartMachine_b_hyperparams.samps_std_normal = norm_samples;
		bartMachine_b_hyperparams.samps_std_normal_length = norm_samples.length;
	}
	
	/**
	 * Takes a library of chi-squared samples provided external and caches them
	 * 
	 * @param gamma_samples	The externally provided cache
	 */
	public void setGammaSamples(double[] gamma_samples){
		bartMachine_b_hyperparams.samps_chi_sq_df_eq_nu_plus_n = gamma_samples;
		bartMachine_b_hyperparams.samps_chi_sq_df_eq_nu_plus_n_length = gamma_samples.length;
	}

	/** This function actually initiates the Gibbs sampling to build all the BART models */
	public void Build() {
		SetupBARTModels();
		//run a build on all threads
		long t0 = System.currentTimeMillis();
		if (verbose){
			System.out.println("building BART " + (mem_cache_for_speed ? "with" : "without") + " mem-cache speedup...");
		}
		BuildOnAllThreads();
		long t1 = System.currentTimeMillis();
		if (verbose){
			System.out.println("done building BART in " + ((t1 - t0) / 1000.0) + " sec \n");
		}
		//once it's done, now put together the chains
		ConstructBurnedChainForTreesAndOtherInformation();
	}	
	
	/** Create a post burn-in chain for ease of manipulation later */
	protected void ConstructBurnedChainForTreesAndOtherInformation() {
		gibbs_samples_of_bart_trees_after_burn_in = new bartMachineTreeNode[numSamplesAfterBurning()][num_trees];

		if (verbose){
			System.out.print("burning and aggregating chains from all threads... ");
		}
		//go through each thread and get the tail and put them together
		for (int t = 0; t < num_cores; t++){
			bartMachineRegression bart_model = bart_gibbs_chain_threads[t];
			for (int i = num_gibbs_burn_in; i < total_iterations_multithreaded; i++){
				int offset = t * (total_iterations_multithreaded - num_gibbs_burn_in);
				int g = offset + (i - num_gibbs_burn_in);
				if (g >= numSamplesAfterBurning()){
					break;
				}
				gibbs_samples_of_bart_trees_after_burn_in[g] = bart_model.gibbs_samples_of_bart_trees[i];
			}			
		}
		if (verbose){
			System.out.print("done\n");
		}
	}

	/** This is the core of BART's parallelization for model creation: build one BART model on each CPU core in parallel */
	private void BuildOnAllThreads(){
		ExecutorService bart_gibbs_chain_pool = Executors.newFixedThreadPool(num_cores);
		for (int t = 0; t < num_cores; t++){
			final int tf = t;
	    	bart_gibbs_chain_pool.execute(new Runnable(){
				public void run() {
					bart_gibbs_chain_threads[tf].Build();
				}
			});
		}
		bart_gibbs_chain_pool.shutdown();
		try {	         
	         bart_gibbs_chain_pool.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS); //effectively infinity
	    } catch (InterruptedException ignored){}		
	}

	protected boolean[][][][] getNodePredictionTrainingIndicies(double[][] records){
		if (records == null) {
			records = new double[n][p];
			for (int i = 0; i < n; i++) {
				records[i] = X_y.get(i); //this will include y but it is never used in evaluation
			}		 	
		}
		int n_star = records.length;
		int num_samples_after_burn_in = numSamplesAfterBurning();
		
		boolean[][][][] node_prediction_training_indices = new boolean[n_star][num_samples_after_burn_in][num_trees][n];
		
		for (int i_star = 0; i_star < n_star; i_star++) {
			for (int g = 0; g < num_samples_after_burn_in; g++){				
				bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
				for (int m = 0; m < num_trees; m++){					
					for (int i : trees[m].EvaluateNode(records[i_star]).indicies) {
						node_prediction_training_indices[i_star][g][m][i] = true;
					}
				}
			}
		}
		return node_prediction_training_indices;
	}
	

	protected double[][] getProjectionWeights(double[][] records){
		if (records == null) {
			records = new double[n][p];
			for (int i = 0; i < n; i++) {
				records[i] = X_y.get(i); //this will include y but it is never used in evaluation
			}		 	
		}
		int n_star = records.length;
		int num_samples_after_burn_in = numSamplesAfterBurning();
		
//		double[][] all_gibbs_samples = getGibbsSamplesForPrediction(records, 1);
		
		boolean[][][][] node_prediction_training_indices = getNodePredictionTrainingIndicies(records);
		
		double[][] sample_weights = new double[n_star][];
		
		for (int i_star = 0; i_star < n_star; i_star++) {
//			double y_hat_i_star = StatToolbox.sample_average(all_gibbs_samples[i_star]);
			double[] sample_weights_i_star = new double[n];
			for (int g = 0; g < num_samples_after_burn_in; g++){
//				bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];

				for (int m = 0; m < num_trees; m++){
					boolean[] indicies_m_i_star = node_prediction_training_indices[i_star][g][m];
					
					int n_g_m = Tools.sum_array(indicies_m_i_star);
					for (int i = 0; i < n; i++) {
						sample_weights_i_star[i] += (indicies_m_i_star[i] ? 1 : 0) / ((double)n_g_m * num_trees);
					}					
				}
			}
			//we need to scale by 1 / G
			for (int i = 0; i < n; i++) {
				sample_weights_i_star[i] *= 1 / (double)num_samples_after_burn_in;
			}
			
			sample_weights[i_star] = sample_weights_i_star;
		}
		return sample_weights;
	}

	/**
	 * Return the predictions from each tree for each burned-in Gibbs sample
	 * 
	 * @param records				the observations / records for which to return predictions
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation	
	 * @return						Predictions for all records further indexed by Gibbs sample
	 */
	protected double[][] getGibbsSamplesForPrediction(final double[][] records, final int num_cores_evaluate){
		final int num_samples_after_burn_in = numSamplesAfterBurning();
		final bartMachineRegression first_bart = bart_gibbs_chain_threads[0];
		
		final int n = records.length;
		final double[][] y_hats = new double[n][records[0].length];
		
		//this is really ugly, but it's faster (we've checked in a Profiler)
		if (num_cores_evaluate == 1){
			for (int i = 0; i < n; i++){
				double[] y_gibbs_samples = new double[num_samples_after_burn_in];
				for (int g = 0; g < num_samples_after_burn_in; g++){
					bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
					double yt_i = 0;
					for (int m = 0; m < num_trees; m++){ 
						yt_i += trees[m].Evaluate(records[i]); //sum of trees, right?
					}
					//just make sure we switch it back to really what y is for the user
					y_gibbs_samples[g] = first_bart.un_transform_y(yt_i);
				}
				y_hats[i] = y_gibbs_samples;
			}			
		}
		else {
			Thread[] fixed_thread_pool = new Thread[num_cores_evaluate];
			for (int t = 0; t < num_cores_evaluate; t++){
				final int final_t = t;
				Thread thread = new Thread(){
					public void run(){
						for (int i = 0; i < n; i++){
							if (i % num_cores_evaluate == final_t){
								double[] y_gibbs_samples = new double[num_samples_after_burn_in];
								for (int g = 0; g < num_samples_after_burn_in; g++){									
									bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
									double yt_i = 0;
									for (int m = 0; m < num_trees; m++){ //sum of trees right?
										yt_i += trees[m].Evaluate(records[i]);
									}
									//just make sure we switch it back to really what y is for the user
									y_gibbs_samples[g] = first_bart.un_transform_y(yt_i);	
								}
								y_hats[i] = y_gibbs_samples;
							}
						}
					}
				};
				thread.start();
				fixed_thread_pool[t] = thread;
			}
			for (int t = 0; t < num_cores_evaluate; t++){
				try {
					fixed_thread_pool[t].join();
				} catch (InterruptedException e) {}
			}
		}
		return y_hats;
	}

	/**
	 * Using the {@link #getGibbsSamplesForPrediction} function, return a credible interval at a specified
	 * level of coverage
	 * 
	 * @param record				The record for which to obtain an interval
	 * @param coverage				The desired coverage
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 * @return						A double pair which represents the upper and lower bound of coverage
	 */
	protected double[] getPostPredictiveIntervalForPrediction(double[] record, double coverage, int num_cores_evaluate){
		double[][] data = new double[1][record.length];
		data[0] = record;		
		//get all gibbs samples sorted
		double[][] y_gibbs_samples_sorted_matrix = getGibbsSamplesForPrediction(data, num_cores_evaluate);
		double[] y_gibbs_samples_sorted = y_gibbs_samples_sorted_matrix[0];
		Arrays.sort(y_gibbs_samples_sorted);
		
		//calculate index of the CI_a and CI_b
		int n_bottom = (int)Math.round((1 - coverage) / 2 * y_gibbs_samples_sorted.length) - 1; //-1 because arrays start at zero
		int n_top = (int)Math.round(((1 - coverage) / 2 + coverage) * y_gibbs_samples_sorted.length) - 1; //-1 because arrays start at zero
		double[] conf_interval = {y_gibbs_samples_sorted[n_bottom], y_gibbs_samples_sorted[n_top]};
		return conf_interval;
	}
	
	/**
	 * Using the {@link #getPostPredictiveIntervalForPrediction} function, return a 95\% credible interval 
	 * 
	 * @param record				The record for which to obtain an interval
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 * @return						A double pair which represents the upper and lower bound of coverage
	 */
	protected double[] get95PctPostPredictiveIntervalForPrediction(double[] record, int num_cores_evaluate){
		return getPostPredictiveIntervalForPrediction(record, 0.95, num_cores_evaluate);
	}	
	
	public double[] getGibbsSamplesSigsqs(){
		DoubleArrayList sigsqs_to_export = new DoubleArrayList(num_gibbs_total_iterations);
		for (int t = 0; t < num_cores; t++){
			DoubleArrayList sigsqs_to_export_by_thread = new DoubleArrayList(bart_gibbs_chain_threads[t].getGibbsSamplesSigsqs());
			if (t == 0){
				sigsqs_to_export.addAll(sigsqs_to_export_by_thread);
			}
			else {
				sigsqs_to_export.addAll(sigsqs_to_export_by_thread.subList(num_gibbs_burn_in, total_iterations_multithreaded));
			}
		}
		return sigsqs_to_export.elements();
	}
	
	/**
	 * Returns a record of Metropolis-Hastings acceptances or rejections for all trees during burn-in
	 * 
	 * @return	Yes/No's for acceptances for all Gibbs samples further indexed by tree
	 */
	public boolean[][] getAcceptRejectMHsBurnin(){
		boolean[][] accept_reject_mh_first_thread = bart_gibbs_chain_threads[0].getAcceptRejectMH();
		boolean[][] accept_reject_mh_burn_ins = new boolean[num_gibbs_burn_in][num_trees];
		for (int g = 1; g < num_gibbs_burn_in + 1; g++){
			accept_reject_mh_burn_ins[g - 1] = accept_reject_mh_first_thread[g];
		}
		return accept_reject_mh_burn_ins;
	}
	
	/**
	 * Returns a record of Metropolis-Hastings acceptances or rejections for all trees after burn-in
	 * 
	 * @param thread_num	Which CPU core's acceptance / rejection record should be returned
	 * @return				Yes/No's for acceptances for all Gibbs samples further indexed by tree
	 */
	public boolean[][] getAcceptRejectMHsAfterBurnIn(int thread_num){
		boolean[][] accept_reject_mh_by_core = bart_gibbs_chain_threads[thread_num - 1].getAcceptRejectMH();
		boolean[][] accept_reject_mh_after_burn_ins = new boolean[total_iterations_multithreaded - num_gibbs_burn_in][num_trees];
		for (int g = num_gibbs_burn_in; g < total_iterations_multithreaded; g++){
			accept_reject_mh_after_burn_ins[g - num_gibbs_burn_in] = accept_reject_mh_by_core[g];
		}
		return accept_reject_mh_after_burn_ins;
	}	

	/**
	 * Return the number of times each of the attributes were used during the construction of the sum-of-trees
	 * by Gibbs sample.
	 * 
	 * @param type	Either "splits" or "trees" ("splits" means total number and "trees" means sum of binary values of whether or not it has appeared in the tree)
	 * @return		The counts for all Gibbs samples further indexed by the attribute 1, ..., p
	 */
	public int[][] getCountsForAllAttribute(final String type) {
		final int[][] variable_counts_all_gibbs = new int[num_gibbs_total_iterations - num_gibbs_burn_in][p];		
		
		for (int g = 0; g < num_gibbs_total_iterations - num_gibbs_burn_in; g++){
			final bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
			int[] variable_counts_one_gibbs = new int[p];
			for (bartMachineTreeNode tree : trees){	
				if (type.equals("splits")){
					variable_counts_one_gibbs = Tools.add_arrays(variable_counts_one_gibbs, tree.attributeSplitCounts());
				}
				else if (type.equals("trees")){
					variable_counts_one_gibbs = Tools.binary_add_arrays(variable_counts_one_gibbs, tree.attributeSplitCounts());
				}				
				
			}
			variable_counts_all_gibbs[g] = variable_counts_one_gibbs;
		}		
		return variable_counts_all_gibbs;
	}
	
	/**
	 * Return the proportion of times each of the attributes were used (count over total number of splits) 
	 * during the construction of the sum-of-trees by Gibbs sample.
	 * 
	 * @param type	Either "splits" or "trees" ("splits" means total number and "trees" means sum of binary values of whether or not it has appeared in the tree)
	 * @return		The proportion of splits for all Gibbs samples further indexed by the attribute 1, ..., p
	 */
	public double[] getAttributeProps(final String type) {
		int[][] variable_counts_all_gibbs = getCountsForAllAttribute(type);
		double[] attribute_counts = new double[p];
		for (int g = 0; g < num_gibbs_total_iterations - num_gibbs_burn_in; g++){
			attribute_counts = Tools.add_arrays(attribute_counts, variable_counts_all_gibbs[g]);
		}
		Tools.normalize_array(attribute_counts); //will turn it into proportions
		return attribute_counts;
	}
	
	/**
	 * For all Gibbs samples after burn in, calculate the set of interaction counts (consider a split on x_j 
	 * and a daughter node splits on x_k and that would be considered an "interaction")
	 * 
	 * @return	A matrix of size p x p where the row is top split and the column is a bottom split. It is recommended to triangularize the matrix after ignoring the order.
	 */
	public int[][] getInteractionCounts(){
		int[][] interaction_count_matrix = new int[p][p];
		
		for (int g = 0; g < gibbs_samples_of_bart_trees_after_burn_in.length; g++){
			bartMachineTreeNode[] trees = gibbs_samples_of_bart_trees_after_burn_in[g];
			
			for (bartMachineTreeNode tree : trees){
				//get the set of pairs of interactions
				HashSet<UnorderedPair<Integer>> set_of_interaction_pairs = new HashSet<UnorderedPair<Integer>>(p * p);
				//find all interactions
				tree.findInteractions(set_of_interaction_pairs);
				//now tabulate these interactions in our count matrix
				for (UnorderedPair<Integer> pair : set_of_interaction_pairs){
					interaction_count_matrix[pair.getFirst()][pair.getSecond()]++; 
				}
			}	
		}
		
		return interaction_count_matrix;
	}

	/** Flush all unnecessary data from the Gibbs chains to conserve RAM */
	protected void FlushData() {
		for (int t = 0; t < num_cores; t++){
			bart_gibbs_chain_threads[t].FlushData();
		}
	}

	/**
	 * The default BART evaluation of a new observations is done via sample average of the 
	 * posterior predictions. Other functions can be used here such as median, mode, etc. 
	 * Default is to use one CPU core.
	 * 
	 * @param record				The observation to be evaluated / predicted
	 */
	public double Evaluate(double[] record) {	
		return EvaluateViaSampAvg(record, 1);
	}	
	
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
		double[][] data = new double[1][record.length];
		data[0] = record;
		double[][] gibbs_samples = getGibbsSamplesForPrediction(data, num_cores_evaluate);
		return StatToolbox.sample_average(gibbs_samples[0]);
	}
	
	/**
	 * Evaluates a new observations via sample median of the posterior predictions.
	 * 
	 * @param record				The observation to be evaluated / predicted
	 * @param num_cores_evaluate	The number of CPU cores to use during evaluation
	 */
	public double EvaluateViaSampMed(double[] record, int num_cores_evaluate) {	
		double[][] data = new double[1][record.length];
		data[0] = record;
		double[][] gibbs_samples = getGibbsSamplesForPrediction(data, num_cores_evaluate);
		return StatToolbox.sample_median(gibbs_samples[0]);
	}	
	
	/**
	 * After burn in, find the depth (greatest generation of a terminal node) of each tree for each Gibbs sample
	 * 
	 * @param thread_num	which CPU core (which Gibbs chain) to return results for
	 * @return				for each Gibbs chain return a vector of depths for all <code>num_trees</code> chains
	 */
	public int[][] getDepthsForTreesInGibbsSampAfterBurnIn(int thread_num){
		return bart_gibbs_chain_threads[thread_num - 1].getDepthsForTrees(num_gibbs_burn_in, total_iterations_multithreaded);
	}	
	
	/**
	 * After burn in, return the number of total nodes (internal plus terminal) of each tree for each Gibbs sample
	 * 
	 * @param thread_num	which CPU core (which Gibbs chain) to return results for
	 * @return				for each Gibbs chain return a vector of number of nodes for all <code>num_trees</code> chains
	 */
	public int[][] getNumNodesAndLeavesForTreesInGibbsSampAfterBurnIn(int thread_num){
		return bart_gibbs_chain_threads[thread_num - 1].getNumNodesAndLeavesForTrees(num_gibbs_burn_in, total_iterations_multithreaded);
	}
	
	public void setData(ArrayList<double[]> X_y){
		this.X_y = X_y;
	 	n = X_y.size();
	 	p = X_y.get(0).length - 1;
	}
	
	
	public void printTreeIllustations(){
		tree_illust = true;
	}
	
	public void setCovSplitPrior(double[] cov_split_prior){
		this.cov_split_prior = cov_split_prior;
	}
	
	public void intializeInteractionConstraints(int num_constraints) {
		interaction_constraints = new HashMap<Integer, IntOpenHashSet>(num_constraints);
	}
	public void addInteractionConstraint(int constrained_feature, int[] constrained_features) {
		if (interaction_constraints.get(constrained_feature) == null) {
			interaction_constraints.put(constrained_feature, new IntOpenHashSet());
		}
		IntOpenHashSet current_constrained_features = interaction_constraints.get(constrained_feature);
		for (int j : constrained_features) {
			current_constrained_features.add(j);
		}		
	}
	
	public void setNumGibbsBurnIn(int num_gibbs_burn_in){
		this.num_gibbs_burn_in = num_gibbs_burn_in;
	}	

	public void setNumTrees(int num_trees){
		this.num_trees = num_trees;
	}
	
	public void setSampleVarY(double sample_var_y){
		this.sample_var_y = sample_var_y;
	}
	
	public void setAlpha(double alpha){
		this.alpha = alpha;
	}
	
	public void setBeta(double beta){
		this.beta = beta;
	}	
	
	public void setK(double hyper_k) {
		this.hyper_k = hyper_k;
	}

	public void setQ(double hyper_q) {
		this.hyper_q = hyper_q;
	}

	public void setNU(double hyper_nu) {
		this.hyper_nu = hyper_nu;
	}	
	
	
	public void setProbGrow(double prob_grow) {
		this.prob_grow = prob_grow;
	}

	public void setProbPrune(double prob_prune) {
		this.prob_prune = prob_prune;
	}
	
	public void setVerbose(boolean verbose){
		this.verbose = verbose;
	}

	public void setSeed(int seed){
		StatToolbox.setSeed(seed);
	}	
	
	public void setNumCores(int num_cores){
		this.num_cores = num_cores;
	}
	
	public void setMemCacheForSpeed(boolean mem_cache_for_speed){
		this.mem_cache_for_speed = mem_cache_for_speed;
	}
	
	public void setFlushIndicesToSaveRAM(boolean flush_indices_to_save_ram) {
		this.flush_indices_to_save_ram = flush_indices_to_save_ram;
	}
	
	/** Must be implemented, but does nothing */
	public void StopBuilding() {}	
	
	public bartMachineTreeNode[] extractRawNodeInformation(int g){
		bartMachineTreeNode[] all_trees = new bartMachineTreeNode[num_trees];
		for (int m = 0; m < num_trees; m++) {
			all_trees[m] = gibbs_samples_of_bart_trees_after_burn_in[g][m];
		}
		return all_trees;
	}
}
