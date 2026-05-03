package bartMachine;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;

import org.apache.commons.math.MathException;
import org.apache.commons.math.distribution.ChiSquaredDistributionImpl;

import it.unimi.dsi.fastutil.doubles.DoubleArrayList;
import it.unimi.dsi.fastutil.ints.IntOpenHashSet;

/**
 * This portion of the code controls hyperparameters for the BART
 * algorithm as well as properties and transformations of the response variable.
 * 
 * @author Adam Kapelner and Justin Bleich
 */
@SuppressWarnings("serial")
public abstract class bartMachine_b_hyperparams extends bartMachine_a_base implements Serializable{


	/** The static field that controls the bounds on the transformed y variable which is between negative and positive this value */
	protected static final double YminAndYmaxHalfDiff = 0.5;
	
	/** A cached library of chi-squared with degrees of freedom nu plus n (used for Gibbs sampling the variance) */
	protected static double[] samps_chi_sq_df_eq_nu_plus_n = {1, 2, 3, 4, 5}; //give a default for debugging in Java ONLY	
	/** The number of samples in the cached library of chi-squared values */
	protected static int samps_chi_sq_df_eq_nu_plus_n_length;
	/** A cached library of standard normal values (used for Gibbs sampling the posterior means of the terminal nodes) */
	protected static double[] samps_std_normal = {1, 2, 3, 4, 5}; //give a default for debugging in Java ONLY
	/** The number of samples in the cached library of standard normal values */
	protected static int samps_std_normal_length;
	
	/** the center of the prior of the terminal node prediction distribution */
	protected double hyper_mu_mu;
	/** the variance of the prior of the terminal node prediction distribution */
	protected double hyper_sigsq_mu;
	/** half the shape parameter and half the multiplicand of the scale parameter of the inverse gamma prior on the variance */
	protected double hyper_nu = 3.0;
	/** the multiplier of the scale parameter of the inverse gamma prior on the variance */
	protected double hyper_lambda;
	/** this controls where to set <code>hyper_sigsq_mu</code> by forcing the variance to be this number of standard deviations on the normal CDF */
	protected double hyper_k = 2.0;
	/** At a fixed <code>hyper_nu</code>, this controls where to set <code>hyper_lambda</code> by forcing q proportion to be at that value in the inverse gamma CDF */
	protected double hyper_q = 0.9;
		
	/** A hyperparameter that controls how easy it is to grow new nodes in a tree independent of depth */
	protected double alpha = 0.95;
	/** A hyperparameter that controls how easy it is to grow new nodes in a tree dependent on depth which makes it more difficult as the tree gets deeper */
	protected double beta = 2;
	/** the minimum of the response variable on its original scale */
	protected double y_min;
	/** the maximum of the response variable on its original scale */
	protected double y_max;
	/** the minimum of the response variable on its original scale */
	protected double y_range_sq;
	/** the sample variance of the response variable on its original scale */
	protected Double sample_var_y;
	/** if a covariate is a key here, the value defines interaction between the variables that are legal */
	protected HashMap<Integer, IntOpenHashSet> interaction_constraints;
	
	/** A cached table of log(sigsq + n * hyper_sigsq_mu) for n = 0 to N */
	protected transient double[] log_sigsq_plus_n_sigsq_mu_table;
	/** Cached log(sigsq) */
	protected transient double log_sigsq;
	
	/** A cached table of log(i) for i = 0 to max(N, P) */
	protected transient double[] log_table;
	/** A cached table of the depth-dependent part of the tree prior log-ratio */
	protected transient double[] depth_prior_log_ratio_table;

	/** A wrapper to set data which also calculates hyperparameters and statistics about the repsonse variable */
	public void setData(ArrayList<double[]> X_y){
		super.setData(X_y);
		calculateHyperparameters();
		initializeGlobalLogTables();
	}
	
	/** Initializes the global log tables */
	private void initializeGlobalLogTables() {
		int max_val = Math.max(n, Math.max(p, num_trees));
		log_table = new double[max_val + 1];
		log_table[0] = Double.NEGATIVE_INFINITY;
		for (int i = 1; i <= max_val; i++) {
			log_table[i] = Math.log(i);
		}
		
		// Precompute depth prior table for depths 0 to 100
		depth_prior_log_ratio_table = new double[101];
		double log_alpha = Math.log(alpha);
		for (int d = 0; d <= 100; d++) {
			double term1 = 1 - alpha / Math.pow(2 + d, beta);
			double term2 = Math.pow(1 + d, beta) - alpha;
			depth_prior_log_ratio_table[d] = log_alpha + 2 * Math.log(term1) - Math.log(term2);
		}
	}
	
	/** Updates the log table after a new sigsq is sampled */
	protected void updateLogSigsqTable(double sigsq) {
		if (log_sigsq_plus_n_sigsq_mu_table == null || log_sigsq_plus_n_sigsq_mu_table.length != n + 1) {
			log_sigsq_plus_n_sigsq_mu_table = new double[n + 1];
		}
		log_sigsq = Math.log(sigsq);
		for (int i = 0; i <= n; i++) {
			log_sigsq_plus_n_sigsq_mu_table[i] = Math.log(sigsq + i * hyper_sigsq_mu);
		}
	}
	
	/** Computes <code>hyper_sigsq_mu</code> and <code>hyper_lambda</code>. */
	protected void calculateHyperparameters() {
		hyper_mu_mu = 0;
		hyper_sigsq_mu = Math.pow(YminAndYmaxHalfDiff / (hyper_k * Math.sqrt(num_trees)), 2);
		
		if (sample_var_y == null){
			sample_var_y = StatToolbox.sample_variance(y_trans);
		}

		//calculate lambda from q
		double ten_pctile_chisq_df_hyper_nu = 0;		
		ChiSquaredDistributionImpl chi_sq_dist = new ChiSquaredDistributionImpl(hyper_nu);
		try {
			ten_pctile_chisq_df_hyper_nu = chi_sq_dist.inverseCumulativeProbability(1 - hyper_q);
		} catch (MathException e) {
			System.err.println("Could not calculate inverse cum prob density for chi sq df = " + hyper_nu + " with q = " + hyper_q);
			System.exit(0);
		}

		hyper_lambda = ten_pctile_chisq_df_hyper_nu / hyper_nu * sample_var_y;
	}	
	
	/** Computes the transformed y variable using the procedure outlined in the following paper:
	 *  
	 *  See HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive Regressive Trees. The Annals of Applied Statistics, 4(1): 266-298, 2010.
	 */
	protected void transformResponseVariable() {
		super.transformResponseVariable();
		y_min = StatToolbox.sample_minimum(y_orig);
		y_max = StatToolbox.sample_maximum(y_orig);
		y_range_sq = Math.pow(y_max - y_min, 2);
	
		for (int i = 0; i < n; i++){
			y_trans[i] = transform_y(y_orig[i]);
		}
	}

	/**
	 * Transforms a response value on the original scale to the transformed scale
	 * 
	 * @param y_i	The original response value
	 * @return		The transformed response value
	 */
	public double transform_y(double y_i){
		return (y_i - y_min) / (y_max - y_min) - YminAndYmaxHalfDiff;
	}
	
	/**
	 * Untransforms a vector of response value on the transformed scale back to the original scale
	 * 
	 * @param yt	The transformed response values
	 * @return		The original response values
	 */
	public double[] un_transform_y(double[] yt){
		double[] y = new double[yt.length];
		for (int i = 0; i < yt.length; i++){
			y[i] = un_transform_y(yt[i]);
		}
		return y;
	}
	
	/**
	 * Untransforms a response value on the transformed scale back to the original scale
	 * 
	 * @param yt_i	The transformed response value
	 * @return		The original response value
	 */
	public double un_transform_y(double yt_i){
		return (yt_i + YminAndYmaxHalfDiff) * (y_max - y_min) + y_min;
	}

	/**
	 * Vectorized version of un_transform_y for batch processing.
	 */
	public void un_transform_y_batch(double[] yt, double[] y_out) {
		int n = yt.length;
		var species = jdk.incubator.vector.DoubleVector.SPECIES_PREFERRED;
		var v_YminAndYmaxHalfDiff = jdk.incubator.vector.DoubleVector.broadcast(species, YminAndYmaxHalfDiff);
		var v_range = jdk.incubator.vector.DoubleVector.broadcast(species, y_max - y_min);
		var v_y_min = jdk.incubator.vector.DoubleVector.broadcast(species, y_min);
		
		int i = 0;
		int loopBound = species.loopBound(n);
		for (; i < loopBound; i += species.length()) {
			var v = jdk.incubator.vector.DoubleVector.fromArray(species, yt, i);
			// (v + diff) * range + min
			v.add(v_YminAndYmaxHalfDiff).mul(v_range).add(v_y_min).intoArray(y_out, i);
		}
		for (; i < n; i++) {
			y_out[i] = un_transform_y(yt[i]);
		}
	}
	
	/**
	 * Untransforms a response value on the transformed scale back to the original scale
	 * 
	 * @param yt_i	The transformed response value
	 * @return		The original response value
	 */
	public double un_transform_y(Double yt_i){
		if (yt_i == null){
			return -9999999;
		}
		return un_transform_y((double)yt_i);
	}	
	
	/**
	 * Untransforms a variance value on the transformed scale back to the original scale
	 * 
	 * @param sigsq_t_i		The transformed variance value
	 * @return				The original variance value
	 */
	public double un_transform_sigsq(double sigsq_t_i){
		//Based on the following elementary calculation: 
		//Var[y^t] = Var[y / R_y] = 1/R_y^2 Var[y]
		return sigsq_t_i * y_range_sq;
	}
	
	/**
	 * Untransforms many variance values on the transformed scale back to the original scale
	 * 
	 * @param sigsq_t_is	The transformed variance values
	 * @return				The original variance values
	 */
	public double[] un_transform_sigsq(double[] sigsq_t_is){
		double[] sigsq_is = new double[sigsq_t_is.length];
		for (int i = 0; i < sigsq_t_is.length; i++){
			sigsq_is[i] = un_transform_sigsq(sigsq_t_is[i]);
		}
		return sigsq_is;
	}			
	
	/**
	 * Untransforms a response value on the transformed scale back to the original scale and rounds to one decimal digit
	 * 
	 * @param yt_i	The transformed response value
	 * @return		The original response value rounded to one decimal digit
	 */
	public double un_transform_y_and_round(double yt_i){
		return Double.parseDouble(TreeArrayIllustration.one_digit_format.format((yt_i + YminAndYmaxHalfDiff) * (y_max - y_min) + y_min));
	}

	/**
	 * Untransforms many response values on the transformed scale back to the original scale and rounds them to one decimal digit
	 * 
	 * @param yt	The transformed response values
	 * @return		The original response values rounded to one decimal digit
	 */
	public double[] un_transform_y_and_round(double[] yt){
		double[] y = new double[yt.length];
		for (int i = 0; i < yt.length; i++){
			y[i] = un_transform_y_and_round(yt[i]);
		}
		return y;
	}	
	
	public void setInteractionConstraints(HashMap<Integer, IntOpenHashSet> interaction_constraints) {
		this.interaction_constraints = interaction_constraints;
	}

	/**
	 * Untransforms many response values on the transformed scale back to the original scale and rounds them to one decimal digit
	 * 
	 * @param yt	The transformed response values
	 * @return		The original response values rounded to one decimal digit
	 */
	public double[] un_transform_y_and_round(DoubleArrayList yt){
		return un_transform_y_and_round(yt.toDoubleArray());
	}	
	
	public void setK(double hyper_k) {
		this.hyper_k = hyper_k;
	}

	public void setQ(double hyper_q) {
		this.hyper_q = hyper_q;
	}

	public void setNu(double hyper_nu) {
		this.hyper_nu = hyper_nu;
	}
		
	public void setAlpha(double alpha){
		this.alpha = alpha;
	}
	
	public void setBeta(double beta){
		this.beta = beta;
	}
	
	public double getHyper_mu_mu() {
		return hyper_mu_mu;
	}

	public double getHyper_sigsq_mu() {
		return hyper_sigsq_mu;
	}

	public double getHyper_nu() {
		return hyper_nu;
	}

	public double getHyper_lambda() {
		return hyper_lambda;
	}

	public double getY_min() {
		return y_min;
	}

	public double getY_max() {
		return y_max;
	}

	public double getY_range_sq() {
		return y_range_sq;
	}
}
