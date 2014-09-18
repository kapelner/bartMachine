package bartMachine;

import gnu.trove.list.array.TDoubleArrayList;

import java.io.Serializable;
import java.util.ArrayList;

import org.apache.commons.math.MathException;
import org.apache.commons.math.distribution.ChiSquaredDistributionImpl;

/**
 * This portion of the code controls hyperparameters for the BART
 * algorithm as well as properties and transformations of the response variable.
 * 
 * @author Adam Kapelner and Justin Bleich
 */
public abstract class bartMachine_b_hyperparams extends bartMachine_a_base implements Serializable{

	/** The static field that controls the bounds on the transformed y variable which is between negative and positive this value */
	protected static final double YminAndYmaxHalfDiff = 0.5;
	
	/** A cached library of chi-squared with degrees of freedom nu plus n (used for Gibbs sampling the variance) */
	protected static double[] samps_chi_sq_df_eq_nu_plus_n;	
	/** The number of samples in the cached library of chi-squared values */
	protected static int samps_chi_sq_df_eq_nu_plus_n_length;
	/** A cached library of standard normal values (used for Gibbs sampling the posterior means of the terminal nodes) */
	protected static double[] samps_std_normal;
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
		
	/** A wrapper to set data which also calculates hyperparameters and statistics about the repsonse variable */
	public void setData(ArrayList<double[]> X_y){
		super.setData(X_y);
		calculateHyperparameters();	
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
	 *  @see HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive Regressive Trees. The Annals of Applied Statistics, 4(1): 266-298, 2010.
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

	/**
	 * Untransforms many response values on the transformed scale back to the original scale and rounds them to one decimal digit
	 * 
	 * @param yt	The transformed response values
	 * @return		The original response values rounded to one decimal digit
	 */
	public double[] un_transform_y_and_round(TDoubleArrayList yt){
		return un_transform_y_and_round(yt.toArray());
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
