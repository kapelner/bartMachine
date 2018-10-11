package bartMachine;

import java.io.Serializable;
import java.util.ArrayList;

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
	protected static double[] samps_chi_sq_df_eq_nu_plus_n = {1, 2, 3, 4, 5}; //give a default for debugging in Java ONLY	
	/** The number of samples in the cached library of chi-squared values */
	protected static int samps_chi_sq_df_eq_nu_plus_n_length;

	/** A cached library of standard normal values (used for Gibbs sampling the posterior means of the terminal nodes) */

	protected static double[] samps_std_normal = {1, 2, 3, 4, 5}; //give a default for debugging in Java ONLY
	/** The number of samples in the cached library of standard normal values */
	protected static int samps_std_normal_length;
	
	/** the first hyperparameter for the lambda prior */
	protected double hyper_a;
	/** the second hyperparameter for the lambda prior  */
	protected double hyper_b;
	/** the hyperparameter for the k prior */
	protected double hyper_k_max;
	
	
	public double getHyper_a() {
		return hyper_a;
	}

	public void setHyper_a(double hyper_a) {
		this.hyper_a = hyper_a;
	}
	
	public double getHyper_b() {
		return hyper_b;
	}

	public void setHyper_b(double hyper_b) {
		this.hyper_b = hyper_b;
	}
	
	public double getHyper_Kmax() {
		return hyper_k_max;
	}

	public void setHyper_Kmax(double hyper_k_max) {
		this.hyper_k_max = hyper_k_max;
	}


	/** A hyperparameter that controls how easy it is to grow new nodes in a tree independent of depth */
	protected double alpha = 0.95;
	/** A hyperparameter that controls how easy it is to grow new nodes in a tree dependent on depth which makes it more difficult as the tree gets deeper */
	protected double beta = 2;
	

	public double getAlpha() {
		return alpha;
	}

	public void setAlpha(double alpha) {
		this.alpha = alpha;
	}

	public double getBeta() {
		return beta;
	}

	public void setBeta(double beta) {
		this.beta = beta;
	}
	
	/** the minimum of the response variable on its original scale */
	protected double y_min;
	/** the maximum of the response variable on its original scale */
	protected double y_max;
	/** the minimum of the response variable on its original scale */
	protected double y_range_sq;
	/** the sample variance of the response variable on its original scale */
	protected Double sample_var_y;
		
	/** A wrapper to set data which also calculates hyperparameters and statistics about the response variable */
	public void setData(ArrayList<double[]> X_y){
		super.setData(X_y);
	}

//ES(We need to worry about scaling here, as weibull does not have pretty property to scale easily like the norm)
//ES(If we somehow scale, we need to worry about the interval in which to scale... 0 to 1? etc) 
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
}
	

