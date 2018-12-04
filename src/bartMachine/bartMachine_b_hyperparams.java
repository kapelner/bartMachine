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
	private static final long serialVersionUID = 8194181668890893274L;

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
	
	/** the first hyperparameter for the k parameter */
	protected double hyper_a;
	/** the second hyperparameter for the k parameter */
	protected double hyper_b;
	/** the third hyperparameter for the k parameter */
	protected double hyper_d;
	
	
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
	
	public double getHyper_d() {
		return hyper_d;
	}

	public void setHyper_d(double hyper_d) {
		this.hyper_d = hyper_d;
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

}
	

