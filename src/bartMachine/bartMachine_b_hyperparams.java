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
	private static final long serialVersionUID = -5560565005652344055L;
	
	/** A cached library of standard normal values (used for Gibbs sampling the posterior means of the terminal nodes) */
	protected static double[] samps_std_normal = {1, 2, 3, 4, 5}; //give a default for debugging in Java ONLY
	/** The number of samples in the cached library of standard normal values */
	protected static int samps_std_normal_length;
	
	/** the center of the prior of the terminal node prediction distribution */
	protected double hyper_a;
	/** the variance of the prior of the terminal node prediction distribution */
	protected double hyper_b;
	/** half the shape parameter and half the multiplicand of the scale parameter of the inverse gamma prior on the variance */
	protected double hyper_d;

	/** A hyperparameter that controls how easy it is to grow new nodes in a tree independent of depth */
	protected double alpha = 0.95;
	/** A hyperparameter that controls how easy it is to grow new nodes in a tree dependent on depth which makes it more difficult as the tree gets deeper */
	protected double beta = 2;
	
	
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

}
