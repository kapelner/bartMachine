package bartMachine;

import java.io.Serializable;

/**
 * The class that is instantiated to build a Regression BART model
 * 
 * @author Adam Kapelner and Justin Bleich
 *
 */
public class bartMachineRegression extends bartMachine_i_prior_cov_spec implements Serializable{
	
	/**
	 * Constructs the BART classifier for regression.
	 */
	public bartMachineRegression() {		
		super();
	}

}
