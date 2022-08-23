/*
    BART - Bayesian Additive Regressive Trees
    Software for Supervised Statistical Learning
    
    Copyright (C) 2012 Professor Ed George & Adam Kapelner, 
    Dept of Statistics, The Wharton School of the University of Pennsylvania

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details:
    
    http://www.gnu.org/licenses/gpl-2.0.txt

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

package bartMachine;

import gnu.trove.list.array.TDoubleArrayList;
import gnu.trove.list.array.TIntArrayList;

import java.util.Arrays;

import OpenSourceExtensions.MersenneTwisterFast;

/**
 * This is a class where we're going to put all sorts of useful functions
 * as a utility-style class
 */
public class StatToolbox {
	
	/** A convenience for a Random object */
	private static final MersenneTwisterFast R = new MersenneTwisterFast();
	/** A flag that indicates an illegal value or failed operation */
	public static final double ILLEGAL_FLAG = -999999999;	


	/**
	 * Draws a sample from an inverse gamma distribution.
	 * 
	 * @param k			The shape parameter of the inverse gamma distribution of interest	
	 * @param theta		The scale parameter of the inverse gamma distribution of interest
	 * @return			The sampled value
	 */
	public static double sample_from_inv_gamma(double k, double theta){
		return (1 / (theta / 2)) / bartMachine_b_hyperparams.samps_chi_sq_df_eq_nu_plus_n[(int)Math.floor(rand() * bartMachine_b_hyperparams.samps_chi_sq_df_eq_nu_plus_n_length)];
	}
	
	/**
	 * Draws a sample from a normal distribution.
	 * 
	 * @param mu		The mean of the normal distribution of interest
	 * @param sigsq		The variance of the normal distribution of interest
	 * @return			The sample value
	 */
	public static double sample_from_norm_dist(double mu, double sigsq){
		double std_norm_realization = bartMachine_b_hyperparams.samps_std_normal[(int)Math.floor(rand() * bartMachine_b_hyperparams.samps_std_normal_length)];
		return mu + Math.sqrt(sigsq) * std_norm_realization;
	}
	
	// constants for the {@link normal_cdf} function
    private static double NORM_CDF_a1 =  0.254829592;
    private static double NORM_CDF_a2 = -0.284496736;
    private static double NORM_CDF_a3 =  1.421413741;
    private static double NORM_CDF_a4 = -1.453152027;
    private static double NORM_CDF_a5 =  1.061405429;
    private static double NORM_CDF_p  =  0.3275911;	
	
    /**
     * Calculate the cumulative density under a standard normal to a point of interest.
     *      
     * @param x	The point of interest on the standard normal density support
     * @return	The probability of interest
     * 
     * @see {@link http://www.johndcook.com/cpp_phi.html}
     */
	public static double normal_cdf(double x) {
	    // Save the sign of x
	    int sign = 1;
	    if (x < 0){
	        sign = -1;
	    }
	    x = Math.abs(x) / Math.sqrt(2.0);

	    // A&S formula 7.1.26
	    double t = 1.0 / (1.0 + NORM_CDF_p * x);
	    double y = 1.0 - (((((NORM_CDF_a5 * t + NORM_CDF_a4) * t) + NORM_CDF_a3) * t + NORM_CDF_a2) * t + NORM_CDF_a1) * t * Math.exp(-x * x);

	    return 0.5 * (1.0 + sign * y);
	}
	
	/**
	 * Compute the sample average of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample average
	 */
	public static final double sample_average(double[] y){
		double y_bar = 0;
		for (int i = 0; i < y.length; i++){
			y_bar += y[i];
		}
		return y_bar / (double)y.length;
	}

	/**
	 * Compute the sample average of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample average
	 */
	public static final double sample_average(TDoubleArrayList y){
		double y_bar = 0;
		for (int i = 0; i < y.size(); i++){
			y_bar += y.get(i);
		}
		return y_bar / (double)y.size();
	}	
	
	/**
	 * Compute the sample average of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample average
	 */
	public static final double sample_average(int[] y){
		double y_bar = 0;
		for (int i = 0; i < y.length; i++){
			y_bar += y[i];
		}
		return y_bar / (double)y.length;
	}

	/**
	 * Compute the sample median of a vector of data
	 * 
	 * @param arr	The vector of data values
	 * @return		The sample median
	 */
	public static double sample_median(double[] arr) {
		int n = arr.length;
		Arrays.sort(arr);
		if (n % 2 == 0){
			double a = arr[n / 2];
			double b = arr[n / 2 - 1];
			return (a + b) / 2;
		}
		else {
			return arr[(n - 1) / 2];
		}
		
	}
	
	/**
	 * Compute the sample standard deviation of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample standard deviation
	 */
	public static final double sample_standard_deviation(int[] y){
		double y_bar = sample_average(y);
		double sum_sqd_deviations = 0;
		for (int i = 0; i < y.length; i++){
			sum_sqd_deviations += Math.pow(y[i] - y_bar, 2);
		}
		return Math.sqrt(sum_sqd_deviations / ((double)y.length - 1));		
	}
	
	/**
	 * Compute the sample standard deviation of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample standard deviation
	 */
	public static final double sample_standard_deviation(double[] y){
		return Math.sqrt(sample_variance(y));
	}	
	
	/**
	 * Compute the sample variance of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample variance
	 */
	public static final double sample_variance(double[] y){
		return sample_sum_sq_err(y) / ((double)y.length - 1);		
	}	
	
	/**
	 * Compute the sum of squared error (the squared deviation from the sample average) of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sum of squared error
	 */	
	public static final double sample_sum_sq_err(double[] y){
		double y_bar = sample_average(y);
		double sum_sqd_deviations = 0;
		for (int i = 0; i < y.length; i++){
			sum_sqd_deviations += Math.pow(y[i] - y_bar, 2);
		}
		return sum_sqd_deviations;
	}

	/**
	 * Compute the sample minimum of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample minimum
	 */
	public static double sample_minimum(int[] y) {
		int min = Integer.MAX_VALUE;
		for (int y_i : y){
			if (y_i < min){
				min = y_i;
			}
		}
		return min;
	}

	/**
	 * Compute the sample maximum of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample maximum
	 */
	public static double sample_maximum(int[] y) {
		int max = Integer.MIN_VALUE;
		for (int y_i : y){
			if (y_i > max){
				max = y_i;
			}
		}
		return max;		
	}

	/**
	 * Compute the sample minimum of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample minimum
	 */
	public static double sample_minimum(double[] y){
		double min = Double.MAX_VALUE;
		for (double y_i : y){
			if (y_i < min){
				min = y_i;
			}
		}
		return min;		
	}
	
	/**
	 * Compute the sample maximum of a vector of data
	 * 
	 * @param y	The vector of data values
	 * @return	The sample maximum
	 */
	public static double sample_maximum(double[] y){
		double max = Double.NEGATIVE_INFINITY;
		for (double y_i : y){
			if (y_i > max){
				max = y_i;
			}
		}
		return max;			
	}
	
	/**
	 * Given an array, return the index of the maximum value
	 * 
	 * @param y		The vector of data value
	 * @return		The index of the greatest value in the array
	 */
	public static int FindMaxIndex(int[] y){
		int index = 0;
		int max = Integer.MIN_VALUE;
		for (int i = 0; i < y.length; i++){
			if (y[i] > max){
				max = y[i];
				index = i;
			}				
		}
		return index;
	}

	/**
	 * Sample from a multinomial distribution
	 * 
	 * @param vals		The integer values of the labels in this multinomial distribution
	 * @param probs		The probabilities with which to sample the labels (must be the same length of the vals)
	 * @return			The integer label of the value that was drawn from this multinomial distribution
	 */
	public static int multinomial_sample(TIntArrayList vals, double[] probs) {
		double r = StatToolbox.rand();
		double cum_prob = 0;
		int index = 0;
		if (r < probs[0]){
			return vals.get(0);
		}
		while (true){			
			cum_prob += probs[index];
			if (r > cum_prob && r < cum_prob + probs[index + 1]){
				return vals.get(index + 1);
			}
			index++;
		}
	}

	/**
	 * Set the seed of the random number generator
	 * 
	 * @param seed	The seed
	 */
	public static void setSeed(long seed) {
		R.setSeed(seed);
	}
	
	/** 
	 * A convenience method for a random object
	 * 
	 * @return	A random number drawn from a uniform distirbution bounded between 0 and 1.
	 */
	public static double rand(){
		return R.nextDouble();
	}	
}
