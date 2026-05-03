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

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicLong;

import jdk.incubator.vector.DoubleVector;
import jdk.incubator.vector.IntVector;
import jdk.incubator.vector.VectorOperators;

import OpenSourceExtensions.MersenneTwisterFast;
import it.unimi.dsi.fastutil.doubles.DoubleArrayList;
import it.unimi.dsi.fastutil.ints.IntArrayList;

/**
 * This is a class where we're going to put all sorts of useful functions
 * as a utility-style class
 */
public class StatToolbox {
	
	private static boolean USE_XOSHIRO = false;

	private static final AtomicLong SEED_UNIQUIFIER = new AtomicLong(System.nanoTime());
	
	private static long nextSeed() {
		long seed = SEED_UNIQUIFIER.getAndAdd(0x9E3779B97F4A7C15L);
		seed ^= System.nanoTime();
		seed ^= Thread.currentThread().threadId();
		return seed;
	}
	
	private static Object createRng() {
		long seed = nextSeed();
		if (USE_XOSHIRO) {
			return java.util.random.RandomGeneratorFactory.of("Xoshiro256PlusPlus").create(seed);
		}
		return new MersenneTwisterFast(seed);
	}
	
	/** A thread-local random source to ensure thread safety */
	private static final ThreadLocal<Object> R = ThreadLocal.withInitial(StatToolbox::createRng);
	
	public static void setUseXoshiro(boolean useXoshiro) {
		if (USE_XOSHIRO != useXoshiro) {
			USE_XOSHIRO = useXoshiro;
			// Clear the thread local to force re-initialization with the new algorithm
			R.remove();
		}
	}

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
			double std_norm_realization;
			Object random = R.get();
			if (random instanceof java.util.random.RandomGenerator rg) {
				std_norm_realization = rg.nextGaussian();
			} else {
				std_norm_realization = bartMachine_b_hyperparams.samps_std_normal[(int)Math.floor(rand() * bartMachine_b_hyperparams.samps_std_normal_length)];
			}
			return mu + Math.sqrt(sigsq) * std_norm_realization;
		}	

	
	/**
	 * Compute the sample average of a vector of data
	 *
	 * @param y	The vector of data values
	 * @return	The sample average
	 */
	public static final double sample_average(double[] y){
		return Tools.sum_array(y) / (double)y.length;
	}

	/**
	 * Compute the sample average of a vector of data
	 *
	 * @param y	The vector of data values
	 * @return	The sample average
	 */
	public static final double sample_average(DoubleArrayList y){
		double y_bar = 0;
		for (int i = 0; i < y.size(); i++){
			y_bar += y.getDouble(i);
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
		var species = IntVector.SPECIES_PREFERRED;
		var sumVec = IntVector.zero(species);
		int i = 0;
		int upperBound = species.loopBound(y.length);
		for (; i < upperBound; i += species.length()) {
			sumVec = sumVec.add(IntVector.fromArray(species, y, i));
		}
		double sum = sumVec.reduceLanes(VectorOperators.ADD);
		for (; i < y.length; i++){
			sum += y[i];
		}
		return sum / (double)y.length;
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
			double diff = y[i] - y_bar;
			sum_sqd_deviations = Math.fma(diff, diff, sum_sqd_deviations);
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
		var species = DoubleVector.SPECIES_PREFERRED;
		var sumSqVec = DoubleVector.zero(species);
		var meanVec = DoubleVector.broadcast(species, y_bar);
		int i = 0;
		int upperBound = species.loopBound(y.length);
		for (; i < upperBound; i += species.length()) {
			var v = DoubleVector.fromArray(species, y, i);
			var diff = v.sub(meanVec);
			sumSqVec = diff.fma(diff, sumSqVec);
		}
		double sum_sqd_deviations = sumSqVec.reduceLanes(VectorOperators.ADD);
		for (; i < y.length; i++){
			double diff = y[i] - y_bar;
			sum_sqd_deviations = Math.fma(diff, diff, sum_sqd_deviations);
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
		var species = IntVector.SPECIES_PREFERRED;
		var minVec = IntVector.broadcast(species, Integer.MAX_VALUE);
		int i = 0;
		int upperBound = species.loopBound(y.length);
		for (; i < upperBound; i += species.length()) {
			var v = IntVector.fromArray(species, y, i);
			minVec = minVec.min(v);
		}
		int min = minVec.reduceLanes(VectorOperators.MIN);
		for (; i < y.length; i++){
			if (y[i] < min){
				min = y[i];
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
		var species = IntVector.SPECIES_PREFERRED;
		var maxVec = IntVector.broadcast(species, Integer.MIN_VALUE);
		int i = 0;
		int upperBound = species.loopBound(y.length);
		for (; i < upperBound; i += species.length()) {
			var v = IntVector.fromArray(species, y, i);
			maxVec = maxVec.max(v);
		}
		int max = maxVec.reduceLanes(VectorOperators.MAX);
		for (; i < y.length; i++){
			if (y[i] > max){
				max = y[i];
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
		var species = DoubleVector.SPECIES_PREFERRED;
		var minVec = DoubleVector.broadcast(species, Double.MAX_VALUE);
		int i = 0;
		int upperBound = species.loopBound(y.length);
		for (; i < upperBound; i += species.length()) {
			var v = DoubleVector.fromArray(species, y, i);
			minVec = minVec.min(v);
		}
		double min = minVec.reduceLanes(VectorOperators.MIN);
		for (; i < y.length; i++){
			if (y[i] < min){
				min = y[i];
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
		var species = DoubleVector.SPECIES_PREFERRED;
		var maxVec = DoubleVector.broadcast(species, Double.NEGATIVE_INFINITY);
		int i = 0;
		int upperBound = species.loopBound(y.length);
		for (; i < upperBound; i += species.length()) {
			var v = DoubleVector.fromArray(species, y, i);
			maxVec = maxVec.max(v);
		}
		double max = maxVec.reduceLanes(VectorOperators.MAX);
		for (; i < y.length; i++){
			if (y[i] > max){
				max = y[i];
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
	public static int multinomial_sample(IntArrayList vals, double[] probs) {
		double r = StatToolbox.rand();
		double cum_prob = 0;
		int index = 0;
		if (r < probs[0]){
			return vals.getInt(0);
		}
		while (true){			
			cum_prob += probs[index];
			if (r > cum_prob && r < cum_prob + probs[index + 1]){
				return vals.getInt(index + 1);
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
		Object random = R.get();
		if (random instanceof MersenneTwisterFast mt) {
			mt.setSeed(seed);
		} else if (random instanceof java.util.random.RandomGenerator rg) {
			// RandomGenerator doesn't have setSeed, so we re-create it from factory
			R.set(java.util.random.RandomGeneratorFactory.of("Xoshiro256PlusPlus").create(seed));
		}
	}
	
	/** 
	 * A convenience method for a random object
	 *
	 * @return	A random number drawn from a uniform distirbution bounded between 0 and 1.
	 */
	public static double rand(){
		Object random = R.get();
		if (random instanceof MersenneTwisterFast mt) {
			return mt.nextDouble(false, false);
		} else {
			java.util.random.RandomGenerator rg = (java.util.random.RandomGenerator) random;
			double d = rg.nextDouble();
			while (d == 0.0 || d == 1.0) {
				d = rg.nextDouble();
			}
			return d;
		}
	}	

	/**
	 * A convenience method for nextInt from the random object
	 * @param bound the upper bound (exclusive). Must be positive.
	 * @return a random integer between 0 (inclusive) and bound (exclusive)
	 */
	public static int nextInt(int bound) {
		Object random = R.get();
		if (random instanceof MersenneTwisterFast mt) {
			return mt.nextInt(bound);
		} else {
			return ((java.util.random.RandomGenerator) random).nextInt(bound);
		}
	}
}
