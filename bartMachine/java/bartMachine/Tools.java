package bartMachine;

import java.util.ArrayList;
import java.util.Collection;

import jdk.incubator.vector.DoubleVector;
import jdk.incubator.vector.IntVector;
import jdk.incubator.vector.VectorOperators;

import it.unimi.dsi.fastutil.doubles.DoubleArrayList;
import it.unimi.dsi.fastutil.ints.IntArrayList;

/**
 * A class that contains many generally useful convenience methods.
 * 
 * @author Adam Kapelner and Justin Bleich
 */
public class Tools {
	
	/**
	 * Joins a collection of strings into one string
	 * 
	 * @param all		the collection of substrings
	 * @param joinby	the token that joins the substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	@SuppressWarnings("rawtypes")
	public static String StringJoin(ArrayList all, String joinby){
		if (all == null){
			return " NULL ARRAY ";
		}
		return StringJoin(all.toArray(), joinby);
	}	
	
	/**
	 * Joins a collection of strings into one string
	 * 
	 * @param all		the collection of substrings
	 * @param joinby	the token that joins the substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(IntArrayList all, String joinby){
		if (all == null){
			return " NULL ARRAY ";
		}		
		return StringJoin(all.toIntArray(), joinby);
	}
	
	/**
	 * Joins a collection of strings into one string
	 * 
	 * @param all		the collection of substrings
	 * @param joinby	the token that joins the substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(double[] all, String joinby){
		if (all == null){
			return " NULL ARRAY ";
		}		
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < all.length; i++){
			sb.append(all[i]);
			if (i < all.length - 1)
				sb.append(joinby);
		}
		return sb.toString();
	}
	
	/**
	 * Joins a collection of strings into one string
	 * 
	 * @param all		the collection of substrings
	 * @param joinby	the token that joins the substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(int[] all, String joinby){
		if (all == null){
			return " NULL ARRAY ";
		}		
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < all.length; i++){
			sb.append(all[i]);
			if (i < all.length - 1)
				sb.append(joinby);
		}
		return sb.toString();
	}
	
	/**
	 * Joins a collection of strings into one string with commas
	 * 
	 * @param all		the collection of substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(IntArrayList all){
		return StringJoin(all.toIntArray(), ", ");
	}
	
	/**
	 * Joins a collection of strings into one string with commas
	 * 
	 * @param all		the collection of substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(boolean[] all){
		int[] all_ints = new int[all.length];
		for (int i = 0; i < all.length; i++){
			all_ints[i] = all[i] ? 1 : 0;
		}
		return StringJoin(all_ints, ", ");
	}
	
	/**
	 * Joins a collection of strings into one string with commas
	 * 
	 * @param all		the collection of substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(int[] all){
		return StringJoin(all, ", ");
	}
	
	/**
	 * Joins a collection of strings into one string with commas
	 * 
	 * @param all		the collection of substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(DoubleArrayList all){
		return StringJoin(all.toDoubleArray(), ", ");
	}
	
	/**
	 * Joins a collection of strings into one string with commas
	 * 
	 * @param all		the collection of substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(double[] all){
		return StringJoin(all, ", ");
	}

	/**
	 * Joins a collection of strings into one string with commas
	 * 
	 * @param all		the collection of substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(ArrayList<Object> all){
		return StringJoin(all, ", ");
	}	
	
	/**
	 * Joins a collection of strings into one string
	 * 
	 * @param all		the collection of substrings
	 * @param joinby	the token that joins the substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(Object[] all, String joinby){
		if (all == null){
			return " NULL ARRAY ";
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < all.length; i++){
			sb.append(all[i]);
			if (i < all.length - 1)
				sb.append(joinby);
		}
		return sb.toString();
	}	
	
	/**
	 * Joins a collection of strings into one string with commas
	 * 
	 * @param all		the collection of substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(Object[] all){
		return StringJoin(all, ", ");
	}	
	
	/**
	 * Joins a collection of strings into one string
	 * 
	 * @param all		the collection of substrings
	 * @param joinby	the token that joins the substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoinStrings(Collection<String> all, String joinby){
		Object[] arr = all.toArray();
		String joined = "";
		for (int i = 0; i < arr.length; i++){
			joined += (String)arr[i];
			if (i < arr.length - 1)
				joined += joinby;
		}
		return joined;
	}
	
	/**
	 * Joins a collection of strings into one string with commas
	 * 
	 * @param all		the collection of substrings
	 * @return			the final product: str1 + joinby + str2 + . . . + strN
	 */	
	public static String StringJoin(Collection<String> all){
		return StringJoinStrings(all, ", ");
	}	
	
	/**
	 * Returns the max of a vector
	 * 
	 * @param values	The values of interest
	 * @return			The maximum of those values
	 */
    public static double max(double[] values) {
        if (values == null || values.length == 0) {
            return Double.NEGATIVE_INFINITY;
        }
        
        var species = DoubleVector.SPECIES_PREFERRED;
        var maxVec = DoubleVector.broadcast(species, Double.NEGATIVE_INFINITY);
        int i = 0;
        int upperBound = species.loopBound(values.length);
        
        for (; i < upperBound; i += species.length()) {
            var v = DoubleVector.fromArray(species, values, i);
            maxVec = maxVec.max(v);
        }
        
        double max = maxVec.reduceLanes(VectorOperators.MAX);
        
        for (; i < values.length; i++) {
            if (values[i] > max) {
                max = values[i];
            }
        }
        return max;
    }
    
    /**
     * Sums an array of doubles
     * 
     * @param arr	The values of interest
     * @return		The sum of those values
     */
    public static double sum_array(double[] arr){
    	var species = DoubleVector.SPECIES_PREFERRED;
    	var sumVec = DoubleVector.zero(species);
    	int i = 0;
    	int upperBound = species.loopBound(arr.length);
    	for (; i < upperBound; i += species.length()) {
    		var v = DoubleVector.fromArray(species, arr, i);
    		sumVec = sumVec.add(v);
    	}
    	double sum = sumVec.reduceLanes(VectorOperators.ADD);
    	for (; i < arr.length; i++){
    		sum += arr[i];
    	}
    	return sum;
    }
    
    
    /**
     * Sums an array of booleans
     * 
     * @param arr	The values of interest
     * @return		The sum of those values
     */
    public static int sum_array(boolean[] arr){
    	int sum = 0;
    	for (int i = 0; i < arr.length; i++){
    		if (arr[i]) {
    			sum++;
    		}
    	}
    	return sum;
    }
    
    /**
     * Sums the inverse values of an array
     * 
     * @param arr	The values of interest
     * @return		The sum of the inverses of those values
     */
	public static double sum_inv_array(double[] arr) {
		var species = DoubleVector.SPECIES_PREFERRED;
		var sumVec = DoubleVector.zero(species);
		var oneVec = DoubleVector.broadcast(species, 1.0);
		int i = 0;
		int upperBound = species.loopBound(arr.length);
		for (; i < upperBound; i += species.length()) {
			var v = DoubleVector.fromArray(species, arr, i);
			sumVec = sumVec.add(oneVec.div(v));
		}
		double sum = sumVec.reduceLanes(VectorOperators.ADD);
    	for (; i < arr.length; i++){
    		sum += 1 / arr[i];
    	}
    	return sum;
	}	    
 
	/**
	 * Normalizes an array by dividing each value by the array's sum
	 * 
	 * @param arr	The values of interest
	 */
    public static void normalize_array(double[] arr){
    	double weight = sum_array(arr);
		var species = DoubleVector.SPECIES_PREFERRED;
		int i = 0;
		int upperBound = species.loopBound(arr.length);
		for (; i < upperBound; i += species.length()) {
			var v = DoubleVector.fromArray(species, arr, i);
			v.div(weight).intoArray(arr, i);
		}
    	for (; i < arr.length; i++){
    		arr[i] = arr[i] / weight;
    	}
    }
    	
	/**
	 * Weights an array by dividing each value by a specified value
	 * 
	 * @param weight	The value to divide each value in the array by
	 * @param arr		The values of interest
	 */
    public static void weight_arr(double[] arr, double weight){
		var species = DoubleVector.SPECIES_PREFERRED;
		int i = 0;
		int upperBound = species.loopBound(arr.length);
		for (; i < upperBound; i += species.length()) {
			var v = DoubleVector.fromArray(species, arr, i);
			v.div(weight).intoArray(arr, i);
		}
    	for (; i < arr.length; i++){
    		arr[i] = arr[i] / weight;
    	}
    }    

    /**
     * Subtracts one array from the other
     * 
     * @param arr1	The array of minuends
     * @param arr2	The array of subtrahends
     * @return		The array of differences
     */
	public static double[] subtract_arrays(double[] arr1, double[] arr2) {
		int n = arr1.length;
		double[] diff = new double[n];
		var species = DoubleVector.SPECIES_PREFERRED;
		int i = 0;
		int upperBound = species.loopBound(n);
		for (; i < upperBound; i += species.length()) {
			var v1 = DoubleVector.fromArray(species, arr1, i);
			var v2 = DoubleVector.fromArray(species, arr2, i);
			v1.sub(v2).intoArray(diff, i);
		}
		for (; i < n; i++){
			diff[i] = arr1[i] - arr2[i];
		}
		return diff;
	}

    /**
     * Adds one array to another
     * 
     * @param arr1	The array of first addends
     * @param arr2	The array of seconds addends
     * @return		The array of sums
     */
	public static double[] add_arrays(double[] arr1, double[] arr2) {
		int n = arr1.length;
		double[] sum = new double[n];
		var species = DoubleVector.SPECIES_PREFERRED;
		int i = 0;
		int upperBound = species.loopBound(n);
		for (; i < upperBound; i += species.length()) {
			var v1 = DoubleVector.fromArray(species, arr1, i);
			var v2 = DoubleVector.fromArray(species, arr2, i);
			v1.add(v2).intoArray(sum, i);
		}
		for (; i < n; i++){
			sum[i] = arr1[i] + arr2[i];
		}
		return sum;
	}

    /**
     * Adds one array to another
     * 
     * @param arr1	The array of first addends
     * @param arr2	The array of seconds addends
     * @return		The array of sums
     */	
	public static double[] add_arrays(double[] arr1, int[] arr2) {
		int n = arr1.length;
		double[] sum = new double[n];
		for (int i = 0; i < n; i++){
			sum[i] = arr1[i] + arr2[i];
		}
		return sum;
	}

    /**
     * Adds one array to another
     * 
     * @param arr1	The array of first addends
     * @param arr2	The array of seconds addends
     * @return		The array of sums
     */		
	public static int[] add_arrays(int[] arr1, int[] arr2) {
		int n = arr1.length;
		int[] sum = new int[n];
		var species = IntVector.SPECIES_PREFERRED;
		int i = 0;
		int upperBound = species.loopBound(n);
		for (; i < upperBound; i += species.length()) {
			var v1 = IntVector.fromArray(species, arr1, i);
			var v2 = IntVector.fromArray(species, arr2, i);
			v1.add(v2).intoArray(sum, i);
		}
		for (; i < n; i++){
			sum[i] = arr1[i] + arr2[i];
		}
		return sum;
	}

    /**
     * Adds one array to another after first converting each addend to binary
     * (1 if the value > 0, 0 otherwise)
     * 
     * @param arr1	The array of first addends
     * @param arr2	The array of seconds addends
     * @return		The array of sums of binary valus
     */
	public static int[] binary_add_arrays(int[] arr1, int[] arr2) {
		int n = arr1.length;
		int[] sum = new int[n];
		var species = jdk.incubator.vector.IntVector.SPECIES_PREFERRED;
		var v_zero = jdk.incubator.vector.IntVector.zero(species);
		int i = 0;
		int loopBound = species.loopBound(n);
		for (; i < loopBound; i += species.length()) {
			var v1 = jdk.incubator.vector.IntVector.fromArray(species, arr1, i);
			var v2 = jdk.incubator.vector.IntVector.fromArray(species, arr2, i);
			
			var b1 = v_zero.add(1, v1.compare(jdk.incubator.vector.VectorOperators.GE, 1));
			var b2 = v_zero.add(1, v2.compare(jdk.incubator.vector.VectorOperators.GE, 1));
			
			b1.add(b2).intoArray(sum, i);
		}
		for (; i < n; i++){
			sum[i] = (arr1[i] >= 1 ? 1 : 0) + (arr2[i] >= 1 ? 1 : 0);
		}
		return sum;
	}

}
