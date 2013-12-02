package bartMachine;

import gnu.trove.list.array.TDoubleArrayList;
import gnu.trove.list.array.TIntArrayList;

import java.util.ArrayList;
import java.util.Collection;

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
	public static String StringJoin(TIntArrayList all, String joinby){
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
	public static String StringJoin(double[] all, String joinby){
		if (all == null){
			return " NULL ARRAY ";
		}		
		String joined = "";
		for (int i = 0; i < all.length; i++){
			joined += all[i];
			if (i < all.length - 1)
				joined += joinby;
		}
		return joined;
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
		String joined = "";
		for (int i = 0; i < all.length; i++){
			joined += all[i];
			if (i < all.length - 1)
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
	public static String StringJoin(TIntArrayList all){
		return StringJoin(all.toArray(), ", ");
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
	public static String StringJoin(TDoubleArrayList all){
		return StringJoin(all.toArray(), ", ");
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
		String joined = "";
		for (int i = 0; i < all.length; i++){
			joined += all[i];
			if (i < all.length - 1)
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
    	double max = Double.NEGATIVE_INFINITY;
        for (double value : values) {
        	if (value > max){
        		max = value;
        	}
        }
        return max;
    }
    
    /**
     * Sums an array
     * 
     * @param arr	The values of interest
     * @return		The sum of those values
     */
    public static double sum_array(double[] arr){
    	double sum = 0;
    	for (int i = 0; i < arr.length; i++){
    		sum += arr[i];
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
    	double sum = 0;
    	for (int i = 0; i < arr.length; i++){
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
    	for (int i = 0; i < arr.length; i++){
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
    	for (int i = 0; i < arr.length; i++){
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
		for (int i = 0; i < n; i++){
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
		for (int i = 0; i < n; i++){
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
		for (int i = 0; i < n; i++){
			sum[i] = (arr1[i] >= 1 ? 1 : 0) + (arr2[i] >= 1 ? 1 : 0);
		}
		return sum;
	}

}
