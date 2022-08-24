package OpenSourceExtensions;

import it.unimi.dsi.fastutil.doubles.DoubleArrayList;
import it.unimi.dsi.fastutil.doubles.DoubleOpenHashSet;

@SuppressWarnings("serial")
public class DoubleOpenHashSetAndArrayList extends DoubleOpenHashSet {
	private DoubleArrayList array;

	public DoubleOpenHashSetAndArrayList(double[] arr) {
		array = new DoubleArrayList(arr.length); //at most it's this length (if all unique)
		for (int i = 0; i < arr.length; i++) {
			double d = arr[i];
			if (!contains(d)) { //ensure unique only -- this is the whole point of this class!
				super.add(d);
				array.add(d);
			}
		}		
	}
	
	public boolean remove(double k) {
		//we only need to remove the value from the array since that's all we care about
		//the hash function is only needed during construction to get rid of the non-unique values.		
		return array.rem(k);
	}

	public double[] elements() {
		return array.elements();
	}

	public double getAtIndex(int index) {
		return array.getDouble(index);
	}
	
	public int size() {
	    return array.size();
	}
	
	
}
