package OpenSourceExtensions;

import java.io.Serializable;

/**
 * An unordered immutable pair of elements. <i>Unordered</i> means the pair <tt>(a,b)</tt> equals the
 * pair <tt>(b,a)</tt>. An element of a pair cannot be <tt>null</tt>.
 * <p/>
 * The class implements {@link Comparable}. Pairs are compared by their smallest elements, and if
 * those are equal, they are compared by their maximum elements.
 * <p/>
 * To work correctly, the underlying element type must implement {@link Object#equals} and
 * {@link Comparable#compareTo} in a consistent fashion.
 */
public record UnorderedPair<E extends Comparable<E>>(E first, E second) implements Comparable<UnorderedPair<E>>, Serializable {
	
	/**
	 * Creates an unordered pair of the specified elements. The order of the arguments is irrelevant,
	 * so the first argument is not guaranteed to be returned by {@link #first()}, for example.
	 * @param first one element of the pair. Must not be <tt>null</tt>.
	 * @param second one element of the pair. Must not be <tt>null</tt>. May be the same as <tt>a</tt>.
	 */
	public UnorderedPair {
		java.util.Objects.requireNonNull(first);
		java.util.Objects.requireNonNull(second);
		if (first.compareTo(second) > 0) {
			E temp = first;
			first = second;
			second = temp;
		}
		// Record fields are assigned here automatically
	}
	
	/**
	 * Gets the smallest element of the pair (according to its {@link Comparable} implementation).
	 * @return an element of the pair. <tt>null</tt> is never returned.
	 */
	public E getFirst() {
		return first;
	}
	
	/**
	 * Gets the largest element of the pair (according to its {@link Comparable} implementation).
	 * @return an element of the pair. <tt>null</tt> is never returned.
	 */
	public E getSecond() {
		return second;
	}

	@Override
	public int compareTo(UnorderedPair<E> o) {
		int firstCmp = first.compareTo(o.first);
		if (firstCmp != 0)
			return firstCmp;
		return second.compareTo(o.second);
	}
}
