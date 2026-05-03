///////////////////////////////////////////////////////////////////////////////
//Copyright (c) 2001, Eric D. Friedman All Rights Reserved.
//Copyright (c) 2009, Rob Eden All Rights Reserved.
//Copyright (c) 2009, Jeff Randall All Rights Reserved.
//
//This library is free software; you can redistribute it and/or
//modify it under the terms of the GNU Lesser General Public
//License as published by the Free Software Foundation; either
//version 2.1 of the License, or (at your option) any later version.
//
//This library is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU Lesser General Public
//License along with this program; if not, write to the Free Software
//Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
///////////////////////////////////////////////////////////////////////////////

package OpenSourceExtensions;

import it.unimi.dsi.fastutil.doubles.AbstractDoubleSet;
import it.unimi.dsi.fastutil.doubles.DoubleArrayList;
import it.unimi.dsi.fastutil.doubles.DoubleCollection;
import it.unimi.dsi.fastutil.doubles.DoubleIterator;
import it.unimi.dsi.fastutil.doubles.DoubleOpenHashSet;
import it.unimi.dsi.fastutil.doubles.DoubleSet;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.Collection;

public class TDoubleHashSetAndArray extends AbstractDoubleSet implements DoubleSet, Externalizable {
    private static final long serialVersionUID = 1L;

    private DoubleArrayList array;
    private DoubleOpenHashSet set;

    public TDoubleHashSetAndArray() {
        this(16, 0.5f);
    }

    public TDoubleHashSetAndArray(int initialCapacity) {
        this(initialCapacity, 0.5f);
    }

    public TDoubleHashSetAndArray(int initialCapacity, float loadFactor) {
        array = new DoubleArrayList(initialCapacity);
        set = new DoubleOpenHashSet(initialCapacity, loadFactor);
    }

    public TDoubleHashSetAndArray(int initialCapacity, float loadFactor, double noEntryValue) {
        this(initialCapacity, loadFactor);
    }

    public TDoubleHashSetAndArray(Collection<? extends Double> collection) {
        this(collection.size());
        addAll(collection);
    }

    public TDoubleHashSetAndArray(DoubleCollection collection) {
        this(collection.size());
        addAll(collection);
    }

    public TDoubleHashSetAndArray(double[] array) {
        this(array.length);
        // We iterate and add to ensure uniqueness in set and order in array.
        // Crucially, Trove's implementation iterates in REVERSE order.
        // We match this behavior to ensure deterministic compatibility.
        for (int i = array.length; i-- > 0;) {
            add(array[i]);
        }
    }

    @Override
    public boolean add(double k) {
        if (k == -0.0) k = 0.0; // Normalize -0.0 to 0.0
        if (set.add(k)) {
            array.add(k);
            return true;
        }
        return false;
    }

    @Override
    public boolean remove(double k) {
        if (k == -0.0) k = 0.0;
        boolean inSet = set.remove(k);
        boolean inArray = array.rem(k); // rem removes first occurrence
        return inSet || inArray;
    }

    @Override
    public boolean contains(double k) {
        if (k == -0.0) k = 0.0;
        return set.contains(k);
    }

    @Override
    public DoubleIterator iterator() {
        return array.iterator();
    }

    @Override
    public int size() {
        return array.size();
    }

    public double getAtIndex(int index) {
        return array.getDouble(index);
    }

    @Override
    public double[] toDoubleArray() {
        return array.toDoubleArray();
    }
    
    // Helper to match old API if possible, but avoids conflict with Object[] toArray()
    // We cannot define double[] toArray() here.
    
    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeByte(1);
        out.writeInt(size());
        DoubleIterator it = array.iterator();
        while(it.hasNext()) out.writeDouble(it.nextDouble());
    }

    public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
        in.readByte(); // version
        int size = in.readInt();
        array = new DoubleArrayList(size);
        set = new DoubleOpenHashSet(size);
        for(int i=0; i<size; i++) {
            add(in.readDouble());
        }
    }
}
