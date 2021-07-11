package it.unipi.jenoma.population;

import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;
import java.util.Iterator;
import java.util.List;


/**
 * Class representing a chromosome inside the genetic domain of the problem.
 * @param <T>  the class representing a gene for this chromosome. It must implement Serializable.
 */
public class Chromosome<T extends Serializable> implements Iterable<T>, Serializable {
    private final List<T> genes;


    public Chromosome(List<T> genes) {
        this.genes = genes;
    }

    public int getLength() {
        return genes.size();
    }

    public T getGene(int index) {
        return genes.get(index);
    }

    public void setGene(int index, T newGene) {
        genes.set(index, newGene);
    }

    public Iterator<T> iterator() {
        return new Iterator<>() {
            private final Iterator<T> iterator = genes.iterator();

            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public T next() {
                return iterator.next();
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException("The removal of genes is not supported");
            }
        };
    }

    /**
     * Replaces the genes of the chromosome with the genes of the given one in the provided interval.
     * @param c          the chromosome from which the new genes are taken.
     * @param fromIndex  the lower bound (inclusive) of the interval.
     * @param toIndex    the upper bound (exclusive) of the interval.
     */
    public void cross(Chromosome<T> c, int fromIndex, int toIndex) {
       for (int i = fromIndex; i <= toIndex; i++)
           this.genes.set(i, c.getGene(i));
    }

    /**
     * Returns a deep clone of the chromosome exploiting the <code>SerializationUtils</code> provided in
     * <code>org.apache.commons.lang3.SerializationUtils</code>.
     * @return  a deep clone of the chromosome.
     */
    public Chromosome<T> clone() {
        return SerializationUtils.clone(this);
    }

    @Override
    public String toString() {
        if (genes == null)
            return "NULL";

        if (genes.size() == 0)
            return "[]";

        StringBuilder output = new StringBuilder();
        output.append("[");

        for (T gene : genes) {
            output.append(gene.toString()).append(" ");
        }

        output.deleteCharAt(output.length() - 1);
        output.append("]");

        return output.toString();
    }
}