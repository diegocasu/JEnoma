package it.unipi.jenoma.population;

import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;
import java.util.Iterator;
import java.util.List;


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

    public void cross(Chromosome<T> c, int from, int to) {
       for (int i = from; i <= to; i++)
           this.genes.set(i, c.getGene(i));
    }

    @Override
    public String toString() {
        StringBuilder output = new StringBuilder();
        output.append("[");

        for (T gene : genes) {
            output.append(gene.toString()).append(" ");
        }

        output.deleteCharAt(output.length() - 1);
        output.append("]");

        return output.toString();
    }

    public Chromosome<T> clone() {
        return SerializationUtils.clone(this);
    }
}