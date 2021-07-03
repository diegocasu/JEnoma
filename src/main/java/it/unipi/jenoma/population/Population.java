package it.unipi.jenoma.population;

import java.util.Iterator;
import java.util.List;


public class Population<T extends Individual<?>> implements Iterable<T> {
    private final List<T> individuals;


    public Population(List<T> individuals) {
        this.individuals = individuals;
    }

    public void addIndividual(T individual) {
        individuals.add(individual);
    }

    public Iterator<T> iterator() {
        return new Iterator<>() {
            private final Iterator<T> iterator = individuals.iterator();

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
                throw new UnsupportedOperationException("The removal of individuals is not supported");
            }
        };
    }

    @Override
    public String toString() {
        StringBuilder output = new StringBuilder();

        for (T individual : individuals) {
            output.append(individual).append("\n");
        }

        output.deleteCharAt(output.length() - 1);
        return output.toString();
    }
}
