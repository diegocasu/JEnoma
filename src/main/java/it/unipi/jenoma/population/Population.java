package it.unipi.jenoma.population;

import it.unipi.jenoma.algorithm.AlgorithmException;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


public class Population<T extends Individual<?>> implements Iterable<T>, Cloneable, Serializable {
    private final List<T> individuals;
    private double fitness;

    public Population(List<T> individuals) {
        this.individuals = individuals;
        this.fitness = 0;
    }

    public void addIndividual(Individual individual) {
        individuals.add((T)individual);
        this.fitness += individual.getFitness();
    }


    public void removeIndividual(Individual i) {
        this.fitness -= i.getFitness();
        individuals.remove(i);
    }

    public Population<T> removeAll(){
        individuals.clear();
        this.fitness=0;
        return this;
    }

    public Iterator<T> iterator() {
        return new Iterator<T>() {
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

    public double getFitness() {
        return fitness;
    }

    public void computeFitness(){
        double acc = 0;
        for(Individual i : individuals){
            acc += i.getFitness();
        }
        this.fitness = acc;
    }

    public void setFitness(double fitness) {
        this.fitness = fitness;
    }

    public Population<T> clone(){
        try {
            super.clone();
            List<T> i = new ArrayList<T>();
            i.addAll(this.individuals);
            Population<T> p = new Population<>(i);
            p.setFitness(this.fitness);
            return p;
        } catch (CloneNotSupportedException e) {
            throw new AlgorithmException(e);
        }
    }

    public int getLength() {
        return this.individuals.size();
    }

    public T getIndividual(int i) {
        return this.individuals.get(i);
    }

    // Returns a view of the portion of the population between the specified fromIndex, inclusive, and toIndex, exclusive.
    public List<T> getIndividuals(int fromIndex, int toIndex) {
        return this.individuals.subList(fromIndex, toIndex);
    }
}
