package it.unipi.jenoma.population;

import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;
import java.util.Iterator;
import java.util.List;


public class Population implements Iterable<Individual>, Serializable {
    private final List<Individual> individuals;
    private double fitness;


    public Population(List<Individual> individuals) {
        this.individuals = individuals;
        computeFitness();
    }

    public void addIndividual(Individual individual) {
        individuals.add(individual);
        this.fitness += individual.getFitness();
    }

    public void removeIndividual(Individual individual) {
        this.fitness -= individual.getFitness();
        individuals.remove(individual);
    }

    public void removeAll() {
        individuals.clear();
        this.fitness = 0;
    }

    public Iterator<Individual> iterator() {
        return new Iterator<>() {
            private final Iterator<Individual> iterator = individuals.iterator();

            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public Individual next() {
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

        for (Individual individual : individuals) {
            output.append(individual).append("\n");
        }

        output.deleteCharAt(output.length() - 1);
        return output.toString();
    }

    public double getFitness() {
        return fitness;
    }

    public void setFitness(double fitness) {
        this.fitness = fitness;
    }

    public void computeFitness(){
        double accumulator = 0;

        for(Individual i : individuals)
            accumulator += i.getFitness();

        this.fitness = accumulator;
    }

    public int getLength() {
        return this.individuals.size();
    }

    public Individual getIndividual(int i) {
        return this.individuals.get(i);
    }

    // Returns a view of the portion of the population between the specified fromIndex, inclusive, and toIndex, exclusive.
    public List<Individual> getIndividuals(int fromIndex, int toIndex) {
        return this.individuals.subList(fromIndex, toIndex);
    }

    public Population clone() {
        return SerializationUtils.clone(this);
    }
}
