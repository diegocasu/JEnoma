package it.unipi.jenoma.population;

import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;


public class Individual implements Comparable<Individual>, Serializable {
    private final Chromosome<?> chromosome;
    private double fitness;


    public Individual(Chromosome<?> chromosome) {
        this.chromosome = chromosome;
        this.fitness = 0;
    }

    public Chromosome<?> getChromosome() {
        return chromosome;
    }

    public double getFitness() {
        return fitness;
    }

    public void setFitness(double fitness) {
        this.fitness = fitness;
    }

    @Override
    public int compareTo(Individual o) {
        return Double.compare(this.fitness, o.getFitness());
    }

    @Override
    public String toString() {
        return "Individual{" +
                "chromosome=" + chromosome +
                ", fitness=" + fitness +
                '}';
    }

    public Individual clone() {
        return SerializationUtils.clone(this);
    }
}