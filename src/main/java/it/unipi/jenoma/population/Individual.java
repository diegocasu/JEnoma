package it.unipi.jenoma.population;

import java.io.Serializable;


public class Individual<T extends Chromosome<?>> implements Comparable<Individual<T>>, Cloneable, Serializable {
    private final T chromosome;
    private double fitness;


    public Individual(T chromosome) {
        this.chromosome = chromosome;
        this.fitness = 0;
    }

    public Chromosome getChromosome() {
        return chromosome;
    }

    public double getFitness() {
        return fitness;
    }

    public void setFitness(double fitness) {
        this.fitness = fitness;
    }

    @Override
    public int compareTo(Individual<T> o) {
        return Double.compare(this.fitness, o.getFitness());
    }

    @Override
    public String toString() {
        return "it.unipi.jenoma.population.Individual{" +
                "it.unipi.jenoma.population.Chromosome=" + chromosome +
                ", fitness=" + fitness +
                '}';
    }

    @Override
    public Individual<T> clone() {
        try {
            super.clone();
            Individual<T> i = new Individual<>((T)this.chromosome.clone());
            i.setFitness(this.getFitness());
            return i;
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            return null;
        }
    }
}