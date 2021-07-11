package it.unipi.jenoma.population;

import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;


/**
 * Class representing an individual inside the genetic domain of the problem.
 * An individual is defined as the combination of a chromosome and a fitness value.<br>
 * The default fitness of an individual is 0.
 */
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

    /**
     * Returns a deep clone of the individual exploiting the <code>SerializationUtils</code> provided in
     * <code>org.apache.commons.lang3.SerializationUtils</code>.
     * @return  a deep clone of the individual.
     */
    public Individual clone() {
        return SerializationUtils.clone(this);
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
}