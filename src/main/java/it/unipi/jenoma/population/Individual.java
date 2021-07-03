package it.unipi.jenoma.population;


public class Individual<T extends Chromosome<?>> implements Comparable<Individual<T>>{
    private final T chromosome;
    private double fitness;


    public Individual(T chromosome) {
        this.chromosome = chromosome;
        this.fitness = 0;
    }

    public T getChromosome() {
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
}