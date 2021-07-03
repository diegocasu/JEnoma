package it.unipi.jenoma.operator;


public class Elitism {
    private final int numberOfIndividuals;
    private final Strategy strategy;
    public enum Strategy { RANDOM, WORST }


    public Elitism(int numberOfIndividuals, Strategy strategy) {
        this.numberOfIndividuals = numberOfIndividuals;
        this.strategy = strategy;
    }

    public int getNumberOfIndividuals() {
        return numberOfIndividuals;
    }

    public Strategy getStrategy() {
        return strategy;
    }
}
