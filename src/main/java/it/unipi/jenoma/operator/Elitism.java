package it.unipi.jenoma.operator;

import java.io.Serializable;


public class Elitism implements Serializable {
    private final int numberOfIndividuals;

    public Elitism(int numberOfIndividuals) {
        this.numberOfIndividuals = numberOfIndividuals;
    }

    public int getNumberOfIndividuals() {
        return numberOfIndividuals;
    }
}
