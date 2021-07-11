package it.unipi.jenoma.operator;

import java.io.Serializable;


public class Elitism implements Serializable {
    private final int numberOfIndividuals;


    public Elitism(int numberOfIndividuals) {
        if (numberOfIndividuals < 0)
            throw new IllegalArgumentException("The number of individuals must be greater than or equal to 0.");

        this.numberOfIndividuals = numberOfIndividuals;
    }

    public int getNumberOfIndividuals() {
        return numberOfIndividuals;
    }
}
