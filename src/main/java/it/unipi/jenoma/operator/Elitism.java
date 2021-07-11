package it.unipi.jenoma.operator;

import java.io.Serializable;


/**
 * An elitism criterion to be applied at each generation of the genetic algorithm.
 * The operator is applied taking into consideration the entire population at a certain generation,
 * i.e. the entire set of population chunks scattered between remote machines: the overall <i>N</i> fittest
 * individuals are preserved, at the expense of the overall <i>N</i> worst individuals.
 */
public class Elitism implements Serializable {
    private final int numberOfIndividuals;

    /**
     * Creates an elitism operator that preserves a chosen number of fittest individuals
     * between generations. If the chosen number is 0, the operator is not applied.
     * @param numberOfIndividuals  the number of fittest individuals to be preserved.
     *                             Must be greater than or equal to 0.
     * @throws IllegalArgumentException  if the number of individuals is less than 0.
     */
    public Elitism(int numberOfIndividuals) {
        if (numberOfIndividuals < 0)
            throw new IllegalArgumentException("The number of individuals must be greater than or equal to 0.");

        this.numberOfIndividuals = numberOfIndividuals;
    }

    public int getNumberOfIndividuals() {
        return numberOfIndividuals;
    }
}
