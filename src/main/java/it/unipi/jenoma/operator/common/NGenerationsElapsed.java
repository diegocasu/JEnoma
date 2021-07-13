package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Population;

import java.util.List;


/**
 * Termination condition that stops the algorithm when <i>N</i> generations/iterations elapsed.
 */
public class NGenerationsElapsed implements TerminationCondition<Boolean> {
    private final int maxNumberOfGenerations;

    /**
     * Creates a new <code>NGenerationsElapsed</code> operator.
     * @param maxNumberOfGenerations  the maximum number of iterations of the algorithm.
     *                                It must be greater than 0.
     * @throws IllegalArgumentException  if the given number of generations is less than or equal to 0.
     */
    public NGenerationsElapsed(int maxNumberOfGenerations) {
        if (maxNumberOfGenerations <= 0)
            throw new IllegalArgumentException("The number of generations must be greater than 0.");

        this.maxNumberOfGenerations = maxNumberOfGenerations;
    }

    @Override
    public Boolean map(Population population, int generationsElapsed, ClusterLogger logger) {
        return generationsElapsed >= maxNumberOfGenerations;
    }

    @Override
    public boolean end(List<Boolean> partialConditions, ClusterLogger logger) {
        return partialConditions.get(0);
    }
}
