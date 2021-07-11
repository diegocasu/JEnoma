package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Population;

import java.util.List;


public class NGenerationsElapsed implements TerminationCondition<Boolean> {
    private final int maxNumberOfGenerations;


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
