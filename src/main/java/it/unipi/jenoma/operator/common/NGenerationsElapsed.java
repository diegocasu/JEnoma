package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Population;

import java.util.List;


public class NGenerationsElapsed implements TerminationCondition<Boolean> {
    private final int maxNumberOfGenerations;


    public NGenerationsElapsed(int maxNumberOfGenerations) {
        this.maxNumberOfGenerations = maxNumberOfGenerations;
    }

    @Override
    public Boolean map(Population population, int numberOfIterations) {
        return maxNumberOfGenerations > numberOfIterations;
    }

    @Override
    public boolean end(List<Boolean> partialConditions) {
        return partialConditions.get(0);
    }
}
