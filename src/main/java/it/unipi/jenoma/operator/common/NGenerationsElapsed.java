package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Population;

import java.util.List;

public class NGenerationsElapsed<T> implements TerminationCondition<T> {
    @Override
    public T map(Population<?> population, int numberOfIterations) {
        return null;
    }

    @Override
    public boolean end(List<T> partialConditions) {
        return false;
    }
}
