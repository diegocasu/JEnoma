package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Population;

import java.io.Serializable;
import java.util.List;


public class LowPopulationVariability<T extends Serializable> implements TerminationCondition<T> {
    @Override
    public T map(Population population, int numberOfIterations) {
        return null;
    }

    @Override
    public boolean end(List<T> partialConditions) {
        return false;
    }
}
