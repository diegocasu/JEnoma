package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Population;

import java.io.Serializable;
import java.util.List;


// TODO: implement
public class LowPopulationVariability<T extends Serializable> implements TerminationCondition<T> {
    @Override
    public T map(Population population, int numberOfIterations, ClusterLogger logger) {
        return null;
    }

    @Override
    public boolean end(List<T> partialConditions, ClusterLogger logger) {
        return false;
    }
}
