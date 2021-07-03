package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Population;

import java.util.List;


public interface TerminationCondition<T> {

    T map(Population<?> population, int numberOfIterations);

    boolean end(List<T> partialConditions);
}

