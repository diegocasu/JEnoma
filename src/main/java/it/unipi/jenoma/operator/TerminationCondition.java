package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Population;

import java.io.Serializable;
import java.util.List;


public interface TerminationCondition<T extends Serializable> extends Serializable {

    T map(Population population, int numberOfIterations);

    boolean end(List<T> partialConditions);
}

