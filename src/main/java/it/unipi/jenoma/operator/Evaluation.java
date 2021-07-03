package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Individual;


public interface Evaluation<T extends Individual<?>> {

    double evaluate(T individual);
}
