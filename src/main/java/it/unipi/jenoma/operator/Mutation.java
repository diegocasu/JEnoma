package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Individual;


public interface Mutation<T extends Individual<?>> {

    void mutate(T individual);
}
