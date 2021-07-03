package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;


public interface Mutation<T extends Individual<?>> {

    void mutate(T individual, PRNG prng);
}
