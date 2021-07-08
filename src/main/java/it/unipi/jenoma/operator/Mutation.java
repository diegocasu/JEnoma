package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;


public interface Mutation<T extends Individual<?>> extends Serializable {

    void mutate(T individual, PRNG prng);
}
