package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;
import java.util.List;


public interface Crossover<T extends Individual<?>> extends Serializable {

    List<T> crossover(T parent1, T parent2, PRNG prng);
}
