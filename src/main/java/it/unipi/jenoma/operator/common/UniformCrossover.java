package it.unipi.jenoma.operator.common;


import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.util.List;

public class UniformCrossover<T extends Individual<?>> implements Crossover<T> {

    @Override
    public List<T> crossover(T parent1, T parent2, PRNG prng) {
        return null;
    }
}
