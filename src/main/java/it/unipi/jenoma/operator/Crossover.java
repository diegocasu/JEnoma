package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Individual;

import java.util.List;


public interface Crossover<T extends Individual<?>> {

    List<T> crossover(T parent1, T parent2);
}
