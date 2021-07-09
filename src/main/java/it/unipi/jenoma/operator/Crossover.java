package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;
import java.util.List;


public interface Crossover extends Serializable {

    List<Individual> crossover(Individual parent1, Individual parent2, PRNG prng);
}
