package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;


public interface Selection<T extends Population<?>> extends Serializable {

    T select(T population, PRNG prng);
}
