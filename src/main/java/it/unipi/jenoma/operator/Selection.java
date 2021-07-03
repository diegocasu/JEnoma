package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

public interface Selection<T extends Population<?>> {

    T select(T population, PRNG prng);
}
