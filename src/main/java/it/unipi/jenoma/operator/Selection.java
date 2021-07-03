package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Population;


public interface Selection<T extends Population<?>> {

    T select(T population);
}
