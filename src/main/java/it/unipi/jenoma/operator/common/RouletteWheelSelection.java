package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

public class RouletteWheelSelection<T extends Population<?>> implements Selection<T> {

    @Override
    public T select(T population, PRNG prng) {
        return null;
    }
}
