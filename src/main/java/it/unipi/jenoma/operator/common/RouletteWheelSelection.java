package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;
import java.util.List;

public class RouletteWheelSelection<T extends Population<?>> implements Selection<T> {

    private T population;
    private T matingPool;
    private int chromosomeChosenForReproduction;
    PRNG prng;

    public RouletteWheelSelection(int ccfr) {
        this.chromosomeChosenForReproduction = ccfr;
    }

    @Override
    public T select(T population, PRNG prng) {
        this.prng = prng;
        if (this.matingPool != null)
            this.matingPool.removeAll();

        this.population = (T) population.clone();
        this.matingPool = (T) population.clone().removeAll();
        for (int i = 0; i < this.chromosomeChosenForReproduction; i++) this.matingPool.addIndividual(select_helper());
        return (T) this.matingPool;
    }

    public Individual<?> select_helper() {
        double populationTotalFitness = this.population.getFitness();
        double spinStartingPoint = this.prng.nextDouble(populationTotalFitness) / populationTotalFitness;
        double offset = spinStartingPoint;
        Individual chosenOne;
        for (Individual i : this.population) {
            offset += i.getFitness();
            if (spinStartingPoint < offset) {
                chosenOne = i.clone();
                this.population.removeIndividual(i);
                return chosenOne;
            }
        }
        return null;
    }
}
