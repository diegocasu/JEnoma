package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;


public class RouletteWheelSelection implements Selection {
    private final int chromosomeChosenForReproduction;


    public RouletteWheelSelection(int ccfr) {
        this.chromosomeChosenForReproduction = ccfr;
    }

    @Override
    public Population select(Population population, PRNG prng) {
        Population matingPool = new Population(new ArrayList<>(chromosomeChosenForReproduction));

        for (int i = 0; i < this.chromosomeChosenForReproduction; i++) {
            double populationFitness = population.getFitness();
            double randomDouble = prng.nextDouble()*populationFitness;
            double partialSum = 0;

            for (Individual individual : population) {
                partialSum += individual.getFitness();

                if (partialSum > randomDouble) {
                    matingPool.addIndividual(individual);
                    population.removeIndividual(individual);
                }
            }
        }

        return matingPool;
    }
}
