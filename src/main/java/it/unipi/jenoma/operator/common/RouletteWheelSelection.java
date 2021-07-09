package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;


public class RouletteWheelSelection implements Selection {
    private final int numberOfIndividuals;


    public RouletteWheelSelection(int numberOfIndividuals) {
        if (numberOfIndividuals <= 0)
            throw new IllegalArgumentException("The number of individuals to select must be greater than 0.");

        this.numberOfIndividuals = numberOfIndividuals;
    }

    @Override
    public Population select(Population population, PRNG prng, ClusterLogger logger) {
        Population matingPool = new Population(new ArrayList<>(numberOfIndividuals));

        for (int i = 0; i < this.numberOfIndividuals; i++) {
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
