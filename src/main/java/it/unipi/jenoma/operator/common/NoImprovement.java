package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;

import java.util.List;


public class NoImprovement implements TerminationCondition<Boolean> {
    private final int maxGenerationsWithoutImprovement;
    private int generationsWithoutImprovement;
    private double bestFitness;


    public NoImprovement(int maxGenerationsWithoutImprovement) {
        this.maxGenerationsWithoutImprovement = maxGenerationsWithoutImprovement;
        generationsWithoutImprovement = 0;
        bestFitness = 0;
    }

    @Override
    public Boolean map(Population population, int numberOfIterations) {
        double currentBestFitness = 0;

        for (Individual individual : population) {
            if (individual.getFitness() > currentBestFitness)
                currentBestFitness = individual.getFitness();
        }

        if (currentBestFitness > bestFitness) {
            bestFitness = currentBestFitness;
            generationsWithoutImprovement = 0;
            return false;
        }

        generationsWithoutImprovement++;
        return generationsWithoutImprovement > maxGenerationsWithoutImprovement;
    }

    @Override
    public boolean end(List<Boolean> partialConditions) {
        for (Boolean improvement : partialConditions) {
            if (improvement)
                return false;
        }
        return true;
    }
}


