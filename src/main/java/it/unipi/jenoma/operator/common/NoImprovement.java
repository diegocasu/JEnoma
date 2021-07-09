package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;

import java.util.List;


public class NoImprovement implements TerminationCondition<Boolean> {
    private final int maxGenerationsWithoutImprovement;
    private int generationsWithoutImprovement;
    private double bestFitness;


    public NoImprovement(int maxGenerationsWithoutImprovement) {
        if (maxGenerationsWithoutImprovement <= 0)
            throw new IllegalArgumentException("The number of generations must be greater than 0.");

        this.maxGenerationsWithoutImprovement = maxGenerationsWithoutImprovement;
        generationsWithoutImprovement = 0;
        bestFitness = 0;
    }

    @Override
    public Boolean map(Population population, int generationsElapsed, ClusterLogger logger) {
        double currentBestFitness = 0;

        for (Individual individual : population) {
            if (individual.getFitness() > currentBestFitness)
                currentBestFitness = individual.getFitness();
        }

        if (currentBestFitness > bestFitness) {
            logger.log(String.format("The fitness improved: [%s > %s].", currentBestFitness, bestFitness));
            bestFitness = currentBestFitness;
            generationsWithoutImprovement = 0;
            return false;
        }

        generationsWithoutImprovement++;
        logger.log(String.format(
                "The fitness did not improve. Number of generations without an improvement: [%s].",
                generationsWithoutImprovement));

        return generationsWithoutImprovement > maxGenerationsWithoutImprovement;
    }

    @Override
    public boolean end(List<Boolean> partialConditions, ClusterLogger logger) {
        for (Boolean improvement : partialConditions) {
            if (improvement)
                return false;
        }
        return true;
    }
}


