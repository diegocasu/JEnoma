package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;

import java.util.List;


/**
 * Termination condition that stops the algorithm when all the workers <b>at the same time</b>
 * did not improve the best fitness achieved for more than <i>N</i> consecutive generations.
 * The operator does not guarantee that the entire algorithm will stop after <i>N</i>
 * consecutive generations with no improvement in the best global fitness achieved.
 */
public class NoImprovement implements TerminationCondition<Boolean> {
    private final int maxGenerationsWithoutImprovement;
    private int generationsWithoutImprovement;
    private double bestFitness;


    /**
     * Creates a new <code>NoImprovement</code> operator.
     * @param maxGenerationsWithoutImprovement  the allowed maximum number of consecutive generations
     *                                          without an improvement in the best fitness achieved for
     *                                          a single worker. It must be greater than 0.
     * @throws IllegalArgumentException  if the given number of generations is less than or equal to 0.
     */
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


