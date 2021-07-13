package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;

import java.util.List;


/**
 * Termination condition that stops the algorithm if the best fitness achieved
 * does not increase for more than <i>N</i> consecutive generations.
 */
public class NoImprovement implements TerminationCondition<Individual> {
    // Fields accessed only in the reduce phase by the coordinator.
    private Individual bestIndividualAcrossGenerations;
    private final int maxGenerationsWithoutImprovement;
    private int generationsWithoutImprovement;


    /**
     * Creates a new <code>NoImprovement</code> operator.
     * @param maxGenerationsWithoutImprovement  the allowed maximum number of consecutive generations
     *                                          without having an improvement in the best fitness.
     *                                          It must be greater than 0.
     * @throws IllegalArgumentException  if the given number of generations is less than or equal to 0.
     */
    public NoImprovement(int maxGenerationsWithoutImprovement) {
        if (maxGenerationsWithoutImprovement <= 0)
            throw new IllegalArgumentException("The number of generations must be greater than 0.");

        this.maxGenerationsWithoutImprovement = maxGenerationsWithoutImprovement;
        this.generationsWithoutImprovement = 0;
        this.bestIndividualAcrossGenerations = null;
    }

    @Override
    public Individual map(Population population, int generationsElapsed, ClusterLogger logger) {
        Individual bestIndividual = null;

        for (Individual individual : population) {
            if (bestIndividual == null)
                bestIndividual = individual;
            else if (bestIndividual.getFitness() < individual.getFitness())
                bestIndividual = individual;
        }

        return bestIndividual;
    }

    @Override
    public boolean end(List<Individual> bestIndividuals, ClusterLogger logger) {
        Individual bestIndividual = null;

        for (Individual individual : bestIndividuals) {
            if (bestIndividual == null)
                bestIndividual = individual;
            else if (bestIndividual.getFitness() < individual.getFitness())
                bestIndividual = individual;
        }

        if (bestIndividualAcrossGenerations == null) {
            bestIndividualAcrossGenerations = bestIndividual;
            logger.log(String.format(
                    "Initial best individual. Fitness %s.",
                    bestIndividualAcrossGenerations.getFitness()));
            return false;
        }

        logger.log(String.format(
                "The best individual of this generation has fitness %s.",
                bestIndividual.getFitness()));

        if (bestIndividual.getFitness() > bestIndividualAcrossGenerations.getFitness()) {
            logger.log(String.format(
                    "The fitness improved: %s > %s.",
                    bestIndividual.getFitness(),
                    bestIndividualAcrossGenerations.getFitness()));

            bestIndividualAcrossGenerations = bestIndividual;
            generationsWithoutImprovement = 0;
            return false;
        }

        generationsWithoutImprovement++;
        logger.log(String.format(
                "The fitness did not improve. Number of generations without an improvement: %s.",
                generationsWithoutImprovement));
        return generationsWithoutImprovement > maxGenerationsWithoutImprovement;
    }
}


