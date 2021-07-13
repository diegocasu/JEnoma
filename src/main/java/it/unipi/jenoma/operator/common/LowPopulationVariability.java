package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;

import java.util.HashMap;
import java.util.List;


/**
 * Termination condition that stops the algorithm when the average fitness of the population
 * composing the current generation is greater than or equal to a given percentage
 * of the fitness of its best individual.
 */
public class LowPopulationVariability implements TerminationCondition<HashMap<String, Object>> {
    private final double percentage;


    /**
     * Creates a new <code>LowPopulationVariability</code> operator.
     * @param percentage  the percentage used to compute the fraction of the
     *                    best individual's fitness. It must be between 0 and 1.
     * @throws IllegalArgumentException  if the percentage is not between 0 and 1.
     */
    public LowPopulationVariability(double percentage) {
        if (percentage < 0 || percentage > 1)
            throw new IllegalArgumentException("The threshold must be a number between 0 and 1.");

        this.percentage = percentage;
    }

    @Override
    public HashMap<String, Object> map(Population population, int generationsElapsed, ClusterLogger logger) {
        Individual bestIndividual = null;
        double fitnessAccumulator = 0.0;

        for (Individual individual : population) {
            fitnessAccumulator += individual.getFitness();

            if (bestIndividual == null)
                bestIndividual = individual;
            else if (bestIndividual.getFitness() < individual.getFitness())
                bestIndividual = individual;
        }

        HashMap<String, Object> result = new HashMap<>();
        result.put("fitnessAccumulator", fitnessAccumulator);
        result.put("populationLength", population.getLength());
        result.put("bestIndividual", bestIndividual);

        return result;
    }

    @Override
    public boolean end(List<HashMap<String, Object>> partialConditions, ClusterLogger logger) {
        Individual bestIndividual = null;
        double totalFitness = 0.0;
        int totalPopulationLength = 0;

        for (HashMap<String, Object> partialCondition : partialConditions) {
            totalFitness += (Double) partialCondition.get("fitnessAccumulator");
            totalPopulationLength += (Integer) partialCondition.get("populationLength");
            Individual individual = (Individual) partialCondition.get("bestIndividual");

            if (bestIndividual == null)
                bestIndividual = individual;
            else if (bestIndividual.getFitness() < individual.getFitness())
                bestIndividual = individual;
        }

        double averageFitness = totalFitness/totalPopulationLength;
        logger.log(String.format(
                "Average fitness: %s. Best individual fitness: %s.",
                averageFitness,
                bestIndividual.getFitness()));

        return averageFitness >= percentage*bestIndividual.getFitness();
    }
}
