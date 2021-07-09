package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;

import java.util.List;


public class DesiredFitnessAchieved implements TerminationCondition<Boolean> {
    private final double desiredFitness;


    public DesiredFitnessAchieved(double desiredFitness) {
        this.desiredFitness = desiredFitness;
    }

    @Override
    public Boolean map(Population population, int generationsElapsed, ClusterLogger logger) {
        double bestFitnessAchieved = population.getIndividual(0).getFitness();

        for (Individual individual : population) {
            if (individual.getFitness() > bestFitnessAchieved)
                bestFitnessAchieved = individual.getFitness();

            if (individual.getFitness() >= desiredFitness) {
                logger.log(String.format("Desired fitness achieved: %s.", individual.getFitness()));
                return true;
            }
        }

        logger.log(String.format("Desired fitness %s not achieved. Best fitness: %s.", desiredFitness, bestFitnessAchieved));
        return false;
    }

    @Override
    public boolean end(List<Boolean> partialConditions, ClusterLogger logger) {
        for (Boolean fitnessAchieved : partialConditions) {
            if (fitnessAchieved)
                return true;
        }
        return false;
    }
}
