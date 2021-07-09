package it.unipi.jenoma.operator.common;

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
    public Boolean map(Population population, int numberOfIterations) {
        for (Individual individual : population) {
            if (individual.getFitness() >= desiredFitness)
                return true;
        }
        return false;
    }

    @Override
    public boolean end(List<Boolean> partialConditions) {
        for (Boolean fitnessAchieved : partialConditions) {
            if (fitnessAchieved)
                return true;
        }
        return false;
    }
}
