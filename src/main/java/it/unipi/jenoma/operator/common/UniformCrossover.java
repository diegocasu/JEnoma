package it.unipi.jenoma.operator.common;


import it.unipi.jenoma.algorithm.AlgorithmException;
import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.population.Chromosome;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;
import java.util.List;

public class UniformCrossover<T extends Individual<?>> implements Crossover<T> {

    private double crossOverProbability_parent1;
    private double crossOverProbability_parent2;

    public UniformCrossover(double crossOverProbability_parent1, double crossOverProbability_parent2) {
        this.crossOverProbability_parent1 = crossOverProbability_parent1;
        this.crossOverProbability_parent2 = crossOverProbability_parent2;
    }

    @Override
    public List<T> crossover(T parent1, T parent2, PRNG prng) {
        List<T> offspring = new ArrayList<>();
        offspring.add(parent1);
        offspring.add(parent2);

        if (parent1.getChromosome().getLength() != parent2.getChromosome().getLength()) {
            throw new AlgorithmException("UNIFORMCROSSOVER: parent chromosome size must coincide");
        }

        if (this.crossOverProbability_parent1 > 1 || this.crossOverProbability_parent2 > 1) {
            throw new AlgorithmException("UNIFORMCROSSOVER: crossOverProbability not valid");
        }

        for(int i = 0; i< parent1.getChromosome().getLength(); i++){
            double outcome = Math.abs((double) prng.nextInt(1));
            if(outcome > (1 - crossOverProbability_parent1)){
                offspring.get(0).getChromosome().cross(parent2.getChromosome(), i, i);
            }
            if(outcome > (1 - crossOverProbability_parent2)){
                offspring.get(1).getChromosome().cross(parent1.getChromosome(), i, i);
            }
        }

        return offspring;
    }
}
