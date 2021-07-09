package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.population.Chromosome;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;
import java.util.List;


public class UniformCrossover implements Crossover {
    private final double crossoverProbability;


    public UniformCrossover(double crossOverProbability) {
        if (crossOverProbability < 0 || crossOverProbability > 1)
            throw new IllegalArgumentException("The crossover probability must be a number between 0 and 1.");

        this.crossoverProbability = crossOverProbability;
    }

    @Override
    public List<Individual> crossover(Individual parent1, Individual parent2, PRNG prng, ClusterLogger logger) {
        List<Individual> offspring = new ArrayList<>();

        if (parent1.getChromosome().getLength() != parent2.getChromosome().getLength()) {
            logger.log(String.format(
                    "Cannot perform crossover on parents with different chromosome length [%s != %s].",
                    parent1.getChromosome().getLength(), parent2.getChromosome().getLength()));
            return offspring;
        }

        offspring.add(parent1);
        offspring.add(parent2);

        for (int i = 0; i < parent1.getChromosome().getLength(); i++) {
            double outcome = prng.nextDouble();
            if (outcome > (1 - crossoverProbability)) {
                offspring.get(0).getChromosome().cross((Chromosome) parent2.getChromosome(), i, i);
            }
            if (outcome > crossoverProbability) {
                offspring.get(1).getChromosome().cross((Chromosome) parent1.getChromosome(), i, i);
            }
        }

        return offspring;
    }
}
