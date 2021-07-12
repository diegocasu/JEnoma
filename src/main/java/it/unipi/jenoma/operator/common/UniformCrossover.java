package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.population.Chromosome;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;
import java.util.List;


/**
 * Crossover operator that implements the uniform crossover strategy,
 * generating two children for each couple of parents.
 */
public class UniformCrossover implements Crossover {
    private final double crossoverProbability;

    /**
     * Creates a new <code>UniformCrossover</code> operator.
     * @param crossoverProbability  the probability that a child will get the selected gene
     *                              from the first parent. It must be between 0 and 1.
     * @throws IllegalArgumentException  if the crossover probability is not between 0 and 1.
     */
    public UniformCrossover(double crossoverProbability) {
        if (crossoverProbability < 0 || crossoverProbability > 1)
            throw new IllegalArgumentException("The crossover probability must be a number between 0 and 1.");

        this.crossoverProbability = crossoverProbability;
    }

    /**
     * @return  an empty offspring if the chromosomes of the two parents have different length,
     *          an offspring composed of two children otherwise.
     */
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
