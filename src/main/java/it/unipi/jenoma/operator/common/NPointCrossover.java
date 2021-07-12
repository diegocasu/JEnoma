package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.population.Chromosome;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;
import java.util.List;


/**
 * Crossover operator that implements the N-point crossover strategy,
 * generating two children for each couple of parents.
 */
public class NPointCrossover implements Crossover {
    private final int n;


    /**
     * Creates a new <code>NPointCrossover</code> operator.
     * @param n  the number of split points. It must be greater than 0.
     * @throws IllegalArgumentException  if the number of split points is less than or equal to 0.
     */
    public NPointCrossover(int n) {
        if (n <= 0)
            throw new IllegalArgumentException("The number of split points must be greater than 0.");

        this.n = n;
    }

    /**
     * @return  an empty offspring if the chromosomes of the two parents have different length
     *          or the number of split points is greater than the length of the chromosomes,
     *          an offspring composed of two children otherwise.
     */
    @Override
    public List<Individual> cross(Individual parent1, Individual parent2, PRNG prng, ClusterLogger logger) {
        List<Individual> offspring = new ArrayList<>();

        if (parent1.getChromosome().getLength() != parent2.getChromosome().getLength()) {
            logger.log(String.format(
                    "Cannot perform crossover on parents with different chromosome length [%s != %s].",
                    parent1.getChromosome().getLength(), parent2.getChromosome().getLength()));
            return offspring;
        }

        if (this.n > parent1.getChromosome().getLength()) {
            logger.log(String.format(
                    "Cannot perform %s-point crossover on chromosomes of length %s.",
                    this.n, parent1.getChromosome().getLength()));
            return offspring;
        }

        offspring.add(parent1);
        offspring.add(parent2);

        int crossPoint = 0;
        int old = 0;
        int maxPartitionSize = parent1.getChromosome().getLength() - (this.n - 1);
        boolean isOddTurn = true;

        for (int partitionsLeft = this.n - 1; partitionsLeft > 0; partitionsLeft--) {
            crossPoint += (maxPartitionSize == 1) ? 1 : prng.nextInt(maxPartitionSize);

            if (isOddTurn) {
                offspring.get(0).getChromosome().cross((Chromosome) parent1.getChromosome(), old, crossPoint);
                offspring.get(1).getChromosome().cross((Chromosome) parent2.getChromosome(), old, crossPoint);
            } else {
                offspring.get(0).getChromosome().cross((Chromosome) parent2.getChromosome(), old, crossPoint);
                offspring.get(1).getChromosome().cross((Chromosome) parent1.getChromosome(), old, crossPoint);
            }

            int availableSpace = (parent1.getChromosome().getLength() - crossPoint);
            maxPartitionSize = availableSpace - partitionsLeft;
            old = crossPoint;
            isOddTurn = !isOddTurn;
        }

        int chromLength = parent1.getChromosome().getLength() - 1;

        if (isOddTurn) {
            offspring.get(0).getChromosome().cross((Chromosome) parent1.getChromosome(), crossPoint, chromLength);
            offspring.get(1).getChromosome().cross((Chromosome) parent2.getChromosome(), crossPoint, chromLength);
        } else {
            offspring.get(0).getChromosome().cross((Chromosome) parent2.getChromosome(), crossPoint, chromLength);
            offspring.get(1).getChromosome().cross((Chromosome) parent1.getChromosome(), crossPoint, chromLength);
        }

        return offspring;
    }
}
