package it.unipi.jenoma.operator;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;
import java.util.List;


/**
 * A crossover function that recombines couples of individuals to generate children.
 * The crossover function is applied to randomly chosen parents until the generated offspring
 * is as large as the initial population. Mind that returning by mistake an empty children list
 * at each execution will cause the computation to loop on a machine until the timeout expires.
 */
public interface Crossover extends Serializable {

    /**
     * Recombines couples of individuals to generate children. One child, two children or more
     * than two children are allowed.
     * @param parent1  the first parent.
     * @param parent2  the second parent.
     * @param prng     a pseudorandom number generator.
     * @param logger   a logger that can be used to send log messages to the coordinator.
     * @return         a list of individuals representing the children.
     */
    List<Individual> crossover(Individual parent1, Individual parent2, PRNG prng, ClusterLogger logger);
}
