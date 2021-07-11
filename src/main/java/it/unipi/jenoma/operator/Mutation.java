package it.unipi.jenoma.operator;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;


/**
 * A mutation function that alters the chromosome of an individual.
 */
public interface Mutation extends Serializable {

    /**
     * Mutates the chromosome of an individual.
     * @param individual  the individual to be mutated.
     * @param prng        a pseudorandom number generator.
     * @param logger      a logger that can be used to send log messages to the coordinator.
     */
    void mutate(Individual individual, PRNG prng, ClusterLogger logger);
}
