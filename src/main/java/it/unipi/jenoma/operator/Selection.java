package it.unipi.jenoma.operator;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;


/**
 * A selection function that chooses which individuals will enter the mating pool.
 * The selection function is executed in a single thread on the portion of population
 * assigned to the remote machine, meaning that the result of <code>select</code> will be
 * the mating pool used by that remote machine.
 */
public interface Selection extends Serializable {

    /**
     * Selects a list of individuals from a population to form the mating pool
     * used by this remote machine.
     * @param population  the portion of population assigned to this machine.
     * @param prng        a pseudorandom number generator.
     * @param logger      a logger that can be used to send log messages to the coordinator.
     * @return            a population representing the mating pool.
     */
    Population select(Population population, PRNG prng, ClusterLogger logger);
}
