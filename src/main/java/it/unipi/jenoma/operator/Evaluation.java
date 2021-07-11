package it.unipi.jenoma.operator;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.population.Individual;

import java.io.Serializable;


/**
 * An evaluation function that computes the fitness of individuals.
 */
public interface Evaluation extends Serializable {

    /**
     * Computes the fitness of an individual.
     * @param individual  the individual to be evaluated.
     * @param logger      a logger that can be used to send log messages to the coordinator.
     * @return            the fitness of the individual.
     */
    double evaluate(Individual individual, ClusterLogger logger);
}
