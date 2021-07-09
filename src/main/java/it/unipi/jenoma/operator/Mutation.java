package it.unipi.jenoma.operator;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;


public interface Mutation extends Serializable {

    void mutate(Individual individual, PRNG prng, ClusterLogger logger);
}
