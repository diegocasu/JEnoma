package it.unipi.jenoma.operator;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;


public interface Selection extends Serializable {

    Population select(Population population, PRNG prng, ClusterLogger logger);
}
