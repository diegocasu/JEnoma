package it.unipi.jenoma.operator;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.population.Population;

import java.io.Serializable;
import java.util.List;


/**
 * A termination condition for the genetic algorithm.<br>
 * The termination condition is evaluated following a map-reduce approach: each remote
 * machine computes a partial condition executing in isolation the <code>map</code> method
 * on its assigned portion of the population; then, all the partial results are collected and
 * sent to the coordinator node, which will use them in the <code>end</code> method.
 * @param <T>  the class representing the type returned by the <code>map</code> method.
 */
public interface TerminationCondition<T extends Serializable> extends Serializable {

    /**
     * Computes a partial termination condition in isolation on a remote machine, based
     * on the assigned portion of population.
     * @param population          the portion of population assigned to this machine.
     * @param generationsElapsed  the number of generations elapsed in the algorithm
     *                            at the time of the invocation.
     * @param logger              a logger that can be used to send log messages to the coordinator.
     * @return                    a partial termination condition.
     */
    T map(Population population, int generationsElapsed, ClusterLogger logger);

    /**
     * Checks if the termination condition of the algorithm is met, based on the list of partial
     * termination conditions computed by the remote machines. This method is executed by the coordinator.
     * @param partialConditions  the list of termination conditions computed
     *                           in isolation by the remote machines.
     * @param logger             a logger that can be used to print log messages.
     * @return                   true if the termination condition is met, false otherwise.
     */
    boolean end(List<T> partialConditions, ClusterLogger logger);
}

