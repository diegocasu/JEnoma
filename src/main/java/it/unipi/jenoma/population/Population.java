package it.unipi.jenoma.population;

import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;


/**
 * Class representing a population of individuals inside the genetic domain of the problem.
 */
public class Population implements Iterable<Individual>, Serializable {
    private final List<Individual> individuals;


    public Population(List<Individual> individuals) {
        this.individuals = individuals;
    }

    public int getSize() {
        return this.individuals.size();
    }

    public Individual getIndividual(int index) {
        return this.individuals.get(index);
    }

    /**
     * Returns a view of the portion of the population between the specified <code>fromIndex</code>
     * (inclusive) and <code>toIndex</code> (exclusive).
     * @param fromIndex  the lower bound (inclusive) of the interval.
     * @param toIndex    the upper bound (exclusive) of the interval.
     * @return           a view of the population in the given interval.
     */
    public List<Individual> getIndividuals(int fromIndex, int toIndex) {
        return this.individuals.subList(fromIndex, toIndex);
    }

    public void setIndividual(int index, Individual individual) {
        individuals.set(index, individual);
    }

    public void addIndividual(Individual individual) {
        individuals.add(individual);
    }

    public void addIndividuals(List<Individual> individuals) {
        for (Individual individual : individuals)
            addIndividual(individual);
    }

    public void removeIndividual(int index) {
        individuals.remove(index);
    }

    /**
     * Removes the individuals of the population in the interval between
     * <code>fromIndex</code> (inclusive) and <code>toIndex</code> (exclusive).
     * @param fromIndex  the lower bound (inclusive) of the interval.
     * @param toIndex    the upper bound (exclusive) of the interval.
     */
    public void removeIndividuals(int fromIndex, int toIndex) {
        individuals.subList(fromIndex, Math.min(toIndex, individuals.size())).clear();
    }

    public Iterator<Individual> iterator() {
        return individuals.iterator();
    }

    /**
     * Sorts the individuals inside the population by ascending fitness value.
     */
    public void sortByAscendingFitness() {
        individuals.sort(null);
    }

    /**
     * Sorts the individuals inside the population by descending fitness value.
     */
    public void sortByDescendingFitness() {
        individuals.sort(Comparator.comparingDouble(Individual::getFitness).reversed());
    }

    /**
     * Returns a deep clone of the population exploiting the <code>SerializationUtils</code> provided in
     * <code>org.apache.commons.lang3.SerializationUtils</code>.
     * @return  a deep clone of the population.
     */
    public Population clone() {
        return SerializationUtils.clone(this);
    }

    @Override
    public String toString() {
        if (individuals == null)
            return "NULL";

        if (individuals.size() == 0)
            return "[]";

        StringBuilder output = new StringBuilder();

        for (Individual individual : individuals) {
            output.append(individual).append("\n");
        }

        output.deleteCharAt(output.length() - 1);
        return output.toString();
    }
}
