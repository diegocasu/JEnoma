package it.unipi.jenoma.population;

import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;


public class Population implements Iterable<Individual>, Serializable {
    private final List<Individual> individuals;


    public Population(List<Individual> individuals) {
        this.individuals = individuals;
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

    public void removeIndividuals(int fromIndex, int toIndex) {
        individuals.subList(fromIndex, Math.min(toIndex, individuals.size())).clear();
    }

    public Iterator<Individual> iterator() {
        return individuals.iterator();
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

    public int getLength() {
        return this.individuals.size();
    }

    public Individual getIndividual(int i) {
        return this.individuals.get(i);
    }

    // Returns a view of the portion of the population between the specified fromIndex, inclusive, and toIndex, exclusive.
    public List<Individual> getIndividuals(int fromIndex, int toIndex) {
        return this.individuals.subList(fromIndex, toIndex);
    }

    public void setIndividual(int index, Individual individual) {
        individuals.set(index, individual);
    }

    public void sortByAscendingFitness() {
        individuals.sort(null);
    }

    public void sortByDescendingFitness() {
        individuals.sort(Comparator.comparingDouble(Individual::getFitness).reversed());
    }

    public Population clone() {
        return SerializationUtils.clone(this);
    }
}
