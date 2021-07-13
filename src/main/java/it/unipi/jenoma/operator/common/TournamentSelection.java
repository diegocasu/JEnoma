package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


/**
 * Selection operator implementing the k-tournament strategy.
 * The operator guarantees that the given number of individuals is selected
 * for each worker in isolation, i.e. on each assigned population portion, and not globally.
 */
public class TournamentSelection implements Selection {
    private final int numberOfIndividuals;
    private final int tournamentSize;


    /**
     * Creates a new <code>TournamentSelection</code> operator.
     * @param numberOfIndividuals  the number of individuals that must be selected.
     *                             It must be greater than 0.
     * @param tournamentSize       the size of the groups of individuals that will compete
     *                             at each round. It must be greater than 0.
     * @throws IllegalArgumentException  if at least one of the two arguments is less than or equal to 0.
     */
    public TournamentSelection(int numberOfIndividuals, int tournamentSize) {
        if (numberOfIndividuals <= 0)
            throw new IllegalArgumentException("The number of individuals to select must be greater than 0.");

        if (tournamentSize <= 0)
            throw new IllegalArgumentException("The number of individuals in a tournament must be greater than 0.");

        this.numberOfIndividuals = numberOfIndividuals;
        this.tournamentSize = tournamentSize;
    }

    /**
     * @return  an empty population if the size of the tournament is bigger than the population size,
     *          the selected population otherwise.
     */
    @Override
    public Population select(Population population, PRNG prng, ClusterLogger logger) {
        if (this.tournamentSize > population.getSize()) {
            logger.log(String.format(
                    "The number of candidates selected for tournament [%s] exceeds the population size [%s].",
                    tournamentSize, population.getSize()));
            return new Population(new ArrayList<>());
        }

        Population matingPool = new Population(new ArrayList<>(numberOfIndividuals));

        for (int i = 0; i < this.numberOfIndividuals; i++) {
            List<Individual> tournamentCandidates = new ArrayList<>(tournamentSize);
            Set<Integer> extracted = new HashSet<>(tournamentSize);
            int j = 0;

            while (j < tournamentSize) {
                int index = prng.nextInt(population.getSize());

                if (extracted.contains(index))
                    continue;

                extracted.add(index);
                tournamentCandidates.add(population.getIndividual(index));
                j++;
            }

            Individual fittest = tournamentCandidates.get(0);
            for (Individual individual : tournamentCandidates) {
                if (individual.getFitness() > fittest.getFitness())
                    fittest = individual;
            }

            matingPool.addIndividual(fittest);
        }

        return matingPool;
    }
}
