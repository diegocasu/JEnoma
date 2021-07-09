package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.algorithm.AlgorithmException;
import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


public class TournamentSelection implements Selection {
    private final int chromosomeChosenForReproduction;
    private final int candidateSelectedForTournament;


    public TournamentSelection(int chromosomeChosenForReproduction, int candidateSelectedForTournament) {
        this.chromosomeChosenForReproduction = chromosomeChosenForReproduction;
        this.candidateSelectedForTournament = candidateSelectedForTournament;
    }

    @Override
    public Population select(Population population, PRNG prng) {
        if (this.candidateSelectedForTournament > population.getLength()) {
            throw new AlgorithmException("TOURNAMENTSELECTION: Number of candidates selected for tournament exceeding " +
                                         "population length");
        }

        Population matingPool = new Population(new ArrayList<>(chromosomeChosenForReproduction));

        for (int i = 0; i < this.chromosomeChosenForReproduction; i++) {
            List<Individual> tournamentCandidates = new ArrayList<>(candidateSelectedForTournament);
            Set<Integer> extracted = new HashSet<>(candidateSelectedForTournament);
            int j = 0;

            while (j < candidateSelectedForTournament) {
                int index = prng.nextInt(population.getLength());

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
