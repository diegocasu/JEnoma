package it.unipi.jenoma.operator.common;

import it.unipi.jenoma.algorithm.AlgorithmException;
import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;

import java.util.ArrayList;
import java.util.List;

public class TournamentSelection<T extends Population<?>> implements Selection<T>{

    private T population;
    private T matingPool;
    private int chromosomeChosenForReproduction;
    private PRNG prng;
    private int candidateSelectedForTournament;

    public TournamentSelection(int chromosomeChosenForReproduction, int candidateSelectedForTournament, PRNG prng) {
        this.chromosomeChosenForReproduction = chromosomeChosenForReproduction;
        this.candidateSelectedForTournament = candidateSelectedForTournament;
        this.prng = prng;
    }


    @Override
    public T select(T population, PRNG prng) {

        if(this.candidateSelectedForTournament < this.population.getLength()){
            throw new AlgorithmException("TOURNAMENTSELECTION: Number of candidates selected for tournament exceeding " +
                                         "population length");
        }

        if (this.matingPool != null)
            this.matingPool.removeAll();

        this.population = (T) population.clone();
        this.matingPool = (T) population.clone().removeAll();

        for (int i = 0; i < this.chromosomeChosenForReproduction; i++) this.matingPool.addIndividual(select_helper());

        return (T) this.matingPool;
    }

    public Individual<?> select_helper() {

        List<Individual> tournamentCandidates = new ArrayList<>(this.candidateSelectedForTournament);

        for(int i =0; i < this.candidateSelectedForTournament; i++)
            tournamentCandidates.add(this.population.getIndividual(prng.nextInt(this.population.getLength())));

        Individual fittest = tournamentCandidates.get(0);
        for(Individual i : tournamentCandidates){
            if(i.getFitness() > fittest.getFitness())
                fittest = i;
        }

        return fittest;
    }
}
