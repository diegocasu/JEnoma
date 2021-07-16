package it.unipi.jenoma.example.knapsack_problem;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.Mutation;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;

public class KnapSackMutation implements Mutation, Serializable {
    double mutationProbability;

    public KnapSackMutation(double mutation) {
        this.mutationProbability = mutation;
    }

    @Override
    public void mutate(Individual individual, PRNG prng, ClusterLogger logger) {
        int mutatingGene = prng.nextInt(individual.getChromosome().getLength());
        Integer mutationValue = (int)individual.getChromosome().getGene(mutatingGene)+2;
        if(mutationOccurs(prng)) {
            ((KnapSackChromosome)individual.getChromosome()).setGene(mutatingGene, mutationValue);
        }
    }

    Boolean mutationOccurs(PRNG prng){
        return (prng.nextDouble() <= this.mutationProbability);
    }


}
