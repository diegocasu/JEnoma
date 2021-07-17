package it.unipi.jenoma.example.knapsack;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.Mutation;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.utils.PRNG;

import java.io.Serializable;


public class KnapsackMutation implements Mutation, Serializable {
    private final double mutationProbability;


    public KnapsackMutation(double mutation) {
        this.mutationProbability = mutation;
    }

    @Override
    public void mutate(Individual individual, PRNG prng, ClusterLogger logger) {
        int mutatingGene = prng.nextInt(individual.getChromosome().getLength());
        Integer mutationValue = prng.nextInt(avgQuantity((KnapsackChromosome) individual.getChromosome()));

        if (mutationOccurs(prng)) {
            ((KnapsackChromosome) individual.getChromosome()).setGene(mutatingGene, mutationValue);
        }
    }

    private Boolean mutationOccurs(PRNG prng) {
        return (prng.nextDouble() <= this.mutationProbability);
    }

    private int avgQuantity(KnapsackChromosome ksc) {
        double avg = 0;

        for (int i =0; i < ksc.getLength(); i++) {
            avg += ksc.getGene(i);
        }

        avg /= ksc.getLength();
        return (avg <= 2)? 10 : (int) avg;
    }
}
