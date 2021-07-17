package it.unipi.jenoma.example.knapsack;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.Evaluation;
import it.unipi.jenoma.population.Individual;

import java.io.Serializable;


public class KnapsackEvaluation implements Evaluation, Serializable {
    private final KnapsackItem[] itemList;
    private final double maxWeight;


    public KnapsackEvaluation(KnapsackItem[] itemList, double maxWeight) {
        this.itemList = itemList;
        this.maxWeight = maxWeight;
    }

    @Override
    public double evaluate(Individual individual, ClusterLogger logger) {
        KnapsackChromosome knapsackChromosome = (KnapsackChromosome) individual.getChromosome();
        double accumulator = 0;
        double currentWeight = 0;

        for (int i = 0; i < this.itemList.length; i++) {
            accumulator   += (knapsackChromosome.getGene(i) > 0) ? (itemList[i].getProfit() * knapsackChromosome.getGene(i)) : 0;
            currentWeight += (knapsackChromosome.getGene(i) > 0) ? (itemList[i].getWeight() * knapsackChromosome.getGene(i)) : 0;

            if (currentWeight > this.maxWeight)
                return 0;
        }

        return accumulator;
    }
}
