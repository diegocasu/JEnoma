package it.unipi.jenoma.example.knapsack_problem;

import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.operator.Evaluation;
import it.unipi.jenoma.population.Individual;

import java.io.Serializable;

public class KnapsackEvaluation implements Evaluation, Serializable {

    KnapSackItem[] itemList;
    double maxWeight;

    public KnapsackEvaluation(KnapSackItem[] itemList, double mw) {
        this.itemList = itemList;
        this.maxWeight = mw;
    }

    @Override
    public double evaluate(Individual individual, ClusterLogger logger) {
        KnapSackChromosome knapsackChromosome = (KnapSackChromosome) individual.getChromosome();
        double accumulator = 0;
        double currentWeight =0;
        for(int i = 0; i < this.itemList.length; i++){
            accumulator   += (knapsackChromosome.getGene(i) == 1)? itemList[i].getProfit() : 0;
            currentWeight += (knapsackChromosome.getGene(i) == 1)? itemList[i].getWeight() : 0;
            if(currentWeight > this.maxWeight) {
                individual.setFitness(0);
                return 0;
            }
        }

        individual.setFitness(accumulator);
        return accumulator;
    }
}
