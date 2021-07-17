package it.unipi.jenoma.example.knapsack;

import it.unipi.jenoma.population.Chromosome;

import java.io.Serializable;
import java.util.List;


public class KnapsackChromosome extends Chromosome<Integer> implements Serializable {
    private KnapsackItem[] items;


    public KnapsackChromosome(List<Integer> genes) {
        super(genes);
    }

    @Override
    public String toString() {
        StringBuilder output = new StringBuilder(super.toString());

        if (this.items != null) {
            output.setCharAt(output.length() - 1, ',');
            output.append(" Total weight = ").append(getTotalWeight()).append("}");
        }

        return output.toString();
    }

    double getTotalWeight() {
        double currentWeight = 0;

        for (int i = 0; i < this.items.length; i++) {
            currentWeight += (this.getGene(i) > 0) ? (items[i].getWeight() * this.getGene(i)) : 0;
        }

        return currentWeight;
    }

    public void setItems(KnapsackItem[] items) {
        this.items = items;
    }
}
