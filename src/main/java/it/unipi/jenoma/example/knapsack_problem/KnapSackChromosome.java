package it.unipi.jenoma.example.knapsack_problem;

import it.unipi.jenoma.population.Chromosome;

import java.io.Serializable;
import java.util.List;

public class KnapSackChromosome extends Chromosome<Integer> implements Serializable {
    private KnapSackItem[] items;
    public KnapSackChromosome(List<Integer> genes) {
        super(genes);
    }

    @Override
    public String toString() {
        StringBuilder output = new StringBuilder( super.toString());
        if(this.items!= null) {
            output.setCharAt(output.length() - 1, ',');
            output.append(" Total weight= " + getTotalWeight() + "}");
        }
        return output.toString();
    }

    double getTotalWeight(){
        double currentWeight = 0;
        for (int i = 0; i < this.items.length; i++) {
            currentWeight += (this.getGene(i) > 0) ? (items[i].getWeight() * this.getGene(i)) : 0;
        }
        return currentWeight;
    }

    public void setItems(KnapSackItem[] items) {
        this.items = items;
    }
}
