package it.unipi.jenoma.example.knapsack;

import java.io.Serializable;


public class KnapsackItem implements Serializable, Comparable<KnapsackItem> {
    private double weight;
    private double profit;


    public KnapsackItem(double weight, double profit) {
        this.weight = weight;
        this.profit = profit;
    }

    public double getWeight() {
        return weight;
    }

    public void setWeight(double weight) {
        this.weight = weight;
    }

    public double getProfit() {
        return profit;
    }

    public void setProfit(double profit) {
        this.profit = profit;
    }

    public double getRatio(){
        return this.profit/this.weight;
    }

    @Override
    public int compareTo(KnapsackItem o) {
        return Double.compare(this.getRatio(), o.getRatio());
    }
}
