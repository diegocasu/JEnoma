package it.unipi.jenoma.example.knapsack_problem;

import java.io.Serializable;

public class KnapSackItem  implements Serializable,Comparable {
    double weight;
    double profit;

    public KnapSackItem(double weight, double profit) {
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
    public int compareTo(Object o) {
        return Double.compare(this.getRatio(), ((KnapSackItem)o).getRatio());
    }
}
