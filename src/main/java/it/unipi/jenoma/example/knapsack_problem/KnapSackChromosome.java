package it.unipi.jenoma.example.knapsack_problem;

import it.unipi.jenoma.population.Chromosome;

import java.io.Serializable;
import java.util.List;

public class KnapSackChromosome extends Chromosome<Integer> implements Serializable {

    public KnapSackChromosome(List<Integer> genes) {
        super(genes);
    }

}
