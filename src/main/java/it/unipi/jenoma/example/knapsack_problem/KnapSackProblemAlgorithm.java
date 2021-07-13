package it.unipi.jenoma.example.knapsack_problem;

import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.cluster.Coordinator;
import it.unipi.jenoma.operator.*;
import it.unipi.jenoma.operator.common.NGenerationsElapsed;
import it.unipi.jenoma.operator.common.RouletteWheelSelection;
import it.unipi.jenoma.operator.common.UniformCrossover;
import it.unipi.jenoma.population.Chromosome;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.Configuration;
import it.unipi.jenoma.utils.PRNG;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class KnapSackProblemAlgorithm {
    public static void main(String[] args) throws IOException {
        Configuration conf = new Configuration("configuration.json");
        Population population = new Population(new ArrayList<>());

        PRNG prng = new PRNG(2075);

        KnapSackItem[] items = new KnapSackItem[]{
                new KnapSackItem(2,6),
                new KnapSackItem(3,5),
                new KnapSackItem(6,8),
                new KnapSackItem(7,9),
                new KnapSackItem(5,6),
                new KnapSackItem(9,7),
                new KnapSackItem(4,3)
        };

        double maxWeight = 9;

        for (int i = 0; i < 100; i++) {
            List<Integer> genes = new ArrayList<>();

            for (int j = 0; j < items.length; j++)
                genes.add(prng.nextInt(2));

            KnapSackChromosome knapsackChromosome = new KnapSackChromosome(genes);
            Individual individual = new Individual(knapsackChromosome);
            population.addIndividual(individual);
        }


        Crossover crossover = new UniformCrossover(0.5);

        Selection selection = new RouletteWheelSelection(20);

        KnapSackMutation knapSackMutation = new KnapSackMutation(0.01);

        Elitism elitism = new Elitism(20);

        TerminationCondition<Boolean> terminationCondition = new NGenerationsElapsed(12);

        KnapsackEvaluation knapsackEvaluation = new KnapsackEvaluation(items, maxWeight);


        Coordinator c = new Coordinator(
                new GeneticAlgorithm(
                        conf,
                        population,
                        knapsackEvaluation,
                        selection,
                        crossover,
                        knapSackMutation,
                        terminationCondition,
                        elitism
                ));

        Population finalPopulation = c.start();

        System.out.println(finalPopulation);
    }
}
