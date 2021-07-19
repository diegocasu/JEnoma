package it.unipi.jenoma.example.knapsack_problem;

import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.cluster.Coordinator;
import it.unipi.jenoma.operator.*;
import it.unipi.jenoma.operator.common.NGenerationsElapsed;
import it.unipi.jenoma.operator.common.TournamentSelection;
import it.unipi.jenoma.operator.common.UniformCrossover;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.Configuration;
import it.unipi.jenoma.utils.PRNG;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.google.gson.JsonParseException;

public class KnapSackProblemAlgorithm {
    private static final int MAX_ITEMS_PER_SLOT = 30;
    private static final int MAX_WEIGHT = 73581;
    private static final int POPULATION_SIZE = 30;
    private static final double CROSSOVER_PROBABILITY = 0.4;
    private static final double MUTATION_PROBABILITY = 0.1;
    private static final int SELECTION_SIZE = 15;
    private static final int TOURNAMENT_SIZE = 10;
    private static final int ELITISM_SIZE = 10;
    private static final int GENERATIONS_LIMIT = 30;


    /**
     * Starts the execution of the knapsack problem.
     * The path of the configuration file must be specified as argument when
     * invoking the <code>main()</code> method from the command line.
     * @throws IOException         if an I/O error occurs while accessing the configuration file.
     * @throws JsonParseException  if the configuration file contains an invalid JSON syntax.
     */
    public static void main(String[] args) throws IOException {
        Configuration conf = new Configuration(args[0]);
        Population population = new Population(new ArrayList<>());
        PRNG prng = new PRNG(conf.getSeed());

        System.out.printf("Max items per slot: %s.%n", MAX_ITEMS_PER_SLOT);
        System.out.printf("Max weight: %s.%n", MAX_WEIGHT);
        System.out.printf("Population size: %s.%n", POPULATION_SIZE);
        System.out.printf("Mutation probability: %s.%n", CROSSOVER_PROBABILITY);
        System.out.printf("Mutation probability: %s.%n", MUTATION_PROBABILITY);
        System.out.printf("Generations limit: %s.%n", GENERATIONS_LIMIT);

        KnapSackItem[] itemsList = new KnapSackItem[] {
                new KnapSackItem(4,6),
                new KnapSackItem(3,5),
                new KnapSackItem(6,8),
                new KnapSackItem(7,9),
                new KnapSackItem(5,6),
                new KnapSackItem(9,8),
                new KnapSackItem(4,3)
        };

        Arrays.sort(itemsList, Collections.reverseOrder()); // Sorting the array is not mandatory

        for (int i = 0; i < POPULATION_SIZE; i++) {
            List<Integer> genes = new ArrayList<>();

            for (int j = 0; j < itemsList.length; j++)
                genes.add(prng.nextInt(MAX_ITEMS_PER_SLOT/(j + 1)));

            KnapSackChromosome knapSackChromosome = new KnapSackChromosome(genes);
            Individual individual = new Individual(knapSackChromosome);
            population.addIndividual(individual);
        }

        KnapsackEvaluation knapsackEvaluation = new KnapsackEvaluation(itemsList, MAX_WEIGHT);
        Crossover crossover = new UniformCrossover(CROSSOVER_PROBABILITY);
        Selection selection = new TournamentSelection(SELECTION_SIZE, TOURNAMENT_SIZE);
        KnapSackMutation knapsackMutation = new KnapSackMutation(MUTATION_PROBABILITY);
        Elitism elitism = new Elitism(ELITISM_SIZE);
        TerminationCondition<Boolean> terminationCondition = new NGenerationsElapsed(GENERATIONS_LIMIT);

        Coordinator coordinator = new Coordinator(
                new GeneticAlgorithm(
                        conf,
                        population,
                        knapsackEvaluation,
                        selection,
                        crossover,
                        knapsackMutation,
                        terminationCondition,
                        elitism));

        Population finalPopulation = coordinator.start();
        finalPopulation.sortByDescendingFitness();

        Individual bestIndividual = finalPopulation.getIndividual(0);
        ((KnapSackChromosome) bestIndividual.getChromosome()).setItems(itemsList);

        System.out.printf("Best individual: %s.%n", bestIndividual);
    }
}
