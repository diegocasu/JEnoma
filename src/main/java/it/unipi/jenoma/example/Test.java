package it.unipi.jenoma.example;

import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.cluster.Coordinator;
import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.operator.Elitism;
import it.unipi.jenoma.operator.Evaluation;
import it.unipi.jenoma.operator.Mutation;
import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.operator.common.NGenerationsElapsed;
import it.unipi.jenoma.operator.common.RouletteWheelSelection;
import it.unipi.jenoma.operator.common.UniformCrossover;
import it.unipi.jenoma.population.Chromosome;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.Configuration;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


public class Test {
    public static void main(String[] args) throws IOException {
        Configuration conf = new Configuration("configuration.json");
        Population population = new Population(new ArrayList<>());

        for (int i = 0; i < 50; i++) {
            List<Integer> genes = new ArrayList<>();

            for (int j = 0; j < 20; j++)
                genes.add(i);

            Chromosome<Integer> chromosome = new Chromosome<>(genes);
            Individual individual = new Individual(chromosome);
            population.addIndividual(individual);
        }

        Evaluation evaluation = (individual, logger) -> {
            int acc = 0;
            for (Integer i : (Chromosome<Integer>) individual.getChromosome())
                acc += i;
            return acc;
        };

        //Crossover crossover = new NPointCrossover(4);
        Crossover crossover = new UniformCrossover(0.5);

        Selection selection = new RouletteWheelSelection(10);

        Mutation mutation = (individual, prng, logger) -> {
            int n = prng.nextInt(10);
            Chromosome<Integer> c = (Chromosome<Integer>)individual.getChromosome();
            c.setGene(0, n);
        };

        Elitism elitism = new Elitism(10);

        TerminationCondition<Boolean> terminationCondition = new NGenerationsElapsed(5);

        Coordinator c = new Coordinator(new GeneticAlgorithm(
                conf,
                population,
                evaluation,
                selection,
                crossover,
                mutation,
                terminationCondition,
                elitism));
        Population finalPopulation = c.start();

        System.out.println(finalPopulation);
    }
}
