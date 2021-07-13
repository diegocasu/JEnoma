package it.unipi.jenoma.example;

import com.google.gson.JsonParseException;
import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.cluster.ClusterLogger;
import it.unipi.jenoma.cluster.Coordinator;
import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.operator.Elitism;
import it.unipi.jenoma.operator.Evaluation;
import it.unipi.jenoma.operator.Mutation;
import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.operator.common.NGenerationsElapsed;
import it.unipi.jenoma.operator.common.RouletteWheelSelection;
import it.unipi.jenoma.population.Chromosome;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.Configuration;
import it.unipi.jenoma.utils.PRNG;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


/**
 * Class representing a city in the travelling salesman problem.
 */
class City implements Serializable {
    public final int id;
    public final int x;
    public final int y;


    public City(int id, int x, int y) {
        this.id = id;
        this.x = x;
        this.y = y;
    }

    public double distance(City that) {
        return Math.sqrt((this.x - that.x)*(this.x - that.x) + (this.y - that.y)*(this.y - that.y));
    }

    @Override
    public String toString() {
        return String.valueOf(id);
    }
}


/**
 * A randomly initialized travelling salesman problem.<br>
 * The problem works with:<br>
 * 1) 25 cities, whose positions (x,y) are randomly chosen. Each coordinate can be extracted
 *    in the interval [0, 500];<br>
 * 2) a population size of 5000;<br>
 * 3) a mutation probability of 0.1;<br>
 * 4) a maximum number of generations equal to 100.<br>
 * The path of the configuration file must be specified as argument when
 * invoking the <code>main()</code> method from the command line.
 */
public class TSP implements Serializable {
    private final int CITIES = 25;
    private final int MAX_X_COORDINATE = 500;
    private final int MAX_Y_COORDINATE = 500;
    private final int POPULATION_SIZE = 5000;
    private final double MUTATION_PROBABILITY = 0.1;
    private final int GENERATIONS_LIMIT = 100;
    private final Configuration configuration;
    private final PRNG prng;


    /**
     * Generates a list of cities used in the TSP, whose coordinates are randomly chosen.
     * @return  the list of cities used in the TSP.
     */
    private List<City> generateCities() {
        List<City> cities = new ArrayList<>();

        for (int i = 0; i < CITIES; i++)
            cities.add(new City(i, prng.nextInt(MAX_X_COORDINATE + 1), prng.nextInt(MAX_Y_COORDINATE + 1)));

        return cities;
    }

    /**
     * Generates a random individual for the TSP. The chromosome of an individual represents
     * a possible travelling path, starting and ending in the same city, inside which each
     * other city appears only one time.
     * @param startingCity  the starting (and ending) city of the TSP.
     * @param otherCities   the list of cities that appear the path, starting city excluded.
     * @return              a randomly initialized individual.
     */
    private Individual generateIndividual(City startingCity, List<City> otherCities) {
        List<City> genes = new ArrayList<>();
        genes.add(startingCity);

        for (int i = 0; i <= otherCities.size() - 2; i++) {
            int j = i + prng.nextInt(otherCities.size() - i);
            Collections.swap(otherCities, i, j);
            genes.add(otherCities.get(i));
        }

        genes.add(otherCities.get(otherCities.size() - 1));
        genes.add(startingCity);
        return new Individual(new Chromosome<>(genes));
    }

    /**
     * Computes the cost of the travelling path represented by the given chromosome.
     * @param path  the chromosome representing the travelling path.
     * @return      the cost of the path.
     */
    private double computePathCost(Chromosome<City> path) {
        double pathCost = 0;

        for (int i = 0; i <= path.getLength() - 2; i++) {
            City start = path.getGene(i);
            City end = path.getGene(i + 1);
            pathCost += start.distance(end);
        }

        return pathCost;
    }

    /**
     * Returns the evaluation operator used in the TSP. The fitness of an individual
     * is defined as <code>1/pathCost</code>, where <code>pathCost</code> is the cost
     * of the travelling path represented by the chromosome of the individual.
     * @return  the evaluation operator used in the TSP.
     */
    private Evaluation getEvaluationOperator() {
        return (individual, logger) -> 1/computePathCost(((Chromosome<City>) individual.getChromosome()));
    }

    /**
     * Returns the crossover operator used in the TSP. The crossover is implemented
     * exploiting the ordered crossover strategy, so that the offspring is guaranteed to
     * contain valid TSP solutions (a city different from the starting one will always
     * appear one time).
     * @return  the crossover operator used in the TSP.
     */
    private Crossover getOrderedCrossoverOperator() {
        return new Crossover() {
            private boolean inSubset(City gene, List<City> subset) {
                for (City city : subset) {
                    if (city.id == gene.id)
                        return true;
                }
                return false;
            }

            private List<City> orderedCrossover(Individual parent1, Individual parent2) {
                // Select a subset of consecutive values of the first parent, excluding the starting/ending cities.
                int startingIndex = 1 + prng.nextInt(parent1.getChromosome().getLength() - 2);
                int endIndex = startingIndex + prng.nextInt(parent1.getChromosome().getLength() - 1 - startingIndex);
                List<City> subset = new ArrayList<>();

                for (int i = startingIndex; i <= endIndex; i++)
                    subset.add(((Chromosome<City>)parent1.getChromosome()).getGene(i));

                /*
                 * Fill the chromosome of the child so that:
                 * 1) the positions in the interval [startingIndex, endIndex] are assigned to
                 *    the genes of the subset extracted from the first parent;
                 * 2) the remaining positions are assigned to the genes of the second parent that
                 *    are different from the ones in the previously mentioned subset. The relative
                 *    order of genes in parent2 is preserved.
                 * An example, where "..." denotes the selected subset in parent1, _ the genes
                 * to not assign from parent2:
                 *
                 * Parent1 -> 8 4 9 3 6 2 5 1 7 0
                 * Parent2 -> 0 1 2 3 4 5 6 7 8 9
                 *
                 * Parent1 -> 8 4 9 "3 6 2 5 1" 7 0
                 * Parent2 -> 0 _ _  _ 4 _ _ 7  8 9
                 * Child   -> 0 4 7  3 6 2 5 1  8 9
                 */
                List<City> childChromosome = new ArrayList<>();
                for (int i = 0; i < parent2.getChromosome().getLength(); i++) {
                    City gene = ((Chromosome<City>) parent2.getChromosome()).getGene(i);
                    if (!inSubset(gene, subset))
                        childChromosome.add(gene);
                }

                childChromosome.addAll(startingIndex, subset);
                return childChromosome;
            }

            @Override
            public List<Individual> cross(Individual parent1, Individual parent2, PRNG prng, ClusterLogger logger) {
                Individual child1 = new Individual(new Chromosome<>(orderedCrossover(parent1, parent2)));
                Individual child2 = new Individual(new Chromosome<>(orderedCrossover(parent2, parent1)));

                List<Individual> offspring = new ArrayList<>();
                offspring.add(child1);
                offspring.add(child2);
                return offspring;
            }
        };
    }

    /**
     * Returns the mutation operator used in the TSP. For each gene of the chromosome,
     * the mutation operator decides, with a certain probability, if the gene must be swapped
     * with another one or not. The mutation preserves the validity of the individuals (the starting
     * and ending cities are not involved).
     * @return  the mutation operator used in the TSP.
     */
    private Mutation getMutationOperator() {
        return (individual, prng, logger) -> {
            Chromosome<City> chromosome = (Chromosome<City>) individual.getChromosome();

            for (int i = 1; i <= individual.getChromosome().getLength() - 2; i++) {
                double outcome = prng.nextDouble();

                if (outcome <= MUTATION_PROBABILITY) { // Swap.
                    int j = 1 + prng.nextInt(individual.getChromosome().getLength() - 2);
                    City gene_i = chromosome.getGene(i);
                    City gene_j = chromosome.getGene(j);
                    chromosome.setGene(i, gene_j);
                    chromosome.setGene(j, gene_i);
                }
            }
        };
    }

    public TSP(Configuration configuration) {
        this.configuration = configuration;
        this.prng = new PRNG(configuration.getSeed());
    }

    /**
     * Starts the resolution of the TSP problem on the cluster.
     * @return  a pair containing the final population and the path cost achieved by best individual.
     */
    public Pair<Population, Double> solve() {
        List<City> cities = generateCities();
        City startingCity = cities.remove(prng.nextInt(cities.size()));

        System.out.printf("Number of cities: %s.%n", CITIES);
        System.out.printf("x coordinates randomly chosen in the interval: [0, %s].%n", MAX_X_COORDINATE);
        System.out.printf("y coordinates randomly chosen in the interval: [0, %s].%n", MAX_Y_COORDINATE);
        System.out.printf("Population size: %s.%n", POPULATION_SIZE);
        System.out.printf("Mutation probability: %s.%n", MUTATION_PROBABILITY);
        System.out.printf("Generations limit: %s.%n", GENERATIONS_LIMIT);
        System.out.printf("Starting city: %s.%n", startingCity);

        // Create a population.
        Population initialPopulation = new Population(new ArrayList<>());
        for (int i = 0; i < POPULATION_SIZE; i++)
            initialPopulation.addIndividual(generateIndividual(startingCity, cities));

        // Create operators.
        Evaluation evaluation = getEvaluationOperator();
        Selection selection = new RouletteWheelSelection(POPULATION_SIZE/3);
        Crossover crossover = getOrderedCrossoverOperator();
        Mutation mutation = getMutationOperator();
        Elitism elitism = new Elitism(POPULATION_SIZE/5);
        TerminationCondition terminationCondition = new NGenerationsElapsed(GENERATIONS_LIMIT);

        // Create algorithm and start.
        GeneticAlgorithm algorithm = new GeneticAlgorithm(
                configuration,
                initialPopulation,
                evaluation,
                selection,
                crossover,
                mutation,
                terminationCondition,
                elitism);

        Coordinator coordinator = new Coordinator(algorithm);
        Population finalPopulation = coordinator.start();

        if (finalPopulation.getSize() == 0)
            return new ImmutablePair<>(finalPopulation, 0.0);

        finalPopulation.sortByDescendingFitness();
        Individual bestIndividual = finalPopulation.getIndividual(0);
        return new ImmutablePair<>(finalPopulation, computePathCost((Chromosome<City>) bestIndividual.getChromosome()));
    }

    /**
     * Starts the execution of the TSP.
     * The path of the configuration file must be specified as argument when
     * invoking the <code>main()</code> method from the command line.
     * @throws IOException         if an I/O error occurs while accessing the configuration file.
     * @throws JsonParseException  if the configuration file contains an invalid JSON syntax.
     */
    public static void main(String[] args) throws IOException {
        Configuration configuration = new Configuration(args[0]);
        TSP tspProblem = new TSP(configuration);
        Pair<Population, Double> result = tspProblem.solve();

        System.out.printf("Best individual: %s.%n", result.getLeft().getIndividual(0));
        System.out.printf("Path cost: %s.%n", result.getRight());
    }
}
