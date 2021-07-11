package it.unipi.jenoma.algorithm;

import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.operator.Elitism;
import it.unipi.jenoma.operator.Evaluation;
import it.unipi.jenoma.operator.Mutation;
import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.Configuration;

import java.io.Serializable;


public class GeneticAlgorithm implements Serializable {
    private final Configuration configuration;
    private final Evaluation evaluation;
    private final Selection selection;
    private final Crossover crossover;
    private final Mutation mutation;
    private final TerminationCondition<?> terminationCondition;
    private final Elitism elitism;
    private int generationsElapsed;
    private Population population;


    private void checkNullFields() {
        String msg = null;

        if (configuration == null) msg = "Configuration is null";
        else if (population == null) msg = "Population is null";
        else if (evaluation == null) msg = "Evaluation is null";
        else if (selection == null) msg = "Selection is null";
        else if (crossover == null) msg = "Crossover is null";
        else if (mutation == null) msg = "Mutation is null";
        else if (terminationCondition == null) msg = "TerminationCondition is null";
        else if (elitism == null) msg = "Elitism is null";

        if (msg != null)
            throw new IllegalArgumentException(msg);
    }

    public GeneticAlgorithm(Configuration configuration,
                            Population population,
                            Evaluation evaluation,
                            Selection selection,
                            Crossover crossover,
                            Mutation mutation,
                            TerminationCondition<?> terminationCondition,
                            Elitism elitism) {
        this.configuration = configuration;
        this.population = population;
        this.evaluation = evaluation;
        this.selection = selection;
        this.crossover = crossover;
        this.mutation = mutation;
        this.terminationCondition = terminationCondition;
        this.elitism = elitism;
        this.generationsElapsed = 0;
        checkNullFields();
    }

    public GeneticAlgorithm(GeneticAlgorithm algorithm, Population population) {
        this.configuration = algorithm.configuration;
        this.evaluation = algorithm.evaluation;
        this.selection = algorithm.selection;
        this.crossover = algorithm.crossover;
        this.mutation = algorithm.mutation;
        this.terminationCondition = algorithm.terminationCondition;
        this.elitism = algorithm.elitism;
        this.generationsElapsed = algorithm.generationsElapsed;
        this.population = population;
        checkNullFields();
    }

    public Configuration getConfiguration() {
        return configuration;
    }

    public Population getPopulation() {
        return population;
    }

    public Evaluation getEvaluation() {
        return evaluation;
    }

    public Selection getSelection() {
        return selection;
    }

    public Crossover getCrossover() {
        return crossover;
    }

    public Mutation getMutation() {
        return mutation;
    }

    public TerminationCondition<?> getTerminationCondition() {
        return terminationCondition;
    }

    public Elitism getElitism() {
        return elitism;
    }

    public int getGenerationsElapsed() {
        return generationsElapsed;
    }

    public void setPopulation(Population population) {
        this.population = population;
    }

    public void incrementGenerations() {
        this.generationsElapsed += 1;
    }
}