package it.unipi.jenoma.algorithm;

import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.operator.Elitism;
import it.unipi.jenoma.operator.Evaluation;
import it.unipi.jenoma.operator.Mutation;
import it.unipi.jenoma.operator.Selection;
import it.unipi.jenoma.operator.TerminationCondition;


public class GeneticAlgorithm {
    private Evaluation<?> evaluation;
    private Selection<?> selection;
    private Crossover<?> crossover;
    private Mutation<?> mutation;
    private TerminationCondition<?> terminationCondition;
    private Elitism elitism;


    public GeneticAlgorithm() {}

    public GeneticAlgorithm setEvaluation(Evaluation<?> evaluationFunction) {
        this.evaluation = evaluationFunction;
        return this;
    }

    public GeneticAlgorithm setSelection(Selection<?> selectionFunction) {
        this.selection = selectionFunction;
        return this;
    }

    public GeneticAlgorithm setCrossover(Crossover<?> crossoverFunction) {
        this.crossover = crossoverFunction;
        return this;
    }

    public GeneticAlgorithm setMutation(Mutation<?> mutationFunction) {
        this.mutation = mutationFunction;
        return this;
    }

    public GeneticAlgorithm setTerminationCondition(TerminationCondition<?> terminationCondition) {
        this.terminationCondition = terminationCondition;
        return this;
    }

    public void setElitismStrategy(Elitism elitism) {
        this.elitism = elitism;
    }

}