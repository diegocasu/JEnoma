package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Individual;

import java.io.Serializable;


public interface Evaluation extends Serializable {

    double evaluate(Individual individual);
}
