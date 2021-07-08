package it.unipi.jenoma.operator;

import it.unipi.jenoma.population.Individual;

import java.io.Serializable;


public interface Evaluation<T extends Individual<?>> extends Serializable {

    double evaluate(T individual);
}
