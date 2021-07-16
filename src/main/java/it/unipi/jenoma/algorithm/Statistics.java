package it.unipi.jenoma.algorithm;

import it.unipi.jenoma.population.Individual;

import java.io.Serializable;

public class Statistics implements Serializable {
    public int currentGeneration;
    public Individual fittestIndividual;
    public String workerID;
    public long computationTime;
    public long communicationTime;

    @Override
    public String toString() {
        return "Statistics{\n" +
                "currentGeneration=" + currentGeneration +
                ",\n\tfittestIndividual=" + fittestIndividual +
                ",\n\tworkerID='" + workerID + '\'' +
                ",\n\tcomputationTime=" + computationTime +
                ",\n\tcommunicationTime=" + communicationTime +
                "}\n";
    }
}


