package it.unipi.jenoma.algorithm;

import com.opencsv.CSVWriter;
import it.unipi.jenoma.population.Individual;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

public class ClusterStatistics implements Serializable {
    public int currentGeneration;
    public Individual fittestIndividual;
    public String workerID;
    public long computationTime;
    public long communicationTime;

    public long crossoverTime;
    public long elitismTime;
    public long mutationTime;
    public long selectionTime;
    public long evaluationTime;

    public static void saveCoordinatorStatistics(List<ClusterStatistics[]> clusterStatisticsList) throws IOException {
        File file = new File("./statistics.csv");
        FileWriter outputfile = new FileWriter(file);
        CSVWriter writer = new CSVWriter(outputfile);

        // adding header to csv
        String[] header = { "Id","ComputationTime", "CommunicationTime", "fitnesses", "crossoverTime", "elitismTime",
                            "mutationTime", "selectionTime", "evaluationTime"};
        writer.writeNext(header);

        for (ClusterStatistics[] arr : clusterStatisticsList) {

            if(arr.length == 0){
                writer.close();
                return;
            }
            double[] workingtTimeArr        = new double[arr.length];
            double[] communicationTimeArr   = new double[arr.length];
            double[] fitnessArr             = new double[arr.length];
            double[] crossoverArr           = new double[arr.length];
            double[] elitismArr             = new double[arr.length];
            double[] mutationArr            = new double[arr.length];
            double[] selectionArr           = new double[arr.length];
            double[] evaluationArr          = new double[arr.length];

            String id = arr[0].workerID;
            for(int j = 0; j < arr.length; j++){
                workingtTimeArr[j] = arr[j].computationTime;
                communicationTimeArr[j] = arr[j].communicationTime;
                fitnessArr[j]           = (arr[j].fittestIndividual ==null )? 0: arr[j].fittestIndividual.getFitness();
                crossoverArr[j]         = arr[j].crossoverTime;
                elitismArr[j]           = arr[j].elitismTime;
                mutationArr[j]          = arr[j].mutationTime;
                selectionArr[j]         = arr[j].selectionTime;
                evaluationArr[j]        = arr[j].evaluationTime;
            }

            String[] newLine = {id,
                    Arrays.toString(workingtTimeArr),
                    Arrays.toString(communicationTimeArr),
                    Arrays.toString(fitnessArr),
                    Arrays.toString(crossoverArr),
                    Arrays.toString(elitismArr),
                    Arrays.toString(mutationArr),
                    Arrays.toString(selectionArr),
                    Arrays.toString(evaluationArr)
            };

            writer.writeNext(newLine);

        }
        writer.close();

    }

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


