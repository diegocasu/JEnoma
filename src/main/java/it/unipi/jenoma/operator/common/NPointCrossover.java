//package it.unipi.jenoma.operator.common;
//
//import it.unipi.jenoma.algorithm.AlgorithmException;
//import it.unipi.jenoma.operator.Crossover;
//import it.unipi.jenoma.population.Individual;
//import it.unipi.jenoma.utils.PRNG;
//
//import java.util.*;
//
//public class NPointCrossover<T extends Individual<?>> implements Crossover<T> {
//
//    private int n;
//
//    public NPointCrossover(int n){
//        this.n = n;
//    }
///*
//     ________________________________________
//    | O | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |  parent1
//    |________________________________________
//
//     ________________________________________
//    | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |  parent2
//    |________________________________________
//
//    ________________________________________
//    | O | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 1 |  child1
//    |________________________________________
//                |           |       |
//     ___________|___________|_______|________
//    | 1 | 1 | 1 | 0 | 0 | 0 | 1 | 1 | 0 | 0 |  child2
//    |________________________________________
// */
//
//    @Override
//    public List<T> crossover(T parent1, T parent2, PRNG prng) {
//        List<T> offspring = new ArrayList<T>();
//        offspring.add(parent1);
//        offspring.add(parent2);
//
//
//        if (parent1.getChromosome().getLength() != parent2.getChromosome().getLength()) {
//            throw new AlgorithmException("NPOINTCROSSOVER: requested " + this.n +
//                    "-Point Crossover n differently sized parents");
//        }
//
//        if (this.n > parent1.getChromosome().getLength()) {
//            throw new AlgorithmException("NPOINTCROSSOVER: requested " + this.n + "-Point Crossover on an " +
//                    parent1.getChromosome().getLength() + " genes chromosome");
//        }
//        int crossPoint = 0;
//        int old = 0;
//        int maxPartitionSize = parent1.getChromosome().getLength() - (this.n-1);
//        boolean isOddTurn = true;
//        for(int partitionsLeft = this.n-1 ; partitionsLeft > 0; partitionsLeft--){
//            crossPoint += (maxPartitionSize ==1 )? 1 : prng.nextInt(maxPartitionSize);
//
//            if(isOddTurn){
//                offspring.get(0).getChromosome().cross(parent1.getChromosome(), old, crossPoint);
//                offspring.get(1).getChromosome().cross(parent2.getChromosome(), old, crossPoint);
//            }else {
//                offspring.get(0).getChromosome().cross(parent2.getChromosome(), old, crossPoint);
//                offspring.get(1).getChromosome().cross(parent1.getChromosome(), old, crossPoint);
//            }
//
//            int availableSpace = (parent1.getChromosome().getLength() - crossPoint);
//            maxPartitionSize = availableSpace - partitionsLeft;
//            old = crossPoint;
//            isOddTurn = !isOddTurn;
//        }
//
//        int chromLength = parent1.getChromosome().getLength() - 1 ;
//
//        if(isOddTurn){
//            offspring.get(0).getChromosome().cross(parent1.getChromosome(), crossPoint, chromLength);
//            offspring.get(1).getChromosome().cross(parent2.getChromosome(), crossPoint, chromLength);
//        }else {
//            offspring.get(0).getChromosome().cross(parent2.getChromosome(), crossPoint, chromLength);
//            offspring.get(1).getChromosome().cross(parent1.getChromosome(), crossPoint, chromLength);
//        }
//
//        return offspring;
//    }
//}
