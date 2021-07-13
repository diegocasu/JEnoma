package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.*;

import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.operator.Elitism;
import it.unipi.jenoma.operator.Evaluation;
import it.unipi.jenoma.operator.Mutation;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;


class Worker {
    private final String host;
    private final String loggerCoordinatorHost;
    private final ExecutorService executorService;
    private final int numberOfThreads;
    private WorkerLogger workerLogger;
    private OtpNode javaNode;
    private OtpMbox mailBox;


    public Worker(String host, String loggerCoordinatorHost) {
        this.host = host;
        this.loggerCoordinatorHost = loggerCoordinatorHost;
        this.numberOfThreads = Runtime.getRuntime().availableProcessors();
        this.executorService = Executors.newFixedThreadPool(numberOfThreads);
    }

    private boolean startJavaNode() {
        try {
            javaNode = new OtpNode(ClusterUtils.compose(ClusterUtils.Node.JAVA, host));
        } catch (IOException e) {
            //TODO: add error log
            e.printStackTrace();
            return false;
        }

        javaNode.setCookie(ClusterUtils.Cluster.COOKIE);
        mailBox = javaNode.createMbox(ClusterUtils.Process.WORKER);
        return true;
    }

    private boolean startWorkerLogger() {
        try {
            workerLogger = new WorkerLogger(
                    host,
                    loggerCoordinatorHost,
                    ClusterUtils.compose(ClusterUtils.Node.JAVA, host));
        } catch (IOException e) {
            return false;
        }

        return true;
    }

    private void stopJavaNode() {
        executorService.shutdown();
        mailBox.close();
        javaNode.close();
    }

    private void stopWorkerLogger() {
        workerLogger.stop();
    }

    private void sendHeartbeatToErlangNode() {
        mailBox.send(
                ClusterUtils.Process.WORKER,
                ClusterUtils.compose(ClusterUtils.Node.ERLANG, host),
                ClusterUtils.Atom.HEARTBEAT);
    }

    private Pair<PRNG, List<PRNG>> createPRNGs(int seed) {
        // Create one main PRNG for generating values outside threads and N to do it inside threads.
        int hostSeed = Objects.hash(ClusterUtils.compose(ClusterUtils.Node.JAVA, host), seed);

        PRNG mainGenerator = new PRNG(hostSeed);
        List<PRNG> threadGenerators = new ArrayList<>();

        for (int i = 1; i <= numberOfThreads; i++)
            threadGenerators.add(new PRNG(hostSeed + i));

        return new ImmutablePair<>(mainGenerator, threadGenerators);
    }

    private boolean evaluate(Population population, Evaluation evaluation) {
        int chunkSize = population.getLength()/numberOfThreads;
        int threadsToSpawn = chunkSize > 0 ? numberOfThreads : 1;
        List<Callable<Void>> evaluationTasks = new ArrayList<>();

        for (int i = 0; i < threadsToSpawn; i++) {
            int startChunkIndex = i*chunkSize;
            int endChunkIndex = (i == threadsToSpawn - 1) ? population.getLength() : (i + 1)*chunkSize;

            evaluationTasks.add(() -> {
                for (Individual individual : population.getIndividuals(startChunkIndex, endChunkIndex)) {
                    individual.setFitness(evaluation.evaluate(individual, workerLogger));
                }
                return null;
            });
        }

        try {
            List<Future<Void>> results = executorService.invokeAll(evaluationTasks);
            for (Future<Void> result : results)
                result.get();
        } catch (InterruptedException | ExecutionException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            return false;
        }

        // Safety check, since a reference to the population is passed to the user-defined evaluation operator.
        return population != null && population.getLength() != 0;
    }

    private Population cross(Population matingPool, int offspringSize, Crossover crossover, List<PRNG> threadGenerators) {
        /* Random individuals are recombined until the overall offspring is as large as the given size
         * (original population size). N threads are launched, each one executing a certain number of
         * cross operations between individuals. Supposing that each crossover generates two children,
         * the default number of overall crossover operations per execution round is chosen to be
         * offspringSize/2. However, since the user is free to return 1 or more than 2 individuals,
         * the size of the offspring is checked, so that
         * 1) additional execution rounds are scheduled if offspringSize is not reached;
         * 2) the offspring is shrank if its size exceeds offspringSize.
         */
        Population offspring = new Population(new ArrayList<>());
        List<Callable<List<Individual>>> crossoverTasks = new ArrayList<>();

        while (true) {
            for (int i = 0; i < numberOfThreads; i++) {
                final PRNG prng = threadGenerators.get(i);

                crossoverTasks.add(() -> {
                    List<Individual> partialOffspring = new ArrayList<>();
                    int maxAttempts = Math.max(1, offspringSize/(2*numberOfThreads) + (offspringSize%(2*numberOfThreads) == 0 ? 0 : 1));
                    int counter = 0;

                    while (counter < maxAttempts) {
                        int firstParent = prng.nextInt(matingPool.getLength());
                        int secondParent = prng.nextInt(matingPool.getLength());

                        if (firstParent != secondParent) {
                            List<Individual> children = crossover.cross(
                                    matingPool.getIndividual(firstParent),
                                    matingPool.getIndividual(secondParent),
                                    prng, workerLogger);
                            partialOffspring.addAll(children);
                            counter++;
                        }
                    }
                    return partialOffspring;
                });
            }

            try {
                for (Future<List<Individual>> result : executorService.invokeAll(crossoverTasks)) {
                    List<Individual> taskResult = result.get();
                    if (taskResult != null && taskResult.size() != 0)
                        offspring.addIndividuals(taskResult);
                }

                if (offspring.getLength() == offspringSize)
                    return offspring;

                if (offspring.getLength() > offspringSize) {
                    offspring.removeIndividuals(offspringSize, offspring.getLength());
                    return offspring;
                }
            } catch (InterruptedException | ExecutionException e) {
                workerLogger.log(ExceptionUtils.getStackTrace(e));
                return null;
            }
        }
    }

    private boolean mutate(Population offspring, Mutation mutation, List<PRNG> threadGenerators) {
        int chunkSize = offspring.getLength()/numberOfThreads;
        int threadsToSpawn = chunkSize > 0 ? numberOfThreads : 1;
        List<Callable<Void>> mutationTasks = new ArrayList<>();

        for (int i = 0; i < threadsToSpawn; i++) {
            int startChunkIndex = i*chunkSize;
            int endChunkIndex = (i == threadsToSpawn - 1) ? offspring.getLength() : (i + 1)*chunkSize;
            final PRNG prng = threadGenerators.get(i);

            mutationTasks.add(() -> {
                for (Individual individual : offspring.getIndividuals(startChunkIndex, endChunkIndex)) {
                    mutation.mutate(individual, prng, workerLogger);
                }
                return null;
            });
        }

        try {
            List<Future<Void>> results = executorService.invokeAll(mutationTasks);
            for (Future<Void> result : results)
                result.get();
        } catch (InterruptedException | ExecutionException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            return false;
        }

        return true;
    }

    private void sendIndividualsForElitism(List<Individual> elite, List<Individual> worst) {
        // Both "elite" and "worst have the same number of elements by construction.
        int numberOfCandidates = elite.size();

        OtpErlangObject[] eliteTuplesList = new OtpErlangObject[numberOfCandidates];
        OtpErlangObject[] worstTuplesList = new OtpErlangObject[numberOfCandidates];

        for (int i = 0; i < numberOfCandidates; i++) {
            eliteTuplesList[i] = new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangBinary(elite.get(i)),
                            new OtpErlangDouble(elite.get(i).getFitness()),
                    });

            worstTuplesList[i] = new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangDouble(worst.get(i).getFitness()),
                            new OtpErlangInt(i)
                    });
        }

        mailBox.send(
                ClusterUtils.Process.WORKER,
                ClusterUtils.compose(ClusterUtils.Node.ERLANG, host),
                new OtpErlangTuple(
                        new OtpErlangObject[] {
                                ClusterUtils.Atom.ELITISM_PHASE,
                                new OtpErlangList(eliteTuplesList),
                                new OtpErlangList(worstTuplesList)
                        }));
    }

    private void receiveIndividualsForElitism(Population offspring) {
        try {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("The elitism stage failed.");
                workerLogger.log("Stopping the execution.");
                stopWorkerLogger();
                stopJavaNode();
                System.exit(0);
            }

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.NOT_INVOLVED_ELITISM)) {
                workerLogger.log("Not involved in the elitism stage.");
                return;
            }

            if (msg instanceof OtpErlangList msgList) {
                workerLogger.log(String.format("Involved in the elitism stage. Received %s individuals.", msgList.arity()));
                for (OtpErlangObject element : msgList) {
                    OtpErlangTuple individualTuple = (OtpErlangTuple) element;
                    Individual newIndividual = (Individual) ((OtpErlangBinary) individualTuple.elementAt(0)).getObject();
                    int indexToReplace = ((OtpErlangLong) individualTuple.elementAt(1)).intValue();

                    offspring.setIndividual(indexToReplace, newIndividual);
                }
            }
        } catch (OtpErlangExit | OtpErlangDecodeException | OtpErlangRangeException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopWorkerLogger();
            stopJavaNode();
            System.exit(0);
        }
    }

    private void applyElitism(Population original, Population offspring, Elitism elitism) {
        int numberOfCandidates = Math.min(elitism.getNumberOfIndividuals(), original.getLength());
        original.sortByDescendingFitness();
        offspring.sortByAscendingFitness();

        List<Individual> elite = original.getIndividuals(0, numberOfCandidates);
        List<Individual> worst = offspring.getIndividuals(0, numberOfCandidates);

        sendIndividualsForElitism(elite, worst);
        receiveIndividualsForElitism(offspring);
    }

    private boolean endAlgorithm(Population offspring, int generationsElapsed, TerminationCondition<?> termination) {
        mailBox.send(
                ClusterUtils.Process.WORKER,
                ClusterUtils.compose(ClusterUtils.Node.ERLANG, host),
                new OtpErlangTuple(
                        new OtpErlangObject[] {
                                ClusterUtils.Atom.GENERATION_END_PHASE,
                                new OtpErlangBinary(termination.map(offspring, generationsElapsed, workerLogger))
                        }));

        try {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("The termination condition check failed.");
                workerLogger.log("Stopping the execution.");
                stopWorkerLogger();
                stopJavaNode();
                System.exit(0);
            }

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.ALGORITHM_CONTINUE))
                return false;

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.ALGORITHM_END)) {
                workerLogger.log("Sending the population chunk.");

                mailBox.send(
                        ClusterUtils.Process.WORKER,
                        ClusterUtils.compose(ClusterUtils.Node.ERLANG, host),
                        new OtpErlangTuple(
                                new OtpErlangObject[] {
                                        ClusterUtils.Atom.RESULT_COLLECTION_PHASE,
                                        new OtpErlangBinary(offspring)
                                }));
                return true;
            }
        } catch (OtpErlangExit | OtpErlangDecodeException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopWorkerLogger();
            stopJavaNode();
            System.exit(0);
        }

        return true;
    }

    private void executeAlgorithm(GeneticAlgorithm geneticAlgorithm) throws OtpErlangDecodeException, OtpErlangExit{
        Pair<PRNG, List<PRNG>> prngs = createPRNGs(geneticAlgorithm.getConfiguration().getSeed());
        PRNG mainGenerator = prngs.getLeft();
        List<PRNG> threadGenerators = prngs.getRight();

        while (true) { // Algorithm loop.
            Population workingPopulation = geneticAlgorithm.getPopulation().clone();

            Population matingPool = geneticAlgorithm.getSelection().select(workingPopulation, mainGenerator, workerLogger);
            if (matingPool == null || matingPool.getLength() < 2) {
                workerLogger.log("The selection stage failed. Less than two individuals selected.");
                return;
            }
            workerLogger.log("The selection stage succeeded.");

            Population offspring = cross(matingPool, geneticAlgorithm.getPopulation().getLength(), geneticAlgorithm.getCrossover(), threadGenerators);
            if (offspring == null) {
                workerLogger.log("The crossover stage failed.");
                return;
            }
            workerLogger.log("The crossover stage succeeded.");

            if (!mutate(offspring, geneticAlgorithm.getMutation(), threadGenerators)) {
                workerLogger.log("The mutation stage failed.");
                return;
            }
            workerLogger.log("The mutation stage succeeded.");

            if (!evaluate(offspring, geneticAlgorithm.getEvaluation())) {
                workerLogger.log("The evaluation stage failed.");
                return;
            }
            workerLogger.log("The evaluation stage succeeded.");

            if (geneticAlgorithm.getElitism().getNumberOfIndividuals() > 0) {
                applyElitism(geneticAlgorithm.getPopulation(), offspring, geneticAlgorithm.getElitism());
                workerLogger.log("The elitism stage succeeded.");
            } else
                workerLogger.log("Elitism stage skipped.");

            geneticAlgorithm.incrementGenerationsElapsed();
            workerLogger.log(String.format("Reached generation %s.", geneticAlgorithm.getGenerationsElapsed()));

            if (endAlgorithm(offspring, geneticAlgorithm.getGenerationsElapsed(), geneticAlgorithm.getTerminationCondition()))
                return;

            // TODO: shuffling -> returns shuffledPopulation
            workerLogger.log("Starting the shuffling phase");

            sendShuffleMessageToErlangNode(offspring);

            // TODO: after shuffling --> geneticAlgorithm.setPopulation(shuffledPopulation)
            Population shuffledPop = receivePopulationForShuffling();
            if(shuffledPop == null){
                workerLogger.log("Shuffled population is null");
            }
            geneticAlgorithm.setPopulation(shuffledPop);
        }
    }

    private Population receivePopulationForShuffling() {
        OtpErlangObject msg = null;
        try {
            msg = mailBox.receive();
        } catch (OtpErlangExit | OtpErlangDecodeException otpErlangExit) {
           workerLogger.log(Arrays.toString(otpErlangExit.getStackTrace()));
        }

        if (msg instanceof OtpErlangTuple otpErlangTuple &&
                otpErlangTuple.elementAt(0).equals(ClusterUtils.Atom.SHUFFLE_COMPLETE)) {

            OtpErlangList individualsErlangList = new OtpErlangList(otpErlangTuple.elementAt(1));
            ArrayList<Individual> individulasForShuffling = new ArrayList<>(individualsErlangList.arity());
            try {
                for(OtpErlangObject object: individualsErlangList){
                    individulasForShuffling.add((Individual)((OtpErlangBinary)object).getObject());
                }
            }catch (Exception e){
                workerLogger.log(Arrays.toString(e.getStackTrace()));
                workerLogger.log(e.getMessage());
            }

            workerLogger.log("Retrieved population, ready for shuffling");
            return new Population(individulasForShuffling);
        }

        return null;

    }

    private void receiveLoop() throws OtpErlangDecodeException, OtpErlangExit {
        boolean waitForStop = false;

        while (true) {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("Stopping the execution.");
                stopWorkerLogger();
                stopJavaNode();
                return;
            }

            if (msg instanceof OtpErlangBinary msgBinary && !waitForStop) {
                GeneticAlgorithm geneticAlgorithm = (GeneticAlgorithm) msgBinary.getObject();
                workerLogger.log("Received workload.");

                // Start algorithm
                workerLogger.log(String.format("Starting the execution of the algorithm, exploiting %s threads.", numberOfThreads));
                workerLogger.log("Starting the evaluation of the initial population.");

                if (!evaluate(geneticAlgorithm.getPopulation(), geneticAlgorithm.getEvaluation())) {
                    workerLogger.log("The evaluation stage failed.");
                    workerLogger.log("Waiting for a stop command.");
                    waitForStop = true;
                    continue;
                }

                executeAlgorithm(geneticAlgorithm);
                waitForStop = true;
                workerLogger.log("Waiting for a stop command.");
            }
        }
    }

    public void start() {
        if (!startWorkerLogger())
            return;

        if (!startJavaNode()) {
            workerLogger.log("Java node failed to start.");
            stopWorkerLogger();
            return;
        }

        workerLogger.log("Java node started correctly.");
        sendHeartbeatToErlangNode();

        try {
            receiveLoop();
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopWorkerLogger();
            stopJavaNode();
        }
    }

    public static void main(String[] args) {
        String thisHost = args[0];
        String loggerCoordinatorHost = args[1];

        Worker worker = new Worker(thisHost, loggerCoordinatorHost);
        worker.start();
    }
}
