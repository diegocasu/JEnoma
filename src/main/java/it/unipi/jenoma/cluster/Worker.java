package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.algorithm.ClusterStatistics;
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
    private List<ClusterStatistics> clusterStatisticsList;
    private ClusterStatistics lastClusterStatistic;

    private boolean startJavaNode() {
        try {
            javaNode = new OtpNode(ClusterUtils.compose(ClusterUtils.Node.JAVA, host));
        } catch (IOException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            return false;
        }

        javaNode.setCookie(ClusterUtils.Cluster.COOKIE);
        workerLogger.log("Set cookie");
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

    private void stopAllNodes() {
        stopWorkerLogger();
        stopJavaNode();
    }

    private void sendToErlangNode(OtpErlangObject msg) {
        mailBox.send(
                ClusterUtils.Process.WORKER,
                ClusterUtils.compose(ClusterUtils.Node.ERLANG, host),
                msg);
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
        int chunkSize = population.getSize()/numberOfThreads;
        int threadsToSpawn = chunkSize > 0 ? numberOfThreads : 1;
        List<Callable<Void>> evaluationTasks = new ArrayList<>();

        for (int i = 0; i < threadsToSpawn; i++) {
            int startChunkIndex = i*chunkSize;
            int endChunkIndex = (i == threadsToSpawn - 1) ? population.getSize() : (i + 1)*chunkSize;

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
        return population != null && population.getSize() != 0;
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
                        int firstParent = prng.nextInt(matingPool.getSize());
                        int secondParent = prng.nextInt(matingPool.getSize());

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

                if (offspring.getSize() == offspringSize)
                    return offspring;

                if (offspring.getSize() > offspringSize) {
                    offspring.removeIndividuals(offspringSize, offspring.getSize());
                    return offspring;
                }
            } catch (InterruptedException | ExecutionException e) {
                workerLogger.log(ExceptionUtils.getStackTrace(e));
                return null;
            }
        }
    }

    private boolean mutate(Population offspring, Mutation mutation, List<PRNG> threadGenerators) {
        int chunkSize = offspring.getSize()/numberOfThreads;
        int threadsToSpawn = chunkSize > 0 ? numberOfThreads : 1;
        List<Callable<Void>> mutationTasks = new ArrayList<>();

        for (int i = 0; i < threadsToSpawn; i++) {
            int startChunkIndex = i*chunkSize;
            int endChunkIndex = (i == threadsToSpawn - 1) ? offspring.getSize() : (i + 1)*chunkSize;
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

        sendToErlangNode(
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
                stopAllNodes();
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
            stopAllNodes();
            System.exit(0);
        }
    }

    private void applyElitism(Population original, Population offspring, Elitism elitism) {
        int numberOfCandidates = Math.min(elitism.getNumberOfIndividuals(), original.getSize());
        original.sortByDescendingFitness();
        offspring.sortByAscendingFitness();

        List<Individual> elite = original.getIndividuals(0, numberOfCandidates);
        List<Individual> worst = offspring.getIndividuals(0, numberOfCandidates);
        lastClusterStatistic.fittestIndividual = elite.get(0);

        lastClusterStatistic.elitismTime = System.currentTimeMillis() - lastClusterStatistic.elitismTime;
        lastClusterStatistic.computationTime = Math.abs(lastClusterStatistic.computationTime - System.currentTimeMillis());
        lastClusterStatistic.communicationTime = System.currentTimeMillis();

        sendIndividualsForElitism(elite, worst);
        receiveIndividualsForElitism(offspring);
    }

    private boolean endAlgorithm(Population offspring, int generationsElapsed, TerminationCondition<?> termination) {
        lastClusterStatistic.communicationTime = Math.abs(lastClusterStatistic.communicationTime - System.currentTimeMillis());

        long terminationConditionTimestamp = System.currentTimeMillis();
        Object terminationMap = termination.map(offspring, generationsElapsed, workerLogger);
        lastClusterStatistic.computationTime += System.currentTimeMillis() - terminationConditionTimestamp;

        long communicationTimestamp = System.currentTimeMillis();

        sendToErlangNode(
                new OtpErlangTuple(
                        new OtpErlangObject[] {
                                ClusterUtils.Atom.GENERATION_END_PHASE,
                                new OtpErlangBinary(terminationMap)
                        }));
        workerLogger.log("Sent GENERATION_END_PHASE message to erlang node");

        try {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("The termination condition check failed.");
                workerLogger.log("Stopping the execution.");
                stopAllNodes();
                System.exit(0);
            }

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.ALGORITHM_CONTINUE))
                return false;

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.ALGORITHM_END)) {
                workerLogger.log("Sending the population chunk and " + this.clusterStatisticsList.size() + " statistics.");
                OtpErlangBinary[] otpErlangObjects = new OtpErlangBinary[this.clusterStatisticsList.size()];

                for(int j = 0; j < this.clusterStatisticsList.size(); j++){
                    otpErlangObjects[j] = new OtpErlangBinary(this.clusterStatisticsList.get(j));
                }

                sendToErlangNode(
                        new OtpErlangTuple(
                                new OtpErlangObject[] {
                                        ClusterUtils.Atom.RESULT_COLLECTION_PHASE,
                                        new OtpErlangBinary(offspring) ,
                                        new OtpErlangList(otpErlangObjects)
                                }));

                workerLogger.log("Sent RESULT_COLLECTION_PHASE message to erlang node");
                return true;
            }
        } catch (OtpErlangExit | OtpErlangDecodeException | RuntimeException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopAllNodes();
            System.exit(0);
        }

        lastClusterStatistic.communicationTime = System.currentTimeMillis() - communicationTimestamp;
        return true;
    }

    private Population shuffle(Population population) {
        OtpErlangBinary[] individuals = new OtpErlangBinary[population.getSize()];
        for (int i = 0; i < population.getSize(); i++)
            individuals[i] = new OtpErlangBinary(population.getIndividual(i));

        sendToErlangNode(
                new OtpErlangTuple(
                        new OtpErlangObject[] {
                                ClusterUtils.Atom.SHUFFLE_PHASE,
                                new OtpErlangList(individuals)
                        }));
        workerLogger.log("Sent SHUFFLE_PHASE message to Erlang node.");
        List<Individual> shuffledIndividuals = new ArrayList<>();

        try {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("The shuffle stage failed.");
                workerLogger.log("Stopping the execution.");
                stopAllNodes();
                System.exit(0);
            }

            if (msg instanceof OtpErlangList msgList) {
                for (OtpErlangObject individual : msgList)
                    shuffledIndividuals.add((Individual) ((OtpErlangBinary) individual).getObject());
            }
        } catch (OtpErlangDecodeException | OtpErlangExit | RuntimeException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopAllNodes();
            System.exit(0);
        }

        return new Population(shuffledIndividuals);
    }

    private void executeAlgorithm(GeneticAlgorithm geneticAlgorithm) {
        Pair<PRNG, List<PRNG>> prngs = createPRNGs(geneticAlgorithm.getConfiguration().getSeed());
        PRNG mainGenerator = prngs.getLeft();
        List<PRNG> threadGenerators = prngs.getRight();

        while (true) { // Algorithm loop.
            Population workingPopulation = geneticAlgorithm.getPopulation().clone();

            lastClusterStatistic = new ClusterStatistics();
            lastClusterStatistic.workerID = mailBox.getName();
            lastClusterStatistic.computationTime = System.currentTimeMillis();

            lastClusterStatistic.selectionTime = System.currentTimeMillis();
            Population matingPool = geneticAlgorithm.getSelection().select(workingPopulation, mainGenerator, workerLogger);
            lastClusterStatistic.selectionTime = System.currentTimeMillis() - lastClusterStatistic.selectionTime ;

            if (matingPool == null || matingPool.getSize() < 2) {
                workerLogger.log("The selection stage failed. Less than two individuals selected.");
                return;
            }
            workerLogger.log("The selection stage succeeded.");
            lastClusterStatistic.crossoverTime = System.currentTimeMillis();
            Population offspring = cross(matingPool, geneticAlgorithm.getPopulation().getSize(), geneticAlgorithm.getCrossover(), threadGenerators);
            lastClusterStatistic.crossoverTime = System.currentTimeMillis() - lastClusterStatistic.crossoverTime;

            if (offspring == null) {
                workerLogger.log("The crossover stage failed.");
                return;
            }
            workerLogger.log("The crossover stage succeeded.");
            lastClusterStatistic.mutationTime = System.currentTimeMillis();
            boolean mutationPerformedSuccessfully = mutate(offspring, geneticAlgorithm.getMutation(), threadGenerators);
            lastClusterStatistic.mutationTime = System.currentTimeMillis() - lastClusterStatistic.mutationTime;

            if (!mutationPerformedSuccessfully) {
                workerLogger.log("The mutation stage failed.");
                return;
            }

            workerLogger.log("The mutation stage succeeded.");

            lastClusterStatistic.evaluationTime = System.currentTimeMillis();
            boolean evaluationStagePerformedSuccessfully = evaluate(offspring, geneticAlgorithm.getEvaluation());
            lastClusterStatistic.evaluationTime = System.currentTimeMillis() - lastClusterStatistic.evaluationTime;

            if (!evaluationStagePerformedSuccessfully) {
                workerLogger.log("The evaluation stage failed.");
                return;
            }
            workerLogger.log("The evaluation stage succeeded.");


            if (geneticAlgorithm.getElitism().getNumberOfIndividuals() > 0) {
                lastClusterStatistic.elitismTime = System.currentTimeMillis();
                applyElitism(geneticAlgorithm.getPopulation(), offspring, geneticAlgorithm.getElitism());
                workerLogger.log("The elitism stage succeeded.");
            } else
                workerLogger.log("Elitism stage skipped.");

            geneticAlgorithm.incrementGenerationsElapsed();
            lastClusterStatistic.currentGeneration = geneticAlgorithm.getGenerationsElapsed();
            workerLogger.log(String.format("Reached generation %s.", geneticAlgorithm.getGenerationsElapsed()));

            if (endAlgorithm(offspring, geneticAlgorithm.getGenerationsElapsed(), geneticAlgorithm.getTerminationCondition()))
                return;
            workerLogger.log("The shuffling stage is starting.");

            long communicationTimestamp = System.currentTimeMillis();
            Population shuffledPopulation = shuffle(offspring);
            workerLogger.log("The shuffling stage succeeded.");
            geneticAlgorithm.setPopulation(shuffledPopulation);

            lastClusterStatistic.communicationTime += System.currentTimeMillis() - communicationTimestamp;
            this.clusterStatisticsList.add(lastClusterStatistic);
        }
    }

    private void receiveLoop() throws OtpErlangDecodeException, OtpErlangExit {
        boolean waitForStop = false;
        workerLogger.log("Waiting for Workload");

        while (true) {
            OtpErlangObject msg = mailBox.receive();
            workerLogger.log("Stopping the execution.");

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("Stopping the execution.");
                stopAllNodes();
                return;
            }

            if (msg instanceof OtpErlangBinary msgBinary && !waitForStop) {
                GeneticAlgorithm geneticAlgorithm = (GeneticAlgorithm) msgBinary.getObject();
                workerLogger.log("Received workload.");

                // Start algorithm, evaluating the initial population.
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

    public Worker(String host, String loggerCoordinatorHost) {
        this.host = host;
        this.loggerCoordinatorHost = loggerCoordinatorHost;
        this.numberOfThreads = Runtime.getRuntime().availableProcessors();
        this.executorService = Executors.newFixedThreadPool(numberOfThreads);
        this.clusterStatisticsList = new ArrayList<>();
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
        sendToErlangNode(ClusterUtils.Atom.HEARTBEAT);

        try {
            receiveLoop();
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopAllNodes();
        }
    }

    public static void main(String[] args) {
        String thisHost = args[0];
        String loggerCoordinatorHost = args[1];

        Worker worker = new Worker(thisHost, loggerCoordinatorHost);
        worker.start();
    }
}
